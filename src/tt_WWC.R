library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(patchwork)


# define functions --------------------------------------------------------
# Build the x, y coordinates to plot the results
.build.poly <- function(
  md,
  gf,
  ga,
  width = 0.35,
  angle = 14){
  
  
  offset = (gf + ga) * sin(angle)
  offset = offset/10
  
  if(ga < gf){ # if a win, (md, md +offset, md + offset + width, md + width)
    dt <- data.table(
      y = c(-ga,
            gf,
            gf,
            -ga),
      x = c(md,
            md + offset,
            md + offset + width,
            md + width),
      md = rep(as.character(md), 4))
  } 
  
  
  if(ga > gf){ # if a loss, (md, md - offset, md - offset + width, md + width)
    dt <- data.table(
      y = c(-ga,
            gf,
            gf,
            -ga),
      x = c(md + offset,
            md ,
            md + width,
            md + offset + width),
      md = rep(as.character(md), 4))
  }
  
  if(ga == gf){ # if a draw, no offset
    offset <- 0
    dt <- data.table(
      y = c(-ga,
            gf,
            gf,
            -ga),
      x = c(md,
            md,
            md + width,
            md + width),
      md = rep(as.character(md), 4))
  }
  
  return(dt)
}

# collect all results in the required format
.collect.results <- function(country, dt){
  zz <- rbind(
    dt[home == country, .(year, yearly_game_id, gf = home_score, ga = away_score)], 
    dt[away == country, .(year, yearly_game_id, gf = away_score, ga = home_score)])
  setkey(zz, year, yearly_game_id)
  zz[, id := c(1:nrow(zz))]
  zz[, nation := country ]
  return(zz)
}

# Place plot construction in a function, use patchwork to arrange multiple 
# ggplot objects created with this function
build.plot <- function(team = 'USA', df, dt, wc.index, dt.champions){
  p <- ggplot(df[nation == team]) + 
    # add timeline 
    geom_hline(yintercept = 0, size = .5) +
    # add wc delimiter lines
    geom_vline(xintercept = wc.index$md, linetype = 3, color = 'grey70', size = .25) +
    # Draw bars
    geom_polygon(
      aes(
        fill = outcome,
        group = md,
        x = x,
        y = y),
      color = 'black',
      size = .5) +
    # draw 'no goal' points
    geom_point(
      data = dt[nation == team],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'black',
      shape = 21,
      size = 1,
      stroke = 1) +
    # draw 'no score draw' points
    geom_point(
      data = dt[score == FALSE & nation == team],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'white',
      shape = 21,
      size = 2,
      stroke = 1) +
    # draw markers for champions up
    geom_point(
      data = dt.champions[nation == team],
      inherit.aes = FALSE,
      aes(x = x, y = gf + .5),
      fill = '#E9C63A',
      shape = 25,
      size = 1.5,
      stroke = .5) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = as.numeric(
        df[, c(min(md), max(id) + 1)])) + 
    scale_y_continuous(
      expand = c(0, 0),
      limits = as.numeric(
        df[nation == team, c(min(y), max(y))])) + 
    coord_equal(clip = 'off') + 
    guides(color = FALSE,
           fill = FALSE) +
    theme_void() +
    theme(panel.spacing.y = unit(0, "lines"),
          plot.margin = margin(10, 30, 10, 30),
          plot.background = element_rect(fill = '#f5f9ff', color = NA),
          panel.border = element_blank(),
          plot.title = element_text(family = 'Quicksand'),
          plot.caption = element_text(
            family = 'Quicksand',
            color = 'grey30',
            size = 7))
  
  return(p)
}

# Build a small version of elements from the main plot to act as a guide 
# presenting how the plot is to be interpreted
build.guide <- function(df.guide, dt.guide, dt.guide.champions){
  p <- ggplot(df.guide) + 
    # add timeline 
    geom_hline(yintercept = 0, size = .5) +
    # Draw bars
    geom_polygon(
      aes(
        fill = outcome,
        group = md,
        x = x,
        y = y),
      color = 'black',
      size = .5) +
    # draw 'no goal' points
    geom_point(
      data = dt.guide,
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'black',
      shape = 21,
      size = 1,
      stroke = 1) +
    # draw 'no score draw' points
    geom_point(
      data = dt.guide[score == FALSE],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'white',
      shape = 21,
      size = 2,
      stroke = 1) +
    # draw markers for champions up
    geom_point(
      data = dt.guide.champions,
      inherit.aes = FALSE,
      aes(x = x, y = gf + .5),
      fill = '#E9C63A',
      shape = 25,
      size = 1.5,
      stroke = .5) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = as.numeric(
        df.guide[, c(min(md), max(id) + 1)])) + 
    scale_y_continuous(
      expand = c(0, 0),
      limits = as.numeric(
        df.guide[, c(min(y), max(y) + 1)])) + 
    coord_equal(clip = 'off') + 
    guides(color = FALSE,
           fill = FALSE) +
    theme_void() +
    theme(panel.spacing.y = unit(0, "lines"),
          plot.margin = margin(10, 30, 10, 30),
          plot.background = element_rect(fill = '#f5f9ff', color = 'grey50'),
          panel.border = element_blank(),
          plot.title = element_text(family = 'Quicksand'),
          plot.caption = element_text(
            family = 'Quicksand',
            color = 'grey30',
            size = 7))
  
  return(p)
}

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2019-07-09'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$wwc_outcomes)
x <- x[round != 'Third Place Playoff']
# split into home/away
x.home <- x[team_num == 1, .(
  year,
  yearly_game_id,
  home = team,
  home_score = score,
  team_num)]
x.away <- x[team_num == 2, .(
  year,
  yearly_game_id,
  away = team,
  away_score = score,
  team_num)]

# one row per game 
z <- 
  merge(
    x.home,
    x.away, 
    by = c(
      'yearly_game_id',
      'year')
  )[, .(
    year,
    yearly_game_id,
    home, 
    home_score,
    away,
    away_score)]

# only keep games involving ever world champions
z <-
  z[home %in% c('USA', 'GER', 'NOR', 'JPN') | away %in% c('USA', 'GER', 'NOR', 'JPN')]

# Get goals for/against for each country, for each game
zz <- rbindlist(
  lapply(
    X = c('USA', 'GER', 'NOR', 'JPN'),
    FUN = .collect.results,
    dt = z)
)


# bar width
width = .35

# Account for non contiguous appearances at a WWC
# Used to add empty space on plot where a team did not play
# 6 games in 91
# 6 in 91, 95, 99, 03, 08, 11
# 7 in 15, 7 in 19
# 50 possible apps
all.apps <- data.table(
  year = c(
    rep(1991, 6),
    rep(1995, 6),
    rep(1999, 6),
    rep(2003, 6),
    rep(2007, 6),
    rep(2011, 6),
    rep(2015, 7),
    rep(2019, 7)),
  y.id = c(
    c(1:6),
    c(1:6),
    c(1:6),
    c(1:6),
    c(1:6),
    c(1:6),
    c(1:7),
    c(1:7)),
  id = 1:50
)

# add space for unplayed games (i.e. when a team had already been eliminated)
zz[, y.id := 1:.N, by = .(year, nation)]
zz <- 
  na.omit( # delete rows with NAs induced by the merge
    merge(
      zz[, .(year, nation, y.id, gf, ga)],
      all.apps,
      by = c('year','y.id'),
      all.y = TRUE)
)[, .(year, nation, gf, ga, id, y.id)]

zz[gf == ga, outcome := 'draw']
zz[gf > ga, outcome := 'win']
zz[gf < ga, outcome := 'loss']


# Convert the results to polygons
df <- zz[, .build.poly(md = id, gf = gf, ga = ga, width = width), by = .(id, nation)]
df <- merge(
  df,
  zz[, .(md = as.character(id), nation, outcome)],
  by = c('nation', 'md'))

# TODO: put this in a function in the style of build.poly
# Convert no goal games to points
# first get unbalanced goalless teams
dt <- 
  zz[gf == 0 | ga == 0,
     .(nation, gf, ga, x = id + (width/2), y = 1)][
       ga == 0 & gf != 0, y := -y][, score := TRUE]
# then deal with goalless draws
dt <- rbind(
  dt,
  dt[gf == 0 & ga == 0, .(nation, gf, ga, x, y= -y, score = TRUE)], 
  dt[gf == 0 & ga == 0, .(nation, gf, ga, x, y= 0, score = FALSE)]
)

# Build a data.table identifying the victors/runners up for each WC
# this will be used to plot points showing the victorious team for each WC
dt.champions <- 
  zz[(!year %in% c(2015, 2019) & y.id == 6) |  # 6 games prior to 2015
     (year %in% c(2015, 2019) & y.id == 7)]    # 7 possible games after 2015

dt.champions <- dt.champions[outcome %in% c('win', 'draw')]
dt.champions <- dt.champions[outcome == 'win' | 
                               # USA/JPN won on penalties in 1999 and 2011
                               (nation == 'USA' & year == 1999) |
                               (nation == 'JPN' & year == 2011)]
# Add an offset to the plot point, to account for the polygon offset
dt.champions <- merge(
  dt.champions[,.(nation, id, gf, ga)],
  # merge on the second polygon point x-value + (width *.5)
  df[ , .SD[2, .(x = x + (width/2))], by = .(nation, id)],
  by = c('nation', 'id'))
# if final finished 0-0, make the correct height adjustment to plot onto the
# geom used to represent 0-0
dt.champions[gf == 0 & ga == 0, gf := 1.25]

# coordinates for the start of each WWC
# used to plot labels and delimiting lines
wc.index <- all.apps[, .(md = min(id)), by = year]




# Build plot --------------------------------------------------------------

# Define some national team colours, based on the team badge
pal.usa <- c(
  'draw' = '#1f2742',
  'win' = 'white',
  'loss' = '#bb2533'
)

pal.jpn <- c(
  'win' = 'black',
  'draw' = 'white',
  'loss' = '#e30016'
)

pal.ger <- c(
  'win' = '#00a768',
  'draw' = '#7a7878',
  'loss' = '#d80f18'
)

pal.nor <- c(
  'win' = '#00296b',
  'draw' = 'white',
  'loss' = '#d3162f'
)


# Text labels marking each world cup year - add to the top patchwork plot
df.wc.labels <- data.frame(
  x = c(wc.index$md),
  # place at lowest point on the USA plot
  y = rep( 
    df[nation == 'USA', min(y)],
    8),
  # Build label of location + year
  label = paste0(
    c('China ', 'Sweden ', 'USA ', 'USA ',
      'China ', 'Germany ', 'Canada ', 'France ' ),
    wc.index$year)
)


  
# build the individual panels
p.text <- ggplot() +
  geom_textbox(
    data = data.frame(
      x = 0.5,
      y = 0.5,
      label = "Performance Of The Four FIFA Womens World Cup Champions"),
    aes(x, y, label = label),
    color = 'Black',
    box.color = NA,
    fill = NA,
    family = 'Quicksand',
    size = 5,
    width = grid::unit(0.9, "npc"), 
    hjust = 0.5, vjust = 0.5, halign = 0) +
  scale_y_continuous(limits = c(0.48, 0.51)) + 
  theme_void() + 
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = '#f5f9ff', color = NA)) 

p.usa <- build.plot('USA', df, dt, wc.index, dt.champions) + 
  scale_fill_manual(values = pal.usa) + 
  geom_richtext(data = df.wc.labels, # Add wc year annotations
                mapping = aes(x = x, y = y, label = label),
                color = 'grey10',
                family = 'Quicksand',
                size = 2,
                fill = NA,
                label.color = NA,
                hjust = 0,
                vjust = 0.5) +
  #theme(plot.margin = margin(75, 30, 10, 30)) + 
  labs(title = 'USA')
  
p.jpn <- build.plot('JPN', df, dt, wc.index, dt.champions) + 
  scale_fill_manual(values = pal.jpn) +
  labs(title = 'Japan')

p.ger <- build.plot('GER', df, dt, wc.index, dt.champions) + 
  scale_fill_manual(values = pal.ger) +
  labs(title = 'Germany')

p.nor <- build.plot('NOR', df, dt, wc.index, dt.champions) + 
  scale_fill_manual(values = pal.nor) +
  labs(title = 'Norway')









# Build a legend
  # win, loss, draw, goaless draw, champion
zz.guide <- data.table(id = c(1, 3, 5, 7, 9),
                       gf = c(2, 1, 1, 0, 2),
                       ga = c(1, 2, 1, 0, 1),
                       outcome = c('win', 'loss', 'draw', 'draw', 'win'))

df.guide <- zz.guide[,
               .build.poly(md = id, gf = gf, ga = ga, width = width), by = .(id)]
df.guide <- merge(
  df.guide,
  zz.guide[, .(md = as.character(id), outcome)],
  by = c('md'))

dt.guide <- 
  zz.guide[gf == 0 | ga == 0,
     .(gf, ga, x = id + (width/2), y = 1)][
       ga == 0 & gf != 0, y := -y][, score := TRUE]
# then deal with goalless draws
dt.guide <- rbind(
  dt.guide,
  dt.guide[gf == 0 & ga == 0, .(gf, ga, x, y= -y, score = TRUE)], 
  dt.guide[gf == 0 & ga == 0, .(gf, ga, x, y= 0, score = FALSE)]
)


dt.guide.champions <- data.table(id = 9, gf = 2, ga = 1)
dt.guide.champions <- merge(
  dt.guide.champions,
  # merge on the second polygon point x-value + (width *.5)
  df.guide[ , .SD[2, .(x = x + (width/2))], by = .(id)],
  by = 'id')

p.guide <- build.guide(df.guide, dt.guide, dt.guide.champions) +
  scale_fill_manual(values = pal.usa) + 
  theme_void() + 
  scale_x_continuous(limits = c(0, 10.5)) + 
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  theme(
    plot.background = element_rect(
      fill = '#f5f9ff',
      color = 'black')) + 
  geom_richtext(
    data = data.table(
      x = zz.guide$id,
      y = c(-1.5, 1.5, -2, 2, -1.5),
      label = c(
        'win',
        'loss',
        'score<br>draw',
        '0-0<br>draw',
        'champions')), # Add  annotations
    mapping = aes(x = x, y = y, label = label),
    color = 'grey10',
    family = 'Quicksand',
    size = 2,
    fill = NA,
    label.color = NA,
    hjust = 0.5,
    vjust = 0.5) +
  # Add vertical GF/GA labels
  geom_richtext(
    data = data.table(
      x = c(0.5, 0.5),
      y = c(-1.5, 1.5),
      label = c(
        'GA',
        'GF')), 
    mapping = aes(x = x, y = y, label = label),
    angle = 270,
    color = 'grey10',
    family = 'Quicksand',
    size = 2,
    fill = NA,
    label.color = NA,
    hjust = 0.5,
    vjust = 1)
  


p.text <- ggplot() +
  geom_textbox(
    data = data.frame(
      x = 0.5,
      y = 0.5,
      label = "Performance Of The Four FIFA Womens World Cup Champions"),
    aes(x, y, label = label),
    color = 'Black',
    box.color = NA,
    fill = NA,
    family = 'Quicksand',
    size = 5,
    width = grid::unit(0.9, "npc"), 
    hjust = 0.5, vjust = 0.5, halign = 0) +
  scale_y_continuous(limits = c(0.48, 0.51)) + 
  theme_void() + 
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = '#f5f9ff', color = NA)) 

p.text <- 
  p.text + inset_element(p.guide, left = 0.7, bottom = 0, right = 1, top = 0.55)

# arrange and save
p.text / (p.usa / p.ger / p.nor / p.jpn) + 
  plot_layout(
    heights = c(0.15, 0.85),
    widths = c(1)) + 
  plot_annotation(
    caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com)\ninspired by https://sn.ethz.ch/research/soccerbars.html",
    theme = theme(
      plot.margin = margin(5, 0, 10, 0),
      plot.caption = element_text(
        family = 'Quicksand',
        size = 8,
        colour = 'grey10'),
      plot.background = element_rect(
        fill = '#f5f9ff',
        color = NA))) + 
  ggsave(
    filename = paste0(
      getwd(),
      '/plots/',
      tidy.week,
      '.png'),
    width = 8, height = 10, device = 'png')








