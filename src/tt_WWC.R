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
build.plot <- function(team = 'USA', dt.polygons, dt.points, wc.index, dt.champions){
  p <- ggplot(dt.polygons[nation == team]) + 
    # add timeline 
    geom_hline(yintercept = 0, size = .5) +
    # add wc delimiter lines
    geom_vline(xintercept = wc.index$md, linetype = 3, color = 'grey30', size = .25) +
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
      data = dt.points[nation == team],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'black',
      shape = 21,
      size = 1,
      stroke = 1) +
    # draw 'no score draw' points
    geom_point(
      data = dt.points[score == FALSE & nation == team],
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
        dt.polygons[, c(min(md), max(id) + 1)])) + 
    scale_y_continuous(
      expand = c(0, 0),
      limits = as.numeric(
        dt.polygons[nation == team, c(min(y), max(y))])) + 
    coord_equal(clip = 'off') + 
    guides(color = FALSE,
           fill = FALSE) +
    theme_void() +
    theme(panel.spacing.y = unit(0, "lines"),
          plot.margin = margin(10, 10, 10, 10),
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
build.guide <- function(dt.polygons, dt.points, dt.champions){
  p <- ggplot(dt.polygons) + 
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
      data = dt.points,
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'black',
      shape = 21,
      size = 1,
      stroke = 1) +
    # draw 'no score draw' points
    geom_point(
      data = dt.points[score == FALSE],
      inherit.aes = FALSE,
      aes(x = x, y = y),
      fill = 'white',
      shape = 21,
      size = 2,
      stroke = 1) +
    # draw markers for champions up
    geom_point(
      data = dt.champions,
      inherit.aes = FALSE,
      aes(x = x, y = gf + .5),
      fill = '#E9C63A',
      shape = 25,
      size = 1.5,
      stroke = .5) +
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



# Define variables for plotting and polygon creation ----------------------
# bar width
width = .35

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

pal.guide <- c( # a palette for the guide plot
  'win' = 'white',
  'draw' = 'white',
  'loss' = 'white'
)

# Define a vector detailing the total number of possible games played at each WC
# Used to add empty space on plot where a team did not play
# 6 in 91, 95, 99, 03, 08, 11
# 7 in 15, 19
# 50 total possible apps
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

# coordinates for the start of each WWC
# used to plot labels and delimiting lines
wc.index <- all.apps[, .(md = min(id)), by = year]

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2019-07-09'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$wwc_outcomes)
x <- x[round != 'Third Place Playoff'] # remove third place playoffs

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
fixture.results <- 
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
fixture.results <- fixture.results[home %in% c('USA', 'GER', 'NOR', 'JPN') |
       away %in% c('USA', 'GER', 'NOR', 'JPN')]

# Get goals for/against for each country, for each game
nation.results <- rbindlist(
  lapply(
    X = c('USA', 'GER', 'NOR', 'JPN'),
    FUN = .collect.results,
    dt = fixture.results))


# add space for unplayed games (i.e. when a team had already been eliminated)
nation.results[, y.id := 1:.N, by = .(year, nation)]
nation.results <- 
  na.omit( # delete rows with NAs induced by the merge
    merge(
      nation.results[, .(year, nation, y.id, gf, ga)],
      all.apps,
      by = c('year','y.id'),
      all.y = TRUE)
)[, .(year, nation, gf, ga, id, y.id)]

nation.results[gf == ga, outcome := 'draw']
nation.results[gf > ga, outcome := 'win']
nation.results[gf < ga, outcome := 'loss']


# Build the polygons and point coordinates for each match -----------------
# Convert the results to polygons
dt.polygons <- nation.results[, .build.poly(md = id, gf = gf, ga = ga, width = width), by = .(id, nation)]
dt.polygons <- merge(
  dt.polygons,
  nation.results[, .(md = as.character(id), nation, outcome)],
  by = c('nation', 'md'))

# TODO: put this in a function in the style of build.poly
# Convert no goal games to points
# first get unbalanced goalless teams
dt.points <- 
  nation.results[gf == 0 | ga == 0,
     .(nation, gf, ga, x = id + (width/2), y = 1)][
       ga == 0 & gf != 0, y := -y][, score := TRUE]
# then deal with goalless draws
dt.points <- rbind(
  dt.points,
  dt.points[gf == 0 & ga == 0, .(nation, gf, ga, x, y= -y, score = TRUE)], 
  dt.points[gf == 0 & ga == 0, .(nation, gf, ga, x, y= 0, score = FALSE)])

# Build a data.table identifying the victors/runners up for each WC
# this will be used to plot points showing the victorious team for each WC
dt.champions <- 
  nation.results[(!year %in% c(2015, 2019) & y.id == 6) |  # 6 games prior to 2015
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
  dt.polygons[ , .SD[2, .(x = x + (width/2))], by = .(nation, id)],
  by = c('nation', 'id'))
# if final finished 0-0, make the correct height adjustment to plot onto the
# geom used to represent 0-0
dt.champions[gf == 0 & ga == 0, gf := 1.25]


# Build the polygons and points for the legend/guide ----------------------
nation.results.guide <- data.table(id = c(1, 3, 5, 7, 9),
                                   gf = c(2, 1, 1, 0, 2),
                                   ga = c(1, 2, 1, 0, 1),
                                   outcome = c('win', 'loss', 'draw',
                                               'draw', 'win'))

dt.polygons.guide <- 
  nation.results.guide[,.build.poly(md = id, gf = gf, ga = ga, width = width), 
                       by = .(id)]
dt.polygons.guide <- merge(
  dt.polygons.guide,
  nation.results.guide[, .(md = as.character(id), outcome)],
  by = c('md'))

dt.points.guide <- 
  nation.results.guide[gf == 0 | ga == 0,
                       .(gf, ga, x = id + (width/2), y = 1)][
                         ga == 0 & gf != 0, y := -y][, score := TRUE]
# then deal with goalless draws
dt.points.guide <- rbind(
  dt.points.guide,
  dt.points.guide[gf == 0 & ga == 0, .(gf, ga, x, y= -y, score = TRUE)], 
  dt.points.guide[gf == 0 & ga == 0, .(gf, ga, x, y= 0, score = FALSE)])


dt.guide.champions <- data.table(id = 9, gf = 2, ga = 1)
dt.guide.champions <- merge(
  dt.guide.champions,
  # merge on the second polygon point x-value + (width *.5)
  dt.polygons.guide[ , .SD[2, .(x = x + (width/2))], by = .(id)],
  by = 'id')





# Build plot --------------------------------------------------------------


# Text labels marking each world cup year - add to the top patchwork plot
dt.wc.labels <- data.table(
  x = c(wc.index$md),
  # place at lowest point on the USA plot
  y = rep( 
    dt.polygons[nation == 'USA', min(y)], 8),
  # Build label of location + year
  label = paste0(
    c('China ', 'Sweden ', 'USA ', 'USA ',
      'China ', 'Germany ', 'Canada ', 'France ' ),
    wc.index$year))


  
# build the individual panels
# Construct the results for each team of interest
p.usa <- build.plot(
  'USA',
  dt.polygons,
  dt.points,
  wc.index,
  dt.champions) + 
  scale_fill_manual(values = pal.usa) + 
  geom_richtext(data = dt.wc.labels, # Add wc year annotations
                mapping = aes(x = x, y = y, label = label),
                color = 'grey10',
                family = 'Quicksand',
                size = 1.75,
                fill = NA,
                label.color = NA,
                hjust = 0,
                vjust = 0.5) +
  theme(plot.margin = margin(0, 10, 10, 10)) + 
  labs(title = 'USA')
  
p.jpn <- build.plot('JPN',
                    dt.polygons,
                    dt.points,
                    wc.index,
                    dt.champions) + 
  scale_fill_manual(values = pal.jpn) +
  labs(title = 'Japan')

p.ger <- build.plot('GER', 
                    dt.polygons,
                    dt.points,
                    wc.index,
                    dt.champions) + 
  scale_fill_manual(values = pal.ger) +
  labs(title = 'Germany')

p.nor <- build.plot('NOR',
                    dt.polygons,
                    dt.points,
                    wc.index,
                    dt.champions) + 
  scale_fill_manual(values = pal.nor) +
  labs(title = 'Norway')


p.guide <- build.guide(
  dt.polygons.guide,
  dt.points.guide,
  dt.guide.champions) +
  scale_fill_manual(values = pal.guide) + 
  theme_void() + 
  scale_x_continuous(limits = c(0, 10.5)) + 
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  theme(
    plot.background = element_rect(
      fill = '#f5f9ff',
      color = NA)) + 
  geom_richtext(
    data = data.table(
      # Account for offset
      x = dt.polygons.guide[ , .SD[2, .(x = x + (width/2))], by = .(id)][, x],
      y = c(2.5, 1.5, 2, 2, -2),
      label = c(
        'win',
        'loss',
        'score<br>draw',
        '0-0<br>draw',
        'World Cup<br>champions')), # Add  annotations
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
  

# Construct the title as a separate plot
p.text <- ggplot() +
  geom_textbox(
    data = data.frame(
      x = 0,
      y = 0.5,
      label = "<b>Performance Of The Four FIFA Women's World Cup Winning Nations At Each Tournament</b>"),
    aes(x, y, label = label),
    color = 'Black',
    box.color = NA,
    fill = NA,
    family = 'Quicksand',
    size = 5,
    width = grid::unit(0.75, "npc"), 
    hjust = 0.5, vjust = 0.5, halign = 0) +
  scale_y_continuous(limits = c(0.48, 0.51)) + 
  theme_void() + 
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = '#f5f9ff', color = NA)) 

# Add the legend to the title plot as an inset
p.text <- p.text + 
  inset_element(
    p.guide,
    left = 0.6,
    bottom = 0, 
    right = 0.9,
    top = 0.55)

# arrange all plots and save
p.text / (p.usa / p.ger / p.nor / p.jpn) + 
  plot_layout(
    heights = c(0.15, 0.85),
    widths = c(1)) + 
  # Add a caption, exploits plot_annotation() to color the background for coord_equal
  plot_annotation(
    caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com)\nInspired by https://sn.ethz.ch/research/soccerbars.html",
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








