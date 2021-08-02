library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(ggflags)
library(countrycode)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-07-27'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$olympics)

# Push Serbia and Montenegro into Serbia [sorry Montenegro]
x[noc == 'SCG', noc := 'SRB']

min.year <- 2000 # only look at medals since Sydney
x <- x[year >= min.year]
# Only look at nations with >= 25 medals
top.teams <- x[!is.na(medal), .N, by = .(noc)][N >= 25, noc] 

# Split conuntries and medals 
competitors <- x[, .(competitors = .N), by = .(noc, season)]
medals <- x[!is.na(medal), .N, by = .(noc, season)]

# Small function to calculate medals per competitor
rateFun <- function(medals, competitors, season = 'summer') {
  new.var = paste0('rate.', season)
  y <- merge(medals,
             competitors,
             by = c('noc'))
  y[, (new.var) := N / competitors]
  keep <- c('noc', new.var)
  return(y[, ..keep])  
}

summer <- rateFun(
  medals[season == 'Summer'], 
  competitors[season == 'Summer'],
  season = 'summer')

winter <- rateFun(
  medals[season == 'Winter'], 
  competitors[season == 'Winter'],
  season = 'winter')

all.seasons <- rateFun(
  medals, 
  competitors,
  season = 'all')

rates <- merge(
 summer, 
 winter, 
 by = 'noc', 
 all.x = TRUE, 
 all.y = TRUE
)

# If a country hasn't won a summer/winter medal - set their rate to 0
rates[is.na(rate.summer), rate.summer := 0]
rates[is.na(rate.winter), rate.winter := 0]

rates <- rates[noc %in% top.teams]

# get the difference in rate between summer and winter 
rates[, rate.diff := rate.summer - rate.winter]
rates[, rate.diff.abs := abs(rate.diff)]
rates[, noc.2 := countrycode(noc, origin = 'ioc', destination = 'iso2c') ]


# Plotting ----------------------------------------------------------------

pal <- c('#DF2935', '#6E8894')

subtitle.text <- 'This visualisation shows the difference in the medals-per-competitor rate at the summer<br>and winter Olympic games for all nations with more than 25 Olympic medals won since the year 2000.<br>Countries in blue have a greater medal-per-competitor rate at the winter games than the summer games.'

subtitle.text <- gsub(
  x = subtitle.text,
  pattern = 'summer',
  replacement = paste0(
    "<span style = 'color:",
    pal[1],
    ";'>**summer**</span>"))

subtitle.text <- gsub(
  x = subtitle.text,
  pattern = 'winter',
  replacement = paste0(
    "<span style = 'color:",
    pal[2],
    ";'>**winter**</span>"))


ggplot(
  rates,
  aes(
    x = reorder(noc,rate.diff.abs),
    y = rate.diff.abs,
    country = tolower(noc.2))) +
  geom_segment(
    inherit.aes = FALSE,
    size = 1.25,
    aes(
      x = reorder(noc,rate.diff.abs),
      xend = reorder(noc,rate.diff.abs),
      y = 0,
      yend = rate.diff.abs, 
      color = rate.diff < 0)) +
  geom_flag() +
  coord_flip() + 
  guides(color = 'none') + 
  theme_minimal() + 
  ylab('absolute difference in summer and winter "medal-per-competitor" rate') + 
  labs(
    title = 'National Medal Rates At The Summer And Winter Olympic Games', 
    subtitle = subtitle.text,
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)<br>Data from TidyTuesday and ft.com & fivethirtyeight.com") + 
  theme(
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(colour = '#F8F0FB', family = 'Lato'),
    plot.subtitle = element_markdown(
      family = 'Lato',
      margin = margin(0,0,20,0)),
    plot.caption = element_markdown(
      family = 'Lato',
      margin = margin(5,5,5,5)),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(), 
    panel.grid.major.x = element_line(
      size = .5,
      color = 'grey50',
      linetype = "dotted"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.grid.minor.y  = element_blank(),
    panel.background = element_rect(fill = '#272838',colour = NA), 
    plot.background = element_rect(fill = '#272838', colour = NA)
  ) + 
  scale_color_manual(values = pal)

ggsave(
  filename = here::here('plots', paste0(tidy.week, '.png')),
  width = 7.5, height = 10.5, device = 'png')



  