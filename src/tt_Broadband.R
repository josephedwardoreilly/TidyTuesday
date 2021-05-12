library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(geofacet)
# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-05-11'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$broadband)
x[, av := as.numeric(`BROADBAND AVAILABILITY PER FCC`)] # convert to numbers
x[, use := as.numeric(`BROADBAND USAGE`)]
x <- x[complete.cases(x)] # drop NAs 

dt <- x[, .(
  a = 0, # need to add some padding to allow the polar plot to wrap around
  b = mean(use), # mean use per state
  c = mean(av)), by = ST] # mean availability per state

# Bring into long format
dt <- melt(
  dt,
  measure.vars = c('a', 'b', 'c'),
  id.vars = 'ST',
  variable.name = 'broadband',
  value.name  = 'perc',
  variable.factor = TRUE)

# Use a continuous scale for the bars
dt[, broadband.int := as.integer(broadband)]

# define a simple palette 
pal = c('a' = 'white', 'b' = '#EF2D56', 'c' = '#2C8C99')

# Define the basic text for title and caption
title.text <- 
  'Disparities In Broadband Access And Use In The United States'

subtitle.text <- 
  'This visualisation presents the proportion of people in each state who use or have access to broadband speed internet'

# Add some colour + formatting to the text 
title.text <- gsub(
  x = title.text,
  pattern = 'Access',
  replacement = paste0(
    "<span style = 'color:",
    pal['c'],
    ";'>**Access**</span>"))

title.text <- gsub(
  x = title.text,
  pattern = 'Use',
  replacement = paste0(
    "<span style = 'color:",
    pal['b'],
    ";'>**Use**</span>"))

subtitle.text <- gsub(
  x = subtitle.text,
  pattern = 'access',
  replacement = paste0(
    "<span style = 'color:",
    pal['c'],
    ";'>**access**</span>"))

subtitle.text <- gsub(
  x = subtitle.text,
  pattern = 'use',
  replacement = paste0(
    "<span style = 'color:",
    pal['b'],
    ";'>**use**</span>"))

ggplot(
  dt,
  aes(x = broadband.int, y = perc, fill = broadband))+
  geom_col(width = .9) +
  scale_y_continuous(limits  = c(0,1))+
  scale_fill_manual(values = pal)+
  coord_polar(theta = 'y') +
  geom_text( #TODO: pass a data.frame to improve the rendering!
    aes(x = 0, y = 0, label = ST),
    size = 5,
    family = 'Oswald',
    color = '#343434') +
  facet_geo( ~ ST) + 
  guides(fill = FALSE) +
  theme_void() +
  labs(
    title = title.text,
    subtitle = subtitle.text,
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)<br>data from tidyTuesday and Microsoft") +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    plot.background = element_rect(fill = '#EFEFEF', color = NA), 
    panel.background = element_rect(fill = '#EFEFEF', color = NA),
    plot.margin = margin(c(20, 40 , 20, 40)),
    plot.title = element_markdown(
      family = 'Oswald',
      size = 20,
      colour = '#343434',
      margin = margin(0,0,0,0)),
    plot.title.position = 'plot',
    plot.subtitle = element_markdown(
      family = 'Oswald',
      size = 15,
      colour = '#343434',
      margin = margin(0,0,20,0)),
    plot.caption = element_markdown(
      family = 'Oswald',
      colour = 'grey50',
      margin = margin(0,0,0,0)),
    plot.caption.position = 'plot') +
  # Save to disk
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 15.5, height = 11, device = 'png')
