library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(viridis)



# Data Prep ---------------------------------------------------------------

tidy.week <- '2021-06-01'

summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

x <- data.table(summary)


z <- x[!is.na(viewers_mean), .(season = paste0('Season ', season), viewers_mean)]

# Get the change in viewers
z[, viewers.init := 100]
z[, viewers.perc := (viewers_mean/head(viewers_mean,1)) * 100]

# Long format
z <- melt(z[, .(season,  viewers.init, viewers.perc)],id.vars = 'season')
z[, change := -(100-value)]
z[, change.col := rep(z[variable == 'viewers.perc', change], 2)]

# reorder the levels to plot in the correct order
z[, season := factor(season, levels = paste0('Season ', c(1:length(unique(season)))))]


# Plotting ----------------------------------------------------------------

ggplot(z, aes(x = variable, y = value, group = season)) + 
  geom_line(aes(color = change.col), size = 1.75) +
  geom_point(color = 'black', size = 2.25) +
  facet_wrap(.~season, nrow = 2) +
  scale_y_continuous(
    limits = c(0, 110),
    name = 'Viewership - as a percentage of Season 1 viewership',
    breaks = c(0, 50, 100),
    labels = function(x) paste(x, '%'),
    expand = c(0,0)) + 
  theme_void() + 
  labs(title = 'FALLING VIEWERSHIP OF THE TV SERIES "SURVIVIOR"',
       subtitle = 'There has been a steady decline in the number of people watching "Survivor". These plots show the viewership of each season presented as a percentage of the number of viewers for the first season of the show.',
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\ndata from tidyTuesday and survivoR") + 
  scale_color_viridis(option = 'plasma') +
  guides(color = FALSE) + 
  theme(
    plot.margin = margin(10, 10, 5, 10),
    plot.title = element_text(
      size = 15,
      color = 'black',
      family = 'Inria Sans Bold',
      margin = margin(0, 10, 5, 10)),
    plot.subtitle = element_text(
      size = 10,
      color = 'black',
      family = 'Inria Sans Regular',
      margin = margin(0, 10, 10, 10)),
    plot.caption = element_text(
      size = 7,
      color = 'black',
      family = 'Inria Sans Regular',
      margin = margin(5, 10, 0, 10)),
    text = element_text(
      color = 'black',
      family = 'Inria Sans Regular'),
    strip.text = element_text(
      color = 'black',
      hjust = 0.5,
      size = 11,
      margin = margin(0,0,2,0)),
    strip.background = element_rect(
      fill = '#F4F4F4',
      color = NA),
    axis.text.y  = element_text(
      margin = margin(5,5,5,5)),
    axis.title.y = element_text(
      angle = 90,
      margin = margin(5,0,5,0)),
    panel.grid.major.y = element_line(colour = 'grey70', size = .25),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(2.5, "lines"),
    panel.background = element_rect(
      fill = '#F4F4F4',
      color = 'grey70'),
    plot.background = element_rect(
      fill = '#F4F4F4',
      color = NA)) +
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 20,
    height = 7,
    device = 'png')