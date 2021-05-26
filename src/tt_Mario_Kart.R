library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(patchwork)
library(ggtext)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-05-25'
tt_data <- tt_load(tidy.week) 
    x <- data.table(tt_data$records)

    
z <- x[type == 'Single Lap']

# Get first and last records
setkey(z, track, date)
z <- z[, .SD[c(1,.N)], by=track]
z[, id := c('first', 'last'), by = track]

# Get percentage change 
z <- dcast(z, formula = track ~ id , value.var = c('time'))
z[, diff := first - last]
z[, perc := round((diff/first) * 100, 2)]
z[, perc.orig := 100 - perc]

# order by perc change
z[, track := factor(track, levels = z[order(perc, decreasing = FALSE), track])]





# Plot
ggplot(
  z,
  aes(y = track, x = 100-perc, group = track)) + 
  geom_col(
    inherit.aes = FALSE,
    aes(y = track, x = 100),
    color = NA,
    fill = '#619B8A',
    width = .5)+
  geom_col(width = .5, fill = '#FE7F2D', color = NA) + 
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  geom_text(
    aes(
      label = paste0(round(100-perc, 0),' %'),
      x = 99.9 - perc), 
    size = 3,
    family = 'Apercu Pro',
    color = 'white') + 
  labs(title = 'Improvement In Mario Kart 64 Speedrun Times',
       subtitle = 'Each bar shows the most recent world record for a single lap of a Mario Kart 64 track, expressed as a percentage of the initial world record for that same track.',
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly) - Data from TidyTuesday and github.com/benediktclaus") + 
  theme(
    axis.title = element_blank(),
    text = element_text(
      family = 'Apercu Condensed Pro Medium',
      color = 'white'),
    plot.title = element_text(
      family = 'Apercu Pro',
      size = 12,
      hjust = 1,
      color = 'white'),
    plot.title.position = 'plot',
    plot.subtitle = element_text(
      family = 'Apercu Pro Light',
      size = 6,
      hjust = 1,
      color = 'white'),
    plot.caption  = element_text(
      family = 'Apercu Pro Light',
      size = 5,
      hjust = 1,
      color = 'white'),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      color = 'white',
      size = 6,
      family = 'Apercu Pro'),
    panel.grid = element_blank(),
    plot.background = element_rect(
      fill = '#233D4D',
      color = NA),
    panel.background = element_rect(
      fill = '#233D4D',
      color = NA)) + 
  ggsave(
    filename = paste0(
      getwd(),
      '/plots/',
      tidy.week,
      '.png'),
    width = 7,
    height = 5,
    device = 'png')




