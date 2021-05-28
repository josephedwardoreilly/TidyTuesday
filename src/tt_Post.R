require(data.table)
require(ggplot2)
require(viridis)
require(ggtext)

# Load the data
tidy.week <- '2021-04-13'
tuesdata <- tidytuesdayR::tt_load(tidy.week)


# Data Wrangling/Prep -----------------------------------------------------
x <- data.table(tuesdata$post_offices)

x <- x[, .(name, established, discontinued, state)]
# clean data a bit
x <- x[established > 1400 &
       #!is.na(established) & 
       discontinued < 2001 &
       discontinued > 1400 &
       state == 'NY']

# Number of openings/closures by year
z <- merge(
  x[, .(new = .N), by = .(Y = established)], 
  x[, .(closed = .N), by = .(Y = discontinued)], 
  by = 'Y', 
  all.x = TRUE,
  all.y = TRUE)

z[is.na(new), new := 0]
z[is.na(closed), closed := 0]
z[, net := new - closed]
z <- z[Y > 1799 & Y < 1951]


# Plotting ----------------------------------------------------------------
bg <- 'grey90'
# Main title data
title.d <- data.table(label = 'New York State Post Office Closures',
                      x = 1905, y = 1.25)
# label data
label.d <- data.table(label = 'Rural Free Delivery was instituted at the turn of the 20th century; this resulted in the mass closure of post offices as rural residents began to receive mail delivered to their door.',
                      x = 1905, y = .9)
# Handmad x-axis data
label.x <- data.table(label = c(1825, 1875, 1925),
                      x = c(1825, 1875, 1925),
                      y = rep(1.475, 3))

ggplot(
  z,
  aes(x = Y, y = 1, fill = net))+
  geom_tile(stat = 'identity', height = 1) + 
  scale_fill_viridis(option = 'A') +
  geom_textbox(
    data = title.d,
    aes(x, y, label = label),
    inherit.aes = FALSE, 
    hjust = 0,
    size = 12.5,
    fill = NA,
    width = grid::unit(0.25, "npc"),
    box.color = NA,
    color = "white",
    family = 'Apercu Pro',
    alpha = 0.95) + 
  geom_textbox(
    data = label.d,
    aes(x, y, label = label),
    inherit.aes = FALSE, 
    hjust = 0,
    size = 7.5,
    fill = NA,
    width = grid::unit(0.2, "npc"),
    box.color = NA,
    color = "grey90",
    family = 'Apercu Pro',
    alpha = 0.75) +
  geom_textbox(
    data = label.x,
    aes(x, y, label = label),
    inherit.aes = FALSE, 
    hjust = 0,
    size = 6.25,
    fill = NA,
    width = grid::unit(0.2, "npc"),
    box.color = NA,
    color = "grey20",
    family = 'Apercu Pro',
    alpha = 0.75) +
  labs(caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\nData from TidyTuesday via Cameron and Helbock 2021 - https://doi.org/10.7910/DVN/NUKCNA") + 
  theme_void() + 
  guides(
    fill = guide_colourbar(
      barwidth = 30,
      barheight = 0.5,
      ticks = FALSE,
      title.hjust = 0.5,
      title = 'Net Change In Number Of Operating Post Offices',
      title.position = 'bottom',
      label.theme = element_text(family = 'Apercu Pro', color = 'grey50'),
      title.theme = element_text(family = 'Apercu Pro', color = 'grey50'))) + 
  theme(
    panel.background = element_rect(color = bg, fill = bg),
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(c(10, 10 , 10, 10)),
    panel.border  = element_blank(),
    plot.caption = element_text(family = 'Apercu Pro', color = 'grey20'),
    plot.caption.position = 'plot',
    legend.position = 'bottom') + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 20,
    height = 7,
    device = 'png')
  


