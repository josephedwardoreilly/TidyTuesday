library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(geofacet)
library(patchwork)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2019-01-15'

tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$launches)


x <- x[launch_date < '2020-01-01']
x[state_code == 'SU', state_code := 'RU']

bg <- '#242424'
bg.2 <- '#242424'

pal <- c(
  'state' = '#7EBDC2',
  'startup' = '#FFD23F',
  'private' = '#BB4430'
)


# Plot the title and body text 
pt.body <- ggplot() + 
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.80,
      label = 'The distribution of the lightness of foundation shades listed on Sephora and Ulta is not uniform. Plot (a) shows the colour distribution of foundation shades from the 10 largest brands, as measured by the number of unique products the brand produces.'),
  aes(x, y, label = label),
  color = 'grey90',
  box.color = NA,
  family = 'Raleway',
  fill = NA,
  size = 3.5,
  width = grid::unit(0.5, "npc"), halign = 0.5) + 
  theme_void()

      

p.main <- ggplot(x, aes(x = launch_date, y = 1, color = agency_type, fill = agency_type)) + 
  geom_jitter(alpha = 0.75, height = 0.2, size = .75) + 
  theme_void() + 
  scale_x_date(
    limits = as.Date(c('1940-01-01', '2019-01-01')),
    expand = c(0,0),
    breaks = as.Date(c('1960-01-01', '1980-01-01', '2000-01-01', '2019-01-01')),
    date_labels = '%Y') + 
  scale_y_continuous(limits = c(0.3, 1.2))+
  coord_polar() +
  labs(title = 'Space Travel - A Private Enterprise', 
       caption = "Visualisation by Joe O'Reilly (josephedwardoreilly@github.com)") +
  scale_color_manual(values = pal) +
  theme(legend.position = 'none',
        panel.background = element_rect(color = NA, fill = bg),
        panel.spacing = unit(.5, "lines"),
        axis.text.x = element_text(color = 'grey90'),
        text = element_text('Raleway'),
        plot.background = element_rect(fill = bg.2, color = bg.2),
        plot.margin = margin(c(20, 40 , 20, 40)),
        plot.caption = element_text(family = 'Raleway', colour = 'grey90'),
        plot.title = element_text(
          family = 'Raleway',
          colour = 'grey90',
          hjust = 0.5,
          size = 25,
          margin = margin(20, 20, 10, 20)))


p.main + inset_element(
  pt.body,
  left = 0.25, 
  bottom = 0.25,
  right = 0.75,
  top = 0.75)

