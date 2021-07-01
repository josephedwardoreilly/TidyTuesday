library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(data.table)
library(sf)
library(tidyverse)
library(biscale)
library(cowplot)
library(magick)


# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-06-29'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$animal_rescues)
# Keep only the most interesting animals...
# Rename where required
x[grepl(
  x = animal_group_parent,
  pattern = 'Bird|Budgie|Pigeon'),
  animal_group_parent := 'Bird']

x[grepl(
  pattern = 'cat|Cat',
  x =  animal_group_parent),
  animal_group_parent := 'Cat']
x[grepl(
  x = animal_group_parent,
  pattern =  'Bull|Cow|Sheep|Lamb|Goat|Livestock|Farm'),
  animal_group_parent := 'Livestock']

# Subset on the most important types
x <- x[grepl(
  'Bird|Cat|Dog|Livestock|Deer|Fox|Horse|Squirrel',
  animal_group_parent)]

# add a LA code column
x[, Lacode := borough_code]

# Aggregate counts and costs by borough and animal group
x <- x[!is.na(Lacode),
       .(
         cost = mean(na.rm = TRUE, as.numeric(incident_notional_cost)),
         .N),
       by = .(Lacode, animal_group_parent)]

# Calculate the bivariate classes
x <- bi_class(
  x,
  x = cost,
  y = N,
  style = "quantile",
  dim = 3)

# Reorder the factor for animals by one of the classes
fact.ord <- 
  x[, sum(N), by = animal_group_parent
    ][
      order(V1, decreasing = TRUE), animal_group_parent]
x$animal_group_parent <- factor(x$animal_group_parent, levels = fact.ord)

# Merge the shape data with the quantitative data 
ltladata <- st_read(
  here::here('data', tidy.week, "LocalAuthorities-lowertier.gpkg"),
  layer="4 LTLA-2019") %>% 
  filter(RegionNation == 'London') %>% 
  left_join(x, by="Lacode") 


# Plotting ----------------------------------------------------------------

p.main <- ggplot() + 
  geom_sf(
    data=ltladata, 
    aes(
      geometry=geom,
      fill=bi_class),
    colour="Black",
    show.legend = FALSE,
    size= .35) +
  bi_scale_fill(
    pal = 'DkViolet',
    dim = 3) + 
  facet_wrap(
    .~animal_group_parent, 
    ncol  = 2) +
  labs(caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\nData from TidyTuesday and london.gov.uk") + 
  theme_void() + 
  theme(
    plot.margin = margin(1, 10, 1, 1),
    strip.text = element_text(
      hjust = 0,
      size = 10,
      family = 'Apercu Pro',
      margin = margin(2, 0, 2, 0)),
    panel.spacing = unit(2, "lines"),
    axis.title = element_blank(),
    plot.caption = element_text(
      family = 'Apercu Pro',
      size = 6,
      color = 'grey20',
      hjust = 0),
    plot.background = element_rect(
      color = NA,
      fill = 'white')) 

# Draw the legend
legend.inset <- bi_legend(
  pal = "DkViolet",
  dim = 3,
  ylab = "Increasing \nNumber Of Callouts",
  xlab = "Increasing \nMedian Cost Of Callout",
  size = 10) + 
  theme(plot.background = element_blank(),
        axis.title = element_text(
          family = 'Apercu Pro',
          vjust = 0.5),
        plot.margin = margin(5, 5, 5, 5))

# Draw the blurb text and title
p.text.body <- ggplot() + 
  # Blurb first
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.70,
      label = 'These bivariate plots show the number and the median hourly cost of callouts for animal rescue operations by The London Fire Brigade. Data are presented for each borough of London and by the type of animal being rescued.'),
    aes(x, y, label = label),
    color = 'black',
    box.color = NA,
    fill = 'NA',
    family = "Apercu Pro" ,
    size = 3,
    width = grid::unit(0.75, "npc"), 
    hjust = 0, vjust = 0.5
  ) +
  # Text title
  geom_text(
    data = data.frame(
      x = 0.5,
      y = 0.95,
      label = 'Animal Rescue Callouts\nBy London Fire Brigade (2009-2021)'),
    aes(x, y, label = label),
    color = 'black',
    family = 'Apercu Pro Black',
    size = 3.5,
    hjust = 0.5, vjust = 0.5
  ) +
  xlim(0, 1) +
  ylim(0.5, 1) +
  theme_void()+
  theme(
    plot.margin = margin(5, 0, 0, 0),
    plot.background = element_rect(fill = NA, color = NA)
  )

# Bind all of the plots on one plot
p.total <- ggdraw() +
  draw_plot(p.main, 0, 0, 1, 1) +
  draw_plot(p.text.body, x = 0.85, y = 0.15, scale = 0.35, hjust = 0.5) +
  draw_plot(legend.inset, x = 0.85, y = -0.15, scale = .35, hjust = 0.5)
  

# Save to disk
ggsave(
  filename = here::here('plots', paste0(tidy.week, '.png')),
  width = 11,
  height = 7,
  device = 'png')
  
# reread the file, use imagemagick to trim the whitespace, save again
image_read(here::here('plots', paste0(tidy.week, '.png'))) %>%
image_trim() %>%
image_border(geometry = "20x20",color = '#FFFFFF') %>%
image_write(
  path = here::here('plots', paste0(tidy.week, '.png')),
  format = "png")

  
