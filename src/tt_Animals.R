library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(sf)
library(tidyverse)
library(biscale)
library(pals)
library(patchwork)



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
  theme_void() + 
  theme(
    plot.margin = margin(1, 10, 1, 10),
    strip.text = element_text(hjust = 0),
    panel.spacing.y = unit(2, "lines"),
    axis.title = element_blank(),
    plot.title = element_text(
      family = 'Helvetica Neue',
      margin = margin(5, 5, 5, 5)),
    plot.background = element_rect(color = NA, fill = 'white')) 

legend.inset <- bi_legend(
  pal = "DkViolet",
  dim = 3,
  ylab = "Increasing \nNumber Of Callouts",
  xlab = "Increading \nMedian Cost Of Callout",
  size = 10) + 
  theme(plot.background = element_blank(),
        axis.title = element_text(
          family = 'Helvetica Neue', vjust = 0.5),
        plot.margin = margin(5, 5, 5, 5))

p.text.body <- ggplot() + 
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.70,
      label = 'The Scottish Index Of Multiple Deprivation 2020 (SIMD) is an area based ranked index of social deprivation. It is constructed from seven domains, or aspects, of deprivation. Each area is ranked within each domain and then the ranks over all seven domains are combined to create the final SIMD rank. These plots show the difference between the mean SIMD rank for a local area, and the domain specific rank for that same area. **Note: SIMD ranks most to least deprived in ascending order, for these plots the ranking is inverted, such that a lower rank indicates less social deprivation. This has been done to improve ease of interpretation**'),
    aes(x, y, label = label),
    color = 'black',
    box.color = NA,
    fill = 'NA',
    family = "Helvetica Neue" ,
    size = 2,
    width = grid::unit(0.75, "npc"), 
    hjust = 0, vjust = 0.5
  ) +
  # Text title
  geom_richtext(
    data = data.frame(
      x = 0.5,
      y = 0.95,
      label = 'Animal Rescue Callouts <br>By London Fire Brigade (2009-2021)'),
    aes(x, y, label = label),
    color = 'black',
    label.color = NA,
    fill = NA,
    size = 3,
    hjust = 0.5, vjust = 0.5
  ) +
  xlim(0, 1) +
  ylim(0.5, 1) +
  theme_void()+
  theme(
    plot.margin = margin(5, 0, 0, 0),
    plot.background = element_rect(fill = NA, color = NA)
  )

legend <- ggdraw() +
  draw_plot(p.main, 0, 0, 1, 1) +
  draw_plot(p.text.body, x = 0.85, y = 0.25, scale = 0.35, hjust = 0.5) +
  draw_plot(legend.inset, x = 0.85, y = -0.25, scale = .35, hjust = 0.5)

ggsave(
  filename = here::here('plots', paste0(tidy.week, '.png')),
  width = 11,
  height = 7,
  device = 'png')
  
# reread the file, use imagemagick to trim the whitespace
image_read(here::here('plots', paste0(tidy.week, '.png'))) %>%
image_trim() %>%
image_border(geometry = "50x50",color = '#FFFFFF') %>%
image_write(
  path = here::here('plots', paste0(tidy.week, '.png')),
  format = "png")

  
