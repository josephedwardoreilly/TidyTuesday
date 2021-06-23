library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggbump)
library(dplyr)

# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-06-22'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$parks)


# keep 2016 onwards
zz <- x[year > 2015, .(city, spend_per_resident_data, year)]
# Turn spend into an integer
zz[, spend_per_resident_data := as.integer(
  gsub(
    pattern = '\\$',
    replacement = '',
    x = spend_per_resident_data))]

# order on spend and year
setkey(zz, year, spend_per_resident_data)
# rank by spend and year
zz[, rank := .N:1, by = year]
# reorder on rank
setkey(zz, year, rank)

# keep only the top 25 cities in 2020
keep <- zz[year == 2020][1:25, city]
zz <- zz[year > 2015 & city %in% keep]

# factor for city variable, order by 2016 placing 
zz[,city := factor(city, levels = zz[year == 2016, city])]


# Plotting ----------------------------------------------------------------

# Build a sequential palette 
mycolors <- scales::seq_gradient_pal(
  "#28B206",
  "#DEB5A4",
  "Lab")(seq( # curry output function
    0,
    1,
    length.out = zz[,length(unique(city))]))

p <- ggplot(
  zz,
  aes(
    year,
    rank,
    color = city)) +
  geom_bump() +
  geom_point() +
  scale_x_continuous(limits = c(2015.1, 2020.9),breaks = c(2016:2020)) +
  scale_y_reverse(limits = c(50, 0.5), expand = c(0,0)) +
  geom_text(
    data = zz[year == 2016],
    aes(
      x = year - .25,
      label = toupper(city)),
    family = 'Inter',
    size = 3,
    hjust = 1) +
  geom_text(
    data = zz[year == 2020],
    aes(
      x = year + .25,
      label = toupper(city)),
    family = 'Inter',
    size = 3,
    hjust = 0) +
  guides(color = FALSE) +
  ylab('Ranked Per-Resident Spend On Parks') +
  labs(
    title = 'Top 25 US Cities Based On Ranked Per-Resident Park Spending In 2020',
    subtitle = 'This visualisation shows how the ranking of US cities by their per-resident spend on\nparks has evolved from 2016 to 2020.',
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\nData from TidyTuesday and tpl.org"
  ) +
  theme_minimal(base_line_size = 0) +
  theme(
    text = element_text(
      family = 'Inter',
      color = 'white'),
    axis.text = element_text(
      family = 'Inter',
      color = 'white',
      size = 12),
    axis.title.y = element_text(
      family = 'Inter-Bold',
      color = 'white',
      size = 12),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      family = 'Inter-Bold',
      color = 'white'),
    plot.background = element_rect(fill = '#111B0E'),
    plot.margin = margin(10, 10, 5, 10),
    plot.title = element_text(
      size = 15,
      color = 'white',
      family = 'Inter-Bold',
      margin = margin(5, 0, 5, 0)),
    plot.subtitle = element_text(
      size = 10,
      color = 'white',
      family = 'Inter',
      margin = margin(0, 0, 15, 0))
  ) +
  scale_colour_manual(values = mycolors) 

# Using ggsave without '+' as this is not supported in ggplot2 anymore
ggsave(
  plot = p,
  filename = paste0(
    getwd(),
    '/plots/',
    tidy.week,
    '.png'),
  width = 9,
  height = 11,
  device = 'png')


