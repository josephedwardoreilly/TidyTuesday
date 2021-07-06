library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(magick)

# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-07-06'

x <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')
setDT(x)

# Keep only the required columns
x <- x[(!is.na(day)) & (!is.na(month)), .(country, independence_from, day, month)]

# Reformat the date
x[, date := paste0(month, '-', day)]
x[, date := as.Date(date, "%b-%d")]

# Relabel some of the independence_from countries 
x[independence_from == 'Russian Soviet Federative Socialist Republic', 
  independence_from := 'Soviet Union/Russia']

x[independence_from == 'Russian Soviet Federative Socialist Republic and German Empire', 
  independence_from := 'Soviet Union/Russia']

x[grepl('Soviet Union', independence_from, ignore.case = TRUE), 
  independence_from := 'Soviet Union/Russia']

x[grepl('Spanish Empire', independence_from, ignore.case = TRUE), 
  independence_from := 'Spain']

x[independence_from == 'United Kingdom of Portugal, Brazil and the Algarves', 
  independence_from := 'Portugal']

x[independence_from == 'United Kingdom of Great Britain and Ireland', 
  independence_from := 'United Kingdom']

x[independence_from == 'Kingdom of Great Britain and Ireland', 
  independence_from := 'United Kingdom']

x[independence_from == 'Australia, New Zealand and the United Kingdom', 
  independence_from := 'United Kingdom']

x[independence_from == 'United Kingdom and the British Mandate for Palestine ', 
  independence_from := 'United Kingdom']

x[independence_from == 'Socialist Federal Republic of Yugoslavia', 
  independence_from := 'SFR Yugoslavia']

# Keep only the big colonial powers
x <- x[independence_from %in% c(
  'United Kingdom', 'France', 'Spain',
  'Soviet Union/Russia', 'Portugal', 'Ottoman Empire', 'SFR Yugoslavia', 'Empire of Japan')]

# Reorder the factor of nations by number of events
x <- x[, .N, by = .(date, independence_from)]

x[, independence_from := factor(
  independence_from,
  levels = x[, sum(N), by = independence_from][order(V1,decreasing = TRUE), independence_from])]


# Plotting ----------------------------------------------------------------

pal = c(
  'Empire of Japan' = '#E65F5C',
  'France' = '#78A1BB',
  'Ottoman Empire' = '#6B0F1A',
  'Portugal' = '#5FBB97',
  'SFR Yugoslavia' = '#E8DB7D',
  'Soviet Union/Russia' = '#D72638',
  'Spain' = '#FF570A',
  'United Kingdom' = '#2A7221')

ggplot(
  x,
  aes(
    x = date,
    y = 1,
    color = independence_from)) + 
  geom_hline(
    yintercept = 1,
    size = .125,
    color = '#F9EBE0') + 
  geom_segment(
    aes(
      x = as.Date('2021-01-01', format = '%Y-%m-%d'),
      xend = as.Date('2021-01-01', format = '%Y-%m-%d'),
      y = .95,
      yend = 1.05),
    size = .125,
    color = '#F9EBE0') +
  geom_point(alpha = 0.75, aes(size = N)) + 
  coord_polar(theta = 'x') + 
  guides(size = FALSE, color = FALSE) + 
  scale_y_continuous(expand = c(0,0), limits = c(0.95, 1.05)) + 
  scale_x_date(expand = c(0,0)) + 
  theme_minimal() + 
  labs(
    title = 'A Year Of Independence Days',
    subtitle = 'These plots show the distribution of national holidays celebrating independence from a given colonial power throughout the year.\nThe size of each point represents the total number of nations celebrating independence from that nation on a given day.\nThe vertical bar in each panel represents 31st Dec/1st Jan.',
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\n Data from TidyTuesday and worldatlas.com") + 
  theme(
    text = element_text(
      color = 'white',
      family = 'BrandonGrotesque-Regular'),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = '#1E2E34', color = NA),
    panel.background = element_rect(fill = '#0D3745', color = 'white'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(color = '#F9EBE0', hjust = 0),
    panel.grid.major.y = element_blank()
  ) + 
  scale_color_manual(values = pal) + 
  facet_wrap(.~independence_from, ncol = 4)

# Save to disk
ggsave(
  filename = here::here('plots', paste0(tidy.week, '.png')),
  width = 11,
  height = 7,
  device = 'png')

# reread the file, use imagemagick to trim the whitespace, save again
image_read(here::here('plots', paste0(tidy.week, '.png'))) %>%
  image_trim() %>%
  image_write(
    path = here::here('plots', paste0(tidy.week, '.png')),
    format = "png")


