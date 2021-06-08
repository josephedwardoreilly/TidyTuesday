library(tidytuesdayR)
library(ggtext)
library(ggplot2)
library(data.table)


# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-06-08'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$fishing)
x <- x[year > 1915]

# Group species together
x[grepl('trout', ignore.case = TRUE, x = species), species := 'TROUT']
x[grepl('salmon', ignore.case = TRUE, x = species), species := 'SALMON']
x[grepl('perch', ignore.case = TRUE, x = species), species := 'PERCH']
x[!grepl('TROUT|PERCH|SALMON', x = species), species := 'OTHER']

# % change in each fish, per lake 
x[is.na(values), values := 0]
x <- x[, .(tot = sum(values, na.rm = TRUE)), by = .(year, species)]
setkey(x, species, year)

# Do the % change calculation
x[, paste0("change") := lapply(
  .SD, 
  function(x) x/shift(x)),
  by = species, .SDcols = c('tot')]




# Plotting ----------------------------------------------------------------

pal = c('#1B4C87', '#E28413')

title.text <- 'ANNUAL INCREASE OR DECREASE IN FISH STOCK ACROSS THE GREAT LAKES (1916-2015)'

title.text <- gsub(
  x = title.text,
  pattern = 'INCREASE',
  replacement = paste0(
    "<span style = 'color:",
    '#E28413',
    ";'>**INCREASE**</span>"))

title.text <- gsub(
  x = title.text,
  pattern = 'DECREASE',
  replacement = paste0(
    "<span style = 'color:",
    '#1B4C87',
    ";'>**DECREASE**</span>"))


ggplot(x[!is.na(change)],
       aes(x = year, y = 1, fill = change > 1.0, group = species)) + 
  geom_tile(color = '#E8FAFA') + 
  facet_wrap(.~species, ncol = 1) +
  scale_x_continuous(limits = c(1916, 2015),expand = c(0,0)) + 
  theme_void() + 
  labs(title = title.text,
       subtitle = toupper('This visualisation shows years in which observed stock for a given species increased or decreased across The Great Lakes'),
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\nData from TidyTuesday and Great Lakes Fishery Commission") + 
  guides(fill = FALSE) + 
  theme(
    text = element_text(family = 'SofiaPro-Light'),
    plot.title = element_markdown(
      size = 15,
      color = 'black',
      family = 'SofiaPro-Bold'),
    plot.subtitle = element_markdown(
      size = 8,
      color = 'black',
      family = 'SofiaPro-Light'),
    plot.caption = element_text(
      size = 5,
      color = 'black',
      family = 'SofiaPro-Light'),
    axis.text.x = element_text(),
    plot.background = element_rect(fill = '#E8FAFA', color = NA),
    panel.background = element_rect(fill = '#E8FAFA', color = NA),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 1, family = 'SofiaPro-Bold'),
    plot.margin = margin(10, 10, 5, 10)) + 
  scale_fill_manual(values = pal) + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 11,
    height = 3,
    device = 'png')


