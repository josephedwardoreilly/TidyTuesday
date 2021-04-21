library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-04-20'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$netflix_titles)

# Function to retrieve the mode from a character vector
char.mode <- function(x){
  return(names(sort(table(x), decreasing = TRUE))[1])
}

# Bollywood, Hollywood, or Other as the first country in the country listing
x[, country := unlist(strsplit(country, ',', fixed = TRUE))[1], by = show_id]
x[!country %in% c('United States', 'India'), country := 'Other']

# Get cast details for each listing
z <- 
  x[ !is.na(country),
    .(actor = trimws(unlist(strsplit(x = cast, split = ',',fixed = TRUE)))),
    by = title]
z <- z[!is.na(actor)]

# Top 75 actors by credit count
top <- z[,.N, by = actor][order(N, decreasing = TRUE)][1:75]

# Merge all info together
dt <- merge(
  z[actor %in% top$actor], 
  x[,.(release_year, title, country)],
  by = 'title')

# For each actor, are they more bolly/holly/other wood
wood <- dt[,.(`Production Industry` = char.mode(country)), by = actor]
dt <- dt[,.N, by = .(actor, release_year)]

# Add back the 'wood' the actor is most prevalent in
dt <- merge(
  dt,
  wood,
  by = 'actor'
)

# Order by total number
dt[, actor := factor(
  actor,
  levels = dt[,sum(N), by = actor][order(V1, decreasing = FALSE), actor])]

# Add the total for each actor back onto the focal data.table
dt <- merge(
  dt, 
  dt[,.(total = sum(N)), by = actor],
  by = 'actor'
)

# paste total number of credits to each actor's name
dt[,actor := paste0(
  '(',
  total,
  ') ',
  actor)]

# Rename the countries to reflect the industry instead
dt[`Production Industry` == 'United States',
   `Production Industry` := 'Hollywood']
dt[`Production Industry`== 'India',
   `Production Industry` := 'Bollywood']


# Plotting ----------------------------------------------------------------

pal = c("#E50914", "#221F1F", "#E4E4D0")
bg = '#F5F5F1'

ggplot(dt, 
       aes(
         x = release_year,
         y = actor, 
         size = N, 
         fill = `Production Industry`)) + 
  geom_point(alpha = 0.75, shape = 21) +
  theme_void() +
  scale_fill_manual(values = pal) +
  coord_equal(clip = 'off') +
  labs(
    title = 'The 75 Most Credited People On Netflix',
    subtitle = "Hollywood stars are not the most highly credited on Netflix. Each circle represents the number of unique films or TV shows that an individual featured in for a given year. The larger a circle, the more credits a given individual had in that year. The colour of each circle represents the production industry each individual is most prevalent in: Hollywood (USA), Bollywood (India), or Other. The value next to each individual's name is their total number of credits on Netflix.",
    caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com)\nData from TidyTuesday - Kaggle") + 
  scale_y_discrete(position = "right") +
  xlab('Year Of Release') + 
  guides(
    size = FALSE,
    fill = guide_legend(override.aes = list(size = 3)))+
  theme(
    legend.margin = margin(5, 5, 5, 5),
    legend.box.background = element_rect(
      color = 'grey80',
      fill = NA),
    legend.box.margin  = margin(2,2,2,2),
    legend.position = 'top',
    panel.background = element_rect(
      color = NA,
      fill = bg),
    axis.text.x = element_text(
      color = 'grey20'),
    axis.title.x = element_text(
      color = 'grey20',
      hjust = 0.5,
      margin = margin(5, 0, 0, 0)),
    axis.text.y = element_text(
      color = 'grey20',
      hjust = 0,
      size = 8),
    panel.grid.major.y = element_line(
      color = 'grey80'),
    text = element_text('Bebas'),
    plot.background = element_rect(
      fill = bg, 
      color = bg),
    plot.margin = margin(c(10, 20 , 5, 20)),
    plot.caption.position = 'plot',
    plot.caption = element_text(
      family = 'Bebas',
      hjust = 1,
      colour = 'grey50', 
      margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_textbox(
      family = 'Bebas',
      width = grid::unit(1, "npc"),
      colour = '#221F1F',
      hjust = 0.5, halign = 0, 
      size = 10,
      margin = margin(5, 10, 30, 10)),
    plot.title.position = 'plot',
    plot.title = element_text(
      family = 'Bebas',
      colour = '#E50914',
      hjust = 0.5,
      size = 32,
      margin = margin(0, 10, 10, 10))) + 
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 10,
    height = 12,
    device = 'png')






