library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(patchwork)
# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-04-20'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$netflix_titles)

#x <- x[release_year > 2000]

# Function to retrieve the mode from a character vector
char.mode <- function(x){
  return(names(sort(table(x), decreasing = TRUE))[1])
}


# Bollywood, Hollywood, or Other as the first country in the country lsiting
x[, country := unlist(strsplit(country, ',', fixed = TRUE))[1], by = show_id]
x[!country %in% c('United States', 'India'), country := 'Other']

z <- 
  x[type == 'Movie' & !is.na(country),
    .(actor = trimws(unlist(strsplit(x = cast, split = ',',fixed = TRUE)))),
    by = title]
z <- z[!is.na(actor)]

top <- z[,.N, by = actor][order(N,decreasing = TRUE)][1:75]

dt <- merge(
  z[actor %in% top$actor], 
  x[,.(release_year, title, country)],
  by = 'title')

# For each actor, are they more boly/holy/other wood
wood <- dt[,.(wood = char.mode(country)), by = actor]
dt <- dt[,.N, by = .(actor, release_year)]

# Add back the 'wood' the actor is most prevalent in
dt <- merge(
  dt,
  wood,
  by = 'actor'
)


dt[, actor := factor(
  actor,
  levels = dt[,sum(N), by = actor][order(V1, decreasing = FALSE), actor])]



# Plotting ----------------------------------------------------------------

pal = c("#eca400","#eaf8bf","#006992","#fe938c","#aeb8fe","#cc8b86", "#eaf8af")
bg = 'grey90'

ggplot(dt, aes(x = release_year, y = actor, size = N, fill= wood)) + 
  geom_point(alpha = 0.5, shape = 21) +
  theme_void() +
  scale_fill_manual(values = pal) +
  coord_equal(clip = 'off') +
  labs(
    title = 'Netflix Plot',
    caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com)") + 
  scale_y_discrete(position = "right") +
  xlab('Date Of Release') + 
  theme(
    legend.position = 'none',
    panel.background = element_rect(color = NA, fill = bg),
    axis.text.x = element_text(color = 'grey20'),
    axis.title.x = element_text(
      color = 'grey20',
      hjust = 0.5,
      margin = margin(5, 0, 0,0 )),
    axis.text.y = element_text(color = 'grey20', hjust = 0, size = 8),
    panel.grid.major.y = element_line(color = 'grey80'),
    text = element_text('Bebas'),
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(c(20, 20 , 10, 20)),
    plot.caption.position = 'plot',
    plot.caption = element_text(
      family = 'Bebas',
      hjust = 1,
      colour = 'grey50', 
      margin = margin(10, 0, 0, 0)),
    plot.title.position = 'panel',
    plot.title = element_text(
      family = 'Bebas',
      colour = 'grey20',
      hjust = 0.5,
      size = 30,
      margin = margin(5, 10, 10, 10)))






