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

# pick the top country for each entry
x[, country := unlist(strsplit(country, ',', fixed = TRUE))[1], by = show_id]
x[!country %in% c('United States', 'India'), country := 'Other']

z <- 
  x[type == 'Movie' & !is.na(country),
    .(actor = trimws(unlist(strsplit(x = cast, split = ',',fixed = TRUE)))),
    by = title]
z <- z[!is.na(actor)]

top <- z[,.N, by = actor][order(N,decreasing = TRUE)][1:100]

dt <- merge(
  z[actor %in% top$actor], 
  x[,.(release_year, title, country)],
  by = 'title')

dt <- dt[,.N, by = .(actor, release_year, country)]

dt[, actor := factor(
  actor,
  levels = dt[,sum(N), by = actor][order(V1, decreasing = FALSE), actor])]

pal = c("#eca400","#eaf8bf","#006992","#fe938c","#aeb8fe","#cc8b86", "#eaf8af")
bg = 'grey90'

# color by united states, india, other 

ggplot(dt, aes(x = release_year, y = actor, size = N, color = country)) + 
  geom_point(alpha = 0.75) +
  theme_void()+
  scale_color_manual(values = pal)+
  coord_equal()+
  scale_y_discrete(position = "right")+
  theme(
    legend.position = 'none',
    panel.background = element_rect(color = NA, fill = bg),
    panel.spacing = unit(.5, "lines"),
    axis.text.x = element_text(color = 'grey20'),
    axis.text.y = element_text(color = 'grey20', hjust = 0),
    panel.grid.major.y = element_line(color = 'grey80'),
    text = element_text('Bebas'),
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(c(20, 40 , 20, 40)),
    plot.caption = element_text(
      family = 'Bebas',
      colour = 'grey50'),
    plot.title = element_text(
      family = 'Bebas',
      colour = 'grey90',
      hjust = 0.5,
      size = 30,
      margin = margin(20, 20, 10, 20)))