library(tidytuesdayR)
library(data.table)
library(ggplot2)
library(ggtext)

tidy.week <- '2021-03-16'


# Data Wrangling ----------------------------------------------------------
x <- data.table(tidytuesdayR::tt_load(tidy.week)$games)

# extract football manager data
z <- x[grepl('football manager', gamename, ignore.case = TRUE)]
# Drop games prior to 2015, post 2019, and the touch variant
z <- z[!grepl('touch|201[234]|202[01]', gamename, ignore.case = TRUE)]
# Build a meaningful date time system 
z[, month.num := match(month, month.name)]
z[, dt := as.Date(paste0(year, '-', month.num, '-1'))]
# only keep data around the pandemic start
z <- z[(year == 2019 & month.num %in% c(11, 12)) |
      (year == 2020 & month.num %in% c(1, 2, 3, 4))]


# Plotting ----------------------------------------------------------------
# pick colour scheme
bg <- "#E9ECEF"
pal <- c("#6C757D",
         "#495057",
         "#343A40",
         "#212529")

theme_set(
  theme_minimal(base_size = 15, 
                base_family = 'MaisonNeue-Light'
  ))

theme_update(
  panel.background = element_rect(color = bg, fill = bg),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(
    color = 'grey75', linetype = 2, size = 0.25),
  panel.spacing = unit(0.5, "lines"),
  panel.border = element_blank(),
  axis.line.x = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_text(angle = 90, color = 'grey50'),
  axis.text.x = element_text(color = 'grey50'),
  axis.text.y = element_text(color = 'grey50'),
  axis.ticks =  element_blank(),
  legend.position = "none",
  plot.title = element_textbox_simple(
    size = 25, lineheight = 0, hjust = 0,
    linetype = 0, 
    padding = margin(5, 5, 5, 5),
    margin = margin(30, 0, 0, 0)),
  plot.subtitle = element_textbox_simple(
    hjust = 0, color = "grey30", linetype = 0,
    padding = margin(5, 5, 5, 5),
    size = 15, margin = margin(t = 1, b = 25)),
  plot.title.position = "plot",
  plot.caption = element_markdown(
    color = "grey50", size = 10, hjust = 1,
    lineheight = 1.05, margin = margin(30, 0, 0, 0)),
  plot.caption.position = "plot", 
  plot.background = element_rect(fill = bg, color = bg),
  plot.margin = margin(c(15, 15 , 15, 15))
)



# Define any labels and annotations
game.labels <- data.frame(
  label = c('FM 2018', 'FM 2017', 'FM 2016', 'FM 2015'),
  gain = c(400, 900, 1300, 1600),
  gamename = rep(NA, 4), 
  dt = as.Date(rep('2020-04-1',4)))

arrow.labels <- data.frame(
  x = as.Date('2020-01-21'),
  x.end = as.Date('2020-02-15'),
  y = 750,
  y.end = 500)

text.labels <- data.frame(
  label = 'The UK lockdown started on 2020-03-23',
  x = as.Date('2020-01-21'),
  y = 1350
)


# Produce the plot
ggplot(z, aes(dt, gain, fill = gamename)) +
  geom_col()+
  scale_fill_manual(values = pal) + 
  scale_y_continuous(limits = c(-2000, 2000)) + 
  ylab('Monthly Change In Unique Player Count') + 
  # Arrow 
  geom_curve(data = arrow.labels,
               inherit.aes = FALSE,
               color = 'grey50',
               arrow = arrow(length = unit(0.03, "npc")),
               angle = 60,
               aes(x = x, xend = x.end, y = y, yend = y.end))+
  # game name labels
  geom_textbox(data = game.labels ,
               family =  'MaisonNeue-Light',
               color = 'grey90',
               box.color = NA,
               fill = NA,
               width = grid::unit(0.1, "npc"),
               hjust = 0.5,
               halign = 0.5,
               vjust = 1,
               aes(x = dt, y = gain, label = label)) + 
  # text labels
  geom_textbox(data = text.labels,
               family =  'MaisonNeue-Light',
               inherit.aes = FALSE, 
               color = 'grey50',
               box.color = NA,
               fill = NA,
               width = grid::unit(0.1, "npc"),
               hjust = 0.5,
               halign = 0.5,
               vjust = 1,
               aes(x = x, y = y, label = label)) + 
  # Plot title, sub title
  labs(
    title = "Revisiting An Old Friend In Lockdown",
    subtitle = "Data from Steam shows that the average number of people playing older versions of **Football Manager** increased dramatically at the start of the covid-19 pandemic.",
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)<br>Data from TidyTuesday and steamcharts.com") +
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 16, height = 10.5, device = 'png')




