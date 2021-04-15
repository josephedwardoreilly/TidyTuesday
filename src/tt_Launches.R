library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(patchwork)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2019-01-15'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$launches)
x <- x[launch_date < '2020-01-01'] # really simple clean



# Plotting ----------------------------------------------------------------
bg <- '#242424'

pal <- c(
  'state' = '#7EBDC2',
  'startup' = '#FFD23F',
  'private' = '#BB4430'
)

# Main text
body.text <- 'Non-State actors are playing an increasing role in rocket launches since the late 1980s. Each point represents a single rocket launch and is coloured based on the type of agency performing the launch. Launches by state based agencies are slowly being replaced by ones conducted by private companies and startups.'

body.text <- gsub(
  x = body.text,
  pattern = 'state',
  replacement = paste0(
    "<span style = 'color:",
    pal['state'],
    ";'>**state**</span>"))

body.text <- gsub(
  x = body.text,
  pattern = 'private',
  replacement = paste0(
    "<span style = 'color:",
    pal['private'],
    ";'>**private**</span>"))

body.text <- gsub(
  x = body.text,
  pattern = 'startups',
  replacement = paste0(
    "<span style = 'color:",
    pal['startup'],
    ";'>**startups**</span>"))

# Plot the body text 
pt.body <- ggplot() + 
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.80,
      label = body.text),
  aes(x, y, label = label),
  color = 'grey70',
  box.color = NA,
  family = 'Antonio Light',
  fill = NA,
  size = 4,
  width = grid::unit(0.6, "npc"), halign = 0.5) + 
  theme_void()

      
# Plot the data
p.main <- ggplot(x, aes(x = launch_date, y = 1, color = agency_type, fill = agency_type)) +
  geom_jitter(alpha = 0.75, height = 0.2, size = .75) + 
  theme_void() + 
  scale_x_date(
    limits = as.Date(c('1940-01-01', '2019-01-01')),
    expand = c(0,0),
    breaks = as.Date(c('1960-01-01', '1980-01-01', '2000-01-01', '2019-01-01')),
    date_labels = '%Y') + 
  scale_y_continuous(
    limits = c(0.3, 1.2))+
  coord_polar() +
  labs(title = 'Space - An Increasingly Private Enterprise', 
       caption = "Visualisation by Joe O'Reilly (josephedwardoreilly@github.com)\nData from TidyTuesday and planet4589.org/space/lvdb/index.html") +
  scale_color_manual(values = pal) +
  theme(
    legend.position = 'none',
    panel.background = element_rect(color = NA, fill = bg),
    panel.spacing = unit(.5, "lines"),
    axis.text.x = element_text(color = 'grey70'),
    text = element_text('Antonio'),
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(c(20, 40 , 20, 40)),
    plot.caption = element_text(
      family = 'Antonio Light',
      colour = 'grey50'),
    plot.title = element_text(
      family = 'Antonio',
      colour = 'grey90',
      hjust = 0.5,
      size = 30,
      margin = margin(20, 20, 10, 20)))

# Build final structure
p.main + 
  inset_element(
    pt.body,
    left = 0.25, 
    bottom = 0.25,
    right = 0.75,
    top = 0.75) + 
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 10.5, height = 10.5, device = 'png'
  )

