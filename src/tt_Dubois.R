library(tidytuesdayR)
library(ggtext)
library(ggplot2)
library(data.table)


# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-06-15'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$tweets)

# Keep data from the week of the challenge only
x <- x[format(datetime, '%Y-%m-%d') %between% c('2021-02-16', '2021-02-23')]
x[, hour:=format(datetime, '%H')]

# Summary of engagement
z <- x[!is.na(hour), .(
  .N,
  likes = sum(like_count),
  retweets = sum(retweet_count)),
  keyby = hour]

# Add hour of tweet back
z <- merge(
  melt(z, id.vars = 'hour'),
  z[,.(hour, N)],
  by = 'hour')

# Rescale to be value per tweet
z[, value := value/N]

# Use code below to do d3 plot
# x <- x[,.(datetime, like_count, retweet_count, quote_count, tweet = 1:nrow(x))]
# 
# z <- merge(
#   melt(x[,-'datetime'], id.var = 'tweet'),
#   x[,.(tweet, datetime)], 
#     by = 'tweet')




# Plotting ----------------------------------------------------------------

# For formatting
z[, variable := toupper(variable)]
pal <- c('#88A096', '#ECA72C')

ggplot(z[variable != 'N'],
       aes(
         x = hour,
         y = value,
         size = value,
         group = hour,
         color = variable)) + 
  geom_point(alpha = 0.95) + 
  facet_wrap(.~variable, scales = 'free', nrow = 3)  +
  guides(color = FALSE, size = FALSE) + 
  theme_void() + 
  labs(title = 'TidyTuesday Twitter Traction',
       subtitle = 'On average, how many engagements per-tweet did a TidyTuesday tweet gain at different times of the day during the Du Bois challenge?',
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly) - Data from TidyTuesday and #DuBoisChallenge") + 
  scale_color_manual(values = pal) + 
  scale_y_continuous(expand = c(.1,0), limits = c(0, NA)) + 
  theme(
    text = element_text(family = 'Apercu Pro', color = 'white'),
    plot.title = element_markdown(
      size = 15,
      color = 'white',
      family = 'Apercu Pro',
      hjust = 1),
    plot.subtitle = element_markdown(
      size = 8,
      color = 'white',
      family = 'Apercu Pro',
      hjust = 1),
    plot.caption = element_text(
      size = 5,
      color = 'white',
      family = 'Apercu Pro'),
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    plot.background = element_rect(fill = '#042439', color = NA),
    panel.background = element_rect(fill = '#02111B', color = '#042439',size = 3),
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, family = 'Apercu Pro Bold', size = 14),
    plot.margin = margin(10, 10, 5, 10)) + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 12,
    height = 5.75,
    device = 'png')
