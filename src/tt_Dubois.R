library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggwordcloud)

# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-06-15'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$tweets)

# Keep data from the week of the challenge only
x <- x[format(datetime, '%Y-%m-%d') %between% c('2021-02-16', '2021-02-23')]

# split on whitespace
y <- x[,unlist(strsplit(split  = ' ', x =  content))]


# remove any dodgy characters
y <- gsub('\n', '', y, fixed = TRUE)
y <- gsub('(', '', y, fixed = TRUE)
y <- gsub(')', '', y, fixed = TRUE)
y <- gsub('-', '', y, fixed = TRUE)
y <- gsub(';', '', y, fixed = TRUE)
y <- gsub(':', '', y, fixed = TRUE)
y <- gsub('&amp', 'and', y, fixed = TRUE)
y <- gsub(' ', '', y, fixed = TRUE)

# drop trailing comma or full stop
y <- gsub(",$", "", y)
y <- gsub("\\.$", "", y)

# remove urls
y <- grep(x = y, pattern = 'http',invert = TRUE, value = TRUE)
# remove whitespace chars
y <- y[y!='']
# remove usernames
y <- grep(x = y, pattern = '^@', invert = TRUE, value = TRUE)

# Word freqs
z <- data.table(sort(table(toupper(y)),decreasing = TRUE)[1:100])

# Add some angles
z[,angle := 45 * sample(-2:2, .N, replace = TRUE, prob = c(1, 1, 4, 1, 1))]


# Plotting ----------------------------------------------------------------

ggplot(z, aes(label = V1, size = N, color = N, angle = angle)) +
  geom_text_wordcloud(
    family = 'Gilroy-Light',
    area_corr_power = 1) +
  labs(title = 'The 100 Most Frequent Words In #DuBoisChallenge Tweets (2021-02-16 To 2021-02-23)',
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly) - Data from TidyTuesday and #DuBoisChallenge") +
  scale_color_gradient(low = "#FCE2C5", high = "#E9820C") +
theme(
    panel.background = element_rect(fill = '#25283D', color = NA),
    plot.background = element_rect(fill = '#25283D', color = NA),
    plot.margin = margin(10, 10, 5, 10),
    plot.title = element_markdown(
      size = 15,
      color = 'white',
      family = 'Gilroy-Regular',
      hjust = 0),
    plot.caption = element_text(
      size = 10,
      color = 'white',
      family = 'Gilroy-Light')
  ) + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 10,
    height = 5,
    device = 'png')


