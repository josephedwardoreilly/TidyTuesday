library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggdist)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-08-24'
x <- data.table(
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
)


z <- x[!is.na(dod) & !is.na(age_at_death_y), .(
  `YEAR OF DEATH` = as.integer(format(max(dod), '%Y')),
  AGE = max(age_at_death_y)), by = dlc_id]
z <- z[`YEAR OF DEATH` > 1975 & `YEAR OF DEATH` < 2019]


# Plotting ----------------------------------------------------------------
ggplot(
  z,
  aes(x = `YEAR OF DEATH`, y = AGE)) +
  geom_point(alpha = 0.15, color = '#2E294E') + 
  geom_smooth(
    method = 'loess',
    color = '#D7263D',
    fill = '#D7263D',
    alpha = 0.25, 
    linetype = 1,
    size = .5)+
  ylab('AGE AT DEATH') + 
  scale_x_continuous(expand = c(0.025, 0.025)) +
  labs(
    title = toupper('Declining Lemur Infant Mortality'),
    subtitle = toupper('lemur infant mortality is declining in captivity, consequently life expectancy is increasing.\nThis plot shows a loess regression fit to the age at death of lemurs who lived at the Duke\nLemur Center.'),
    caption = toupper("Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\nData from TidyTuesday and lemur.duke.edu")) + 
  theme_minimal() +
  theme(
      plot.background = element_rect(fill = '#FBFFF1', color = NA),
      axis.text = element_text(
        family = 'Apercu Mono Pro Regular'),
      text = element_text(family = 'Flama-Basic', color = '#2E294E'),
      plot.title = element_text(family = 'Flama-Bold', color = '#2E294E'),
      plot.subtitle = element_text(
        family = 'Apercu Mono Pro Regular',
        color = '#2E294E', 
        size = 8),
      plot.caption = element_text(
        family = 'Apercu Mono Pro Regular',
        color = '#2E294E', 
        size = 6))

# Save
ggsave(
  filename = here::here('plots', paste0(tidy.week, '.png')),
  width = 7,
  height = 7,
  device = 'png')  


