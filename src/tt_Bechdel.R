library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(brms)
library(ggdist)


tidy.week <- '2021-03-09'
tt_data <- tt_load(tidy.week) 


x <- tt_data$movies


z <- x %>% 
  filter(year > 1995) %>%
  # pick the cols we want
  select(
    year,
    binary,
    country) %>% 
  # Guess the country
  mutate(country = sapply(strsplit(country, ","), head, 1)) %>% 
  mutate(US = country == 'USA') %>%
  filter(!is.na(country))


# logistic regression, odds of failing the bechdel test by USA/not USA status
m <- brm(
  formula = binary ~ US,
  family = 'bernoulli',
  z,
  chains = 1)


pal = c('#B9314F', '#F2AF29')
pal.2 <- c('#ffeda0', '#feb24c', '#f03b20')

x.labs <- data.frame(
  x = seq(0.6, 1.2, by = 0.1),
  y = -0.1,
  labs = seq(0.6, 1.2, by = 0.1))

posterior_samples(m) %>% 
  ggplot(aes(x = exp(b_USTRUE), fill = stat(x < 1))) +
  stat_interval(aes(y = -0.035), ) +
  stat_dots(
    quantiles = 1500,
    scale = .95,
    dotsize = .475,
    color = NA,
    position = 'dodge') +
  geom_text(
    data = x.labs,
    inherit.aes = FALSE,
    color = 'grey90',
    family = 'SofiaProRegular',
    aes(x = x, y = y, label = labs)) + 
  theme_void() + 
  scale_fill_manual(values = pal) + 
  scale_color_manual(values = pal.2) + 
  guides(fill = FALSE, color = FALSE) + 
  labs(title = toupper('Bayesian Estimation of The association between production location\nand the odds of a film passing The Bechdel Test')) + 
  theme(
    plot.margin = margin(10,10,10,10),
    text = element_text(color = 'grey90', family = 'SofiaProRegular'),
    plot.background = element_rect(fill = '#1D3557', color = NA),
    panel.background = element_rect(fill = '#1D3557', color = NA)
  ) + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 11,
    height = 7,
    device = 'png')






