library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(brms)
library(ggdist)
library(ggtext)


# Data Prep ---------------------------------------------------------------


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
  # Binary US/NonUS column
  mutate(US = country == 'USA') %>%
  filter(!is.na(country))


# Modelling ---------------------------------------------------------------


# logistic regression, odds of failing the Bechdel test by USA/not USA status
# NOTE: Model is simple so the rough and ready 1 chain, no priors approach is 
#       unlikely to be an issue here. ESS and r-hat will be good. 
m <- brm(
  formula = binary ~ US,
  family = 'bernoulli',
  z,
  chains = 1)

# PP estimate of US films being more likely to pass the Bechdel test
pp_estimate <- posterior_samples(m) %>%
  summarise(pp = mean(exp(b_USTRUE) < 1)) %>% 
  unlist() %>%
  round(digits = 2)

# Plotting ----------------------------------------------------------------


# Some colours
pal <- c('#B9314F', '#F2AF29')
pal_2 <- c('#ffeda0', '#feb24c', '#f03b20')

# Arbitrary x axis 
x_labs <- data.frame(
  x = seq(0.6, 1.2, by = 0.1),
  y = -0.1,
  labs = seq(0.6, 1.2, by = 0.1))

x_title <- data.frame(
  x = 0.9,
  y = -0.175,
  labs = toupper('Odds Ratio')
)

# Main body text
p_text <- data.frame(
  x = 0.9,
  y = -0.375,
  labs = 'This visualisation presents the results from a Bayesian logistic regression to estimate the association between production country (in The USA or outside The USA) and the odds of a film failing the Bechdel test. Each dot represents a sampled odds ratio from the posterior distribution and is coloured according to whether the estimate was above or below one. A value above one supports the hypothesis that films produced in The USA are more likely to pass the Bechdel test than films produced in other countries. The bar underneath the plot represents the 50, 80, and 95 percent limits of the marginal posterior distribution for the odds ratio. On the balance of probability, films produced in The USA are less likely to pass the Bechdel test than those produced elsewhere; P(OR < 1) = PP_ESTIMATE .'
)

# Add some formatting to the raw text
p_text <- p_text %>% 
  mutate(
    labs = gsub(
    x = labs,
    pattern = 'PP_ESTIMATE',
    replacement = pp_estimate)) %>%
  mutate(
    labs = gsub(
      x = labs,
      pattern = 'above',
      replacement = paste0(
        "<span style = 'color:",
        '#B9314F',
        ";'>**above**</span>")
    )) %>%
  mutate(
    labs = gsub(
      x = labs,
      pattern = 'below',
      replacement = paste0(
        "<span style = 'color:",
        '#F2AF29',
        ";'>**below**</span>")
    )) %>%
  mutate(
    labs = gsub(
      x = labs,
      pattern = '50',
      replacement = paste0(
        "<span style = 'color:",
        '#f03b20',
        ";'>**50**</span>")
    )) %>%
  mutate(
    labs = gsub(
      x = labs,
      pattern = '80',
      replacement = paste0(
        "<span style = 'color:",
        '#feb24c',
        ";'>**80**</span>")
    )) %>%
  mutate(
    labs = gsub(
      x = labs,
      pattern = '95',
      replacement = paste0(
        "<span style = 'color:",
        '#ffeda0',
        ";'>**95**</span>")
    ))


# Build main plot
posterior_samples(m) %>% 
  ggplot(aes(x = exp(b_USTRUE), fill = stat(x < 1))) +
  stat_interval(aes(y = -0.035),
                .width = c(.5, .8, .95)) +
  stat_dots(
    quantiles = 1500,
    scale = .95,
    dotsize = .475,
    color = NA,
    position = 'dodge') +
  # Axis Labels
  geom_text(
    data = x_labs,
    inherit.aes = FALSE,
    color = 'grey90',
    family = 'MaisonNeue-Medium',
    aes(x = x, y = y, label = labs)) + 
  # Axis Title
  geom_text(
    data = x_title,
    inherit.aes = FALSE,
    color = 'grey90',
    family = 'MaisonNeue-Medium',
    aes(x = x, y = y, label = labs)) + 
  theme_void() + 
  geom_textbox(
    data = p_text, inherit.aes = FALSE,
    width = grid::unit(0.95, "npc"),
    box.color = NA,
    color = 'grey90',
    family = 'MaisonNeue-Light',
    size = 3.5,
    fill = NA,
    aes(x = x, y = y, label = labs)) + 
  scale_fill_manual(values = pal) + 
  scale_color_manual(values = pal_2) + 
  scale_x_continuous(limits = c(0.5, 1.3)) + 
  guides(fill = FALSE, color = FALSE) + 
  labs(
    title = toupper('Bayesian Estimation of The association between production of a film in The USA \nand the odds of it failing The Bechdel Test'),
    caption = toupper("Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\n Data from TidyTuesday and Bechdeltest.com")) + 
  theme(
    plot.margin = margin(10,10,10,10),
    text = element_text(
      color = 'grey90', 
      family = 'MaisonNeue-Light'),
    plot.background = element_rect(
      fill = '#1D3557',
      color = NA),
    panel.background = element_rect(
      fill = '#1D3557',
      color = NA),
    plot.title = element_text(
      color = 'grey90',
      family = 'MaisonNeue-Bold'),
    plot.caption = element_text(
      color = 'grey90',
      family = 'MaisonNeue-Light',
      size = 6)) + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 11,
    height = 7,
    device = 'png')






