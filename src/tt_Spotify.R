library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(brms)


# Data Prep ---------------------------------------------------------------
tidy.week <- '2020-01-21'
tt_data <- tt_load(tidy.week) 
y <- data.table(tt_data$spotify_songs)

y[, date := as.Date(track_album_release_date)]
y <- y[!is.na(date)]
y[, YEAR := as.integer(format(date, '%Y'))]

# Calculate annual summary stats
y <- y[YEAR > 1980, .(
  danceability = mean(danceability),
  energy = mean(energy),
  speechiness = mean(speechiness),
  instrumentalness = mean(instrumentalness),
  liveness = mean(liveness),
  valence = mean(valence)), # scale down
  by = YEAR]

# Bring Year onto the same scale as the other measures
y[, YEAR := (YEAR - 1981)/ 100]



# Model Fitting -----------------------------------------------------------
# Extract the variables of interest as character
vars <- names(y)[-1]

# If we can't find a set of stan model fit files, run the sampler to get them
if(!file.exists(here::here('data', tidy.week, 'GP.Rds'))){
  # Fit a model to each measure of interest
  # NOTE: This is rough and ready, there will be divergent transitions etc.
  #       Running for longer, and adjusting by more covars might improve this.
  # This could also be performed without aggregating by year (i.e. on each song)
  z <- lapply(
    vars,
    # Gaussian Process in BRMS
    FUN = function(x, dt){
      brm(
        formula = measure ~ gp(YEAR),
        # Subset data, just the year and measure of interest (e.g. energy)
        dt[,.(YEAR, measure = get(x))],
        chains = 1,
        control = list(adapt_delta = 0.99))},
    # Extra argument: the data table that gets passed to FUN
    dt = y)
  
  # name the list elements
  names(z) <- vars
  
  saveRDS(z, file = here::here('data', tidy.week, 'GP.Rds'))
} else { # If we can find the models, read them in
  z <- readRDS(here::here('data', tidy.week, 'GP.Rds'))
}

# Get fit draws
j <- lapply(
  X = z,
  FUN = conditional_effects, 
  spaghetti = TRUE, 
  nsamples = 250)

# Extracted the fitted posterior functions
k <- lapply(
  j,
  function(x) data.table(lapply(x, attributes)$`YEAR`$spaghetti))

names(k) <- names(z)

k <- rbindlist(k, idcol = 'var')
k[, var := toupper(var)] # upper case for the plot


# extract the observed data points
k2 <- lapply(
  j,
  function(x) data.table(lapply(x, attributes)$`YEAR`$point))
names(k2) <- names(z)
k2 <- rbindlist(k2, idcol = 'var')
k2[, var := toupper(var)]

# Rescale the measure of year
k[, YEAR := (YEAR * 100) + 1981]
k2[, YEAR := (YEAR * 100) + 1981]

# Plotting ----------------------------------------------------------------
pal <- c('#00A7E1', '#69A197', '#034732', '#f230aa', '#006BA6', '#C1121F')

ggplot(
  k[!var %in% toupper(c('track_popularity', 'acousticness'))], # hide weakest vars
  aes(
    x = YEAR,
    y = estimate__,
    group = sample__)) +
  geom_smooth(
    aes(color = var),
    se = FALSE,
    stat = 'identity',
    size = 0.05,
    alpha = 0.1) + 
  geom_point(
    data = k2[!var %in% toupper(c('track_popularity', 'acousticness'))],
    aes(x = YEAR, y = resp__),
    shape = 21,
    fill = 'black',
    color = 'white',
    inherit.aes = FALSE,
    stroke = .5,
    size = 1) +
  scale_x_continuous(expand = c(0.025, 0.025)) +
  facet_wrap(.~var, scales = 'free', nrow = 2) + 
  theme_void() + 
  guides(color = FALSE, fill = FALSE) +
  labs(
    title = 'Bayesian Gaussian Process Modelling Of Spotify Song Characteristics Through Time',
    subtitle = 'These plots show the relative evolution of the characteristics of songs listed on Spotify, averaged by year of release.\nEach point represents the mean characteristic of all songs released in a given year.\nEach line represents a possible functional relationships between year of release and the characteristic, as sampled from the posterior distribution.',
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly) - Data from TidyTuesday and Spotify")+
  theme(
    plot.margin = margin(20, 10, 0, 10),
    plot.title = element_text(
      size = 15,
      color = 'black',
      family = 'DIN Next LT Pro Bold',
      margin = margin(0, 10, 0, 10)),
    plot.caption = element_text(
      size = 7,
      color = 'black',
      family = 'DIN Next LT Pro',
      margin = margin(20, 10, 5, 10)
    ),
    text = element_text(
      color = 'black',
      family = 'DIN Next LT Pro Light'),
    strip.text = element_text(
      color = 'black',
      hjust = 0,
      size = 10),
    strip.background = element_rect(
      fill = '#FEF8EC',
      color = NA),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(2.5, "lines"),
    axis.text.x = element_text(size = 5), 
    panel.background = element_rect(
      fill = '#FEF8EC',
      color = NA),
    plot.background = element_rect(
      fill = '#FEF8EC',
      color = NA)) +
  scale_color_manual(values = pal) + 
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 11,
    height = 7,
    device = 'png')



