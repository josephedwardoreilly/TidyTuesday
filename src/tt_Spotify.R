library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(brms)


spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


x <- data.table(spotify_songs)
# Fit a gaussian process to tempo through time
y <- x
y[, date := as.Date(track_album_release_date)]
y <- y[!is.na(date)]
y[, YEAR := as.integer(format(date, '%Y'))]

# Calculate annula summary stats
y <- y[YEAR > 1980, .(
  danceability = mean(danceability),
  energy = mean(energy),
  speechiness = mean(speechiness),
  acousticness = mean(acousticness),
  instrumentalness = mean(instrumentalness),
  liveness = mean(liveness),
  valence = mean(valence),
  track_popularity = mean(track_popularity)/100), 
  by = YEAR]

# Bring Year onto the same scale as the measures
y[, YEAR:= (YEAR-min(YEAR) + 1) / 100]


# Extract the variables of interest as character
vars <- names(y)[-1]

if(!file.exists('~/Dropbox/TidyTuesday/data/2020-01-21/GP.Rds')){
  # Fit a model to each measure of interest
  z <- lapply(
    vars,
    # Gaussian Process in BRMS
    FUN = function(x, dt){
      brm(
        formula = measure ~ gp(YEAR),
        # Subset data just the year and measure of interest (e.g. energy)
        dt[,.(YEAR, measure = get(x))],
        chains = 1,
        control = list(adapt_delta = 0.99))},
    # Extra argument: the data table that gets passed to FUN
    dt = y)
  
  # name the list elements
  names(z) <- vars
  
  saveRDS(z, file = '~/Dropbox/TidyTuesday/data/2020-01-21/GP.Rds')
} else {
  z <- readRDS('~/Dropbox/TidyTuesday/data/2020-01-21/GP.Rds')
}

# Get fit draws
j <- lapply(
  X = z,
  FUN = conditional_effects, 
  spaghetti = TRUE, 
  nsamples = 250)

# Extracted the fitted functions
k <- lapply(
  j,
  function(x) data.table(lapply(x, attributes)$`YEAR`$spaghetti))

names(k) <- names(z)


k <- rbindlist(k, idcol = 'var')
k[, var := toupper(var)]


# extract the observed data
k2 <- lapply(
  j,
  function(x) data.table(lapply(x, attributes)$`YEAR`$point))
names(k2) <- names(z)
k2 <- rbindlist(k2, idcol = 'var')
k2[, var := toupper(var)]




pal <- c('#00A7E1', '#69A197', '#034732', '#f230aa', '#006BA6', '#C1121F')

ggplot(
  k[!var %in% toupper(c('track_popularity', 'acousticness'))], # hide weakest vars
  aes(
    x = effect1__,
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
  scale_x_continuous(expand = c(0.025,0.025)) +
  facet_wrap(.~var, scales = 'free', nrow = 2) + 
  theme_void() + 
  guides(color = FALSE, fill = FALSE) +
  labs(title = 'Bayesian Gaussian Process Modelling Of Spotify Song Characteristics Through Time')+
  theme(
    plot.margin = margin(20, 10, 20, 10),
    text = element_text(color = 'black', family = 'DIN Next LT Pro'),
    strip.text = element_text(color = 'black', hjust = 0, size = 15),
    strip.background = element_rect(fill = '#FEF8EC', color = NA),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(2.5, "lines"),
    axis.text.x = element_text(), 
        panel.background = element_rect(fill = '#FEF8EC', color = NA),
        plot.background = element_rect(fill = '#FEF8EC', color = NA)) +
  scale_color_manual(values = pal) 



