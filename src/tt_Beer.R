library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(geofacet)
library(patchwork)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2020-10-20'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$beer_awards)

z <- x[year>  2010,  .(state, medal)]
z[, state := toupper(state)] # fix some case issues
nrow = 32
ncol = 16

# TODO: Sort the ordering of the medlas so it goes 1,2,3
setkey(z, state, medal)


z[, x := rep_len(1:nrow, .N), by = state]
# z[, y := rep_len(1:nrow, .N), by = state]
# z[, y := sort(y), by = state]

z[, y := rep(1:ceiling(.N/nrow), each = nrow, len = .N), by = state]

pal <- c('Bronze' = '#C06E52', 'Silver' = '#CAC0B9', 'Gold' = '#FAC05E')

ggplot(z, aes(x = x, y= y, fill = medal)) + 
  geom_tile(color = 'grey90') + 
  scale_fill_manual(values = pal) + 
  facet_geo(~ state, grid = "us_state_grid1") + 
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'grey90'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(.25, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks =  element_blank())

