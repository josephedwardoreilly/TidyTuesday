library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(geofacet)
library(biscale)
library(cowplot)


# Data Prep ---------------------------------------------------------------
tidy.week <- '2021-07-20'

x <- tidytuesdayR::tt_load(tidy.week)

x <- data.table(x$drought)

# add year as a column
x[, year := format(valid_start, '%Y')]

# Drought in 2020
z <- x[year %in% c(2020), .SD[drought_lvl == 'None', .(
  area_pct = 100 - mean(area_pct),
  pop_pct = 100 - mean(pop_pct))],
  by = .(code = state_abb, year)]
  
# obtain a tile structure 
ust <- data.table(geofacet::us_state_grid1)

# Build the bivariate categories 
z <- bi_class(
  z,
  x = area_pct,
  y = pop_pct,
  style = "quantile",
  dim = 3)

# Add tile info
z <- merge(
  z,
  ust, 
  by = 'code'
)



# Plotting ----------------------------------------------------------------
# used for dynamic text colouring
pal.gen <- colorRampPalette(c("black", 'white'))

# Draw the legend
legend.inset <- bi_legend(
  pal = "Brown",
  dim = 3,
  xlab = "% of Area In Drought",
  ylab = "% of Population Living In Drought",
  size = 10) + 
  theme(plot.background = element_blank(),
        axis.title = element_text(
          family = 'Gilroy-Bold',
          vjust = 0.5),
        plot.margin = margin(5, 5, 5, 5))

# Main plot
p.main <- ggplot(
  z,
  aes(x = col, y = -row, fill = bi_class)) +
  geom_tile(color = 'grey10') +
  coord_equal(expand = FALSE) +
  labs(
    title = 'Drought In The United States',
    subtitle = "Much of the Western and Mid-western United States are currently living through drought conditions of 'abnormally dry'\nthrough to 'exceptional drought'. This bivariate plot shows the average percentage of each state's area and population\nthat were in drought conditions during 2020." ,
    caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)\nData from TidyTuesday and droughtmonitor.unl.edu") + 
  geom_text(
    aes(x = col, y = -row, label = code, color = bi_class),
    size = 5, family = 'Gilroy-Bold') +
  theme_void() +
  bi_scale_fill(
    pal = 'Brown',
    dim = 3) + 
  scale_color_manual(values = pal.gen(7)) + 
  guides(color = FALSE,
         fill = FALSE) + 
  theme(
    plot.caption = element_text(
      hjust = 0,
      margin = margin(c(20, 5, 5, 5)),
      family = 'Gilroy-Light'),
    plot.title = element_text(
      margin = margin(c(10, 5, 5, 5)),
      family = 'Gilroy-Bold'),
    plot.subtitle = element_text(
      margin = margin(c(5, 5, 25, 5)),
      family = 'Gilroy-Regular'),
    plot.caption.position = 'plot',
    plot.margin = margin(c(5, 100 , 5, 5))
  )

# Draw together
p.total <- ggdraw() +
  draw_plot(p.main, 0, 0, 1, 1) +
  draw_plot(legend.inset, x = 0.875, y = -0.3, scale = .35, hjust = 0.5)

# And save
ggsave(
  filename = here::here('plots', paste0(tidy.week, '.png')),
  width = 11,
  height = 7,
  device = 'png')
