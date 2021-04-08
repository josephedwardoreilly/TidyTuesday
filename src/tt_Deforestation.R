library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(geofacet)
library(patchwork)


# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2021-04-06'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$forest_area)

# obtain a tile structure for countries of the world
wt <- data.table(geofacet::world_countries_grid1)

# get the average coverage in last decade 
x <- x[year %in% 2010:2020,
       .(forest_area = mean(forest_area, na.rm = TRUE)), by = .(code)]
# Merge the tile information onto the forest data
z <- merge(x, 
      wt, 
      by.x = 'code',
      by.y = 'code_alpha3',
      all.y = TRUE)

# A colour palette going from yellow to green
pal.gen <- colorRampPalette(c("#DBDFAC", "#114A50"))
pal <- setNames(pal.gen(6), as.character(1:6))

# Background colour
bg = '#FBFFF1'

# Bin the forest coverage by percentage
bins <- c(0, 0.001, 0.01, 0.1,
          1, 10, 100)
z[, bin := as.character(
  .bincode(forest_area, bins, include.lowest = TRUE))] # forest_area == 0 in lowest bin

z[code == 'ATA', bin := '1'] # Antarctica has no forest




# Plotting ----------------------------------------------------------------

# Main tile plot
p <- ggplot(z, aes(x = col, y = -row, fill = bin)) +
  geom_tile(color = 'grey10') +
  coord_equal(expand = FALSE) +
  geom_text(
    aes(x = col, y = -row, label = code, color = bin),
    size = 3, family = 'Bebas') +
  theme_void() + 
      scale_fill_manual(
        values = pal,
        na.value = '#4B3B40') + 
      scale_color_manual(
        values = rep('black', 6),
        na.value = 'grey10') +
  guides(color = FALSE,
         fill = FALSE) +
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0,
      y = -21,
      label = "**Distribution Of Total Global Forest Coverage (2010-2020)**<br> 
      <span style = 'font-size:10pt'> Each tile represents a single nation or administrative entity and is shaded according to the average percentage of the global forest coverage found within that area in the last decade.</span>"),
    aes(x, y, label = label),
    inherit.aes = FALSE,
    color = 'black',
    family = 'Raleway',
    box.color = NA,
    fill = NA,
    size = 5,
    halign = 0.5,
    width = grid::unit(0.3, "npc"), 
    height =  grid::unit(0.1, "npc"), 
    hjust = 0,
    vjust = 0) + 
  labs(caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)<br>data from tidyTuesday and Our World In Data") +
  theme(
    text = element_text('Raleway'),
    panel.background = element_rect(color = bg, fill = bg),
    panel.border = element_blank(),
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(c(10, 20 , 10, 20)),
    plot.caption = element_markdown(family = 'Raleway', colour = 'grey50'),
    plot.caption.position = 'plot')

# Legend 
legend <- data.table(
  level = c(NA, sort(z[,unique(bin)])),
  xmin = c(0:6),
  xmax = c(1:7), 
  label = c('Missing', '<0.001%', '0.001 - 0.01%', '0.01 - 0.1%',
            '0.1 - 1%', '1 - 10%', '>10%'),
  loc = c(-0.5, 1.5, -0.5, 1.5, -0.5, 1.5, -0.5))


p.leg <- ggplot(legend) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = level), 
    color = 'grey10',
    size = 0.15) +
  geom_text(
    aes(y = loc, x = xmin + 0.5, label = label),
    vjust = 'middle',
    hjust = 'middle',
    size = 3,
    colour = "black",
    family = "Bebas") +
  scale_fill_manual(values = pal, na.value = '#4B3B40') +
  scale_y_continuous(limits = c(-2,2)) +
  guides(fill = FALSE) +
  theme_void() + 
  theme(plot.margin = margin(c(2, 2 , 2, 2))) +
  coord_equal()


# build plot 
p + inset_element(
  p.leg,
  left = 0.75, 
  bottom = 0.85,
  right = 1,
  top = 1,
  clip = FALSE,
  align_to = "full") +
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 10.5, height = 10.5, device = 'png')







