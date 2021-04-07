require(tidytuesdayR)
require(ggplot2)
require(data.table)

tidy.week <- '2021-04-06'
tt_data <- tt_load(tidy.week) 

x <- data.table(tt_data$forest_area)



wt <- data.table(geofacet::world_countries_grid1)


# get the avergae covergae in last decade 
x <- x[year %in% 2010:2020,
       .(forest_area = mean(forest_area, na.rm = TRUE)), by = .(code)]


z <- merge(x, 
      wt, 
      by.x = 'code',
      by.y = 'code_alpha3',
      all.y = TRUE)


pal.gen <- colorRampPalette(c("#DBDFAC", "#114A50"))
pal <- setNames(pal.gen(6), as.character(1:6))

bins <- c(0, 0.001, 0.01, 0.1,
          1, 10, 100)
z[, bin := as.character(.bincode(forest_area, bins))]


bg = '#BAD7F2'

ggplot(z, aes(x = col, y = -row, fill = bin))+
  geom_tile(color = 'grey10') +
  coord_equal() +
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
         fill = guide_legend(
           title = 'Global Forest Cover',
           label.position = "top", nrow = 1, ncol = 7,
           label.hjust = 0.5))+
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0,
      y = -21,
      label = "**National Share Of Total Global Forest Coverage**<br> 
      <span style = 'font-size:10pt'> Each square represents a single nation, and is shaded according to the percentage of the global forest coverage found within that country.</span>"),
  aes(x, y, label = label), inherit.aes = FALSE,
  color = 'black',
  family = 'Raleway',
  box.color = NA,
  fill = NA,
  size = 5,
  halign = 0.5,
  width = grid::unit(0.3, "npc"), 
  height =  grid::unit(0.1, "npc"), 
  hjust = 0, vjust = 0
  ) + 
  theme(
    text = element_text('Raleway'),
    panel.background = element_rect(color = bg, fill = bg),
    panel.border = element_blank(),
    legend.position="top",
    legend.justification='right',
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(c(10, 5 , 10, 10)))