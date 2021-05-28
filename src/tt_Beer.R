library(tidytuesdayR)
library(ggplot2)
library(data.table)
library(ggtext)
library(geofacet)

# Data Prep ---------------------------------------------------------------
# Read the data
tidy.week <- '2020-10-20'
tt_data <- tt_load(tidy.week) 
x <- data.table(tt_data$beer_awards)

z <- x[year>  2010,  .(state, medal)]
z[, state := toupper(state)] # fix some case issues
# Order medals so gold is on the top
z[, medal.int := 3L]
z[medal == 'Silver', medal.int := 2L]
z[medal == 'Bronze', medal.int := 1L]

setkey(z, state, medal.int)

# Define plotting params
ncol <- nrow <- 24
# Build x,y data for each medal
z[, x := rep_len(1:nrow, .N), by = state]
z[, y := rep(1:ceiling(.N/nrow), each = nrow, len = .N), by = state]



# Plotting ----------------------------------------------------------------
pal <- c('Bronze' = '#782408', 'Silver' = '#D3CBC5', 'Gold' = '#F8A312')
bg <- '#ffe1a8'
bg.2 <- '#fffcf2'

ggplot(z, aes(x = x, y= y, fill = medal)) + 
  geom_tile(color = '#fffcf2') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = pal) + 
  facet_geo(~ state, grid = "us_state_grid2") + 
  theme(legend.position = 'none',
        panel.background = element_rect(color = NA, fill = bg),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks =  element_blank(),
        text = element_text('Helvetica Neue'),
        plot.background = element_rect(fill = bg.2, color = bg.2),
        plot.margin = margin(c(20, 40 , 20, 40)),
        plot.caption = element_markdown(family = 'Helvetica Neue', colour = 'Black'),
        plot.title = element_markdown(
          family = 'Helvetica Neue',
          colour = 'Black',
          hjust = 0.5,
          size = 25,
          margin = margin(20, 20, 10, 20)),
        plot.subtitle = element_textbox(
          family = 'Helvetica Neue',
          colour = 'grey10',
          size = 20,
          fill = bg.2,
          box.colour = NA,
          width = grid::unit(0.75, "npc"),
          hjust = 0.5,
          margin = margin(1, 20, 50, 20)),
        strip.text = element_text(
          size = 15,
          color = 'Black',
          'Helvetica Neue', 
          hjust = 0, 
          margin = margin(c(1,1,1,1))),
        strip.background = element_rect(color = NA, fill = bg)) +
  labs(title = '**Which State Has Produced The Most Prize Winning Beers In The Last Decade?**',
       subtitle = "In the last ten years (2010-2020) which US state has received the most medals at the Great American Beer Festival for beers that its breweries have produced? Each tile represents a single medal awarded to a beer from a given state, and the tile colour reflects the type of medal awarded (i.e. <span style = 'color:#F8A312;'>**gold**</span>, <span style = 'color:#D3CBC5;'>**silver**</span>, or <span style = 'color:#782408;'>**bronze**</span>).",
       caption = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly)<br>Data from TidyTuesday and Great American Beer Festival") +
  ggsave(
    filename = here::here('plots', paste0(tidy.week, '.png')),
    width = 16, height = 10.5, device = 'png')


# geofacet breaks when using patchwork, using grobs might fix this...
# g <- ggplot_gtable(ggplot_build(pm))
# pat <- data.table(geofacet::us_state_grid2)[, paste0('panel-', col, '-', row)]
# pat <- setdiff(g$layout$name[grep(pattern = 'panel', g$layout$name)], pat)
# stripr <-  which(g$layout$name %in% pat)
# for (i in stripr) {
#   j <- which(grepl('grill', g$grobs[[i]]$children))
#   k <- which(grepl('rect', g$grobs[[i]]$children[[j]]$children))
#   g$grobs[[i]]$children[[j]]$children[[k]]$gp$fill <- 'red'
# }
# grid.draw(g)



