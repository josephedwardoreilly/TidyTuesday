library(data.table)
library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(patchwork)
library(plotwidgets)

# Function to convert hsl data to a hex code
hsl2hex <- function(h, s, l){
  hsl <- rbind(h, s, l)
  hsl <- hsl2rgb(hsl)
  hex <- rgb(hsl[1,], hsl[2,], hsl[3,], maxColorValue = 255)
  return(hex)
}

# Load the data
tidy.week <- '2021-03-30'
tuesdata <- tidytuesdayR::tt_load(tidy.week)

x <- tuesdata$allShades
setDT(x)
# keep only what we need
x <- x[, .(brand, hex, lightness, hue, sat)]

### Build the data.table to plot the per brand distribution of shades
# Get the mean lightness across the whole dataset 
m.lightness <- x[, mean(lightness)]

# Pick the 10 brand with the most shades
top.ten <- x[brand %in% x[, .N, by = brand][order(N, decreasing = TRUE)][1:10, brand]]

# bin all data into lightness bins of 0.1 intervals
bins <- seq(0, 1, by = 1/10)
top.ten[, bin := .bincode(lightness, bins)]

# Build a y axis for the bins (darkest at the bottom, lightest at top)
setkey(top.ten, lightness) # order on lightness

top.ten[, ord := rowid(bin, brand)]
top.ten[, ID := factor(c(1L:.N))] # each product gets an ID

# order the brands to plot in the right order when faceted 
top.ten[, brand := factor(brand, top.ten[, mean(lightness), by = brand][order(V1), brand])]

### Build the data.tables to plot sampled observed/expected shade distribution
all.x <- x[,.(hex, hue, sat, lightness)]
all.x[, bin := .bincode(lightness, bins)]
all.x[, ID := as.character(c(1L:.N))] # each product gets an ID
dist.sample <- 
  all.x[
    all.x[,
          .(as.integer(sample(ID, size = 1000, replace = TRUE)))][, V1],]
# add axis information
setkey(dist.sample, lightness)

dist.sample[, x.axs := rep_len(1:10, nrow(dist.sample))]
dist.sample[, y.axs := sort(rep(1:100, 10))]

## Sample of 1000 shades from expected uniform distribution
# Add some bin 1 shades that should exist but don't
theoretical.x <- rbind(
  all.x,
  all.x[bin == 2][, `:=`(bin = 1,
                         lightness = lightness - 0.1,
                         hex = hsl2hex(hue, sat, lightness))])
theoretical.x[, ID := as.character(c(1L:.N))] # each product gets an ID

# Sample 100 from each bin
ideal.sample <- 
  theoretical.x[
    theoretical.x[,
                  .(as.integer(sample(ID, size = 100, replace = TRUE))), by = bin][, V1],]
# add axis information
setkey(ideal.sample, lightness)
ideal.sample[, x.axs := rep_len(1:(length(unique(ideal.sample$bin))), nrow(ideal.sample))]
ideal.sample[, y.axs := sort(rep(1:100, 10))]

# Plotting ----------------------------------------------------------------
bg <- '#ededf5'

theme_set(
  theme_minimal(base_size = 15, 
                base_family = 'KiwiMaru-Regular'
  ))

theme_update(
  panel.background = element_rect(color = bg, fill = bg),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.spacing = unit(0.5, "lines"),
  panel.border = element_blank(),
  axis.line.x = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks =  element_blank(),
  axis.ticks.length = unit(.06, "lines"),
  legend.position = "none",
  plot.title = element_text(hjust = 1, color = "grey30", 
                            size = 10, margin = margin(t = 10, b = 10)),
  plot.subtitle = element_text(hjust = 0, face = "bold", color = "grey30",
                               size = 14, margin = margin(0, 0, 25, 0)),
  plot.title.position = "plot",
  plot.caption = element_text(color = "black", size = 10, hjust = 1,
                              lineheight = 1.05, margin = margin(30, 0, 0, 0)),
  plot.caption.position = "plot", 
  plot.background = element_rect(fill = bg, color = bg),
  plot.margin = margin(c(10, 5 , 10, 10)),
  strip.text = element_textbox_simple(
    size = 8,
    halign = 0.5,
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    margin = margin(0, 0, 5.5, 0),
    fill = NA
  )
)

# Plot of the top ten brands, ordered by distribution of lightness
p.top.ten <- ggplot(top.ten, aes(x = bin, y = ord))+
  geom_tile(aes(fill = ID), height = 1, width = 1, color = 'black')+
  scale_fill_manual(values = top.ten$hex) + 
  scale_x_continuous(breaks = c(1, 5, 10), limits = c(1,10))+
  theme(legend.position = 'none',
        plot.title = element_text(color = 'grey30')) + 
  facet_wrap(.~brand, nrow = 1, strip.position = 'bottom') +
  ggtitle('(a) distribution of shades sold by the 10 largest brands') + 
  coord_cartesian(expand = FALSE)


# Plot a sample of the observed distribution of shades
p.obs <- ggplot(dist.sample, aes(x = x.axs, y = y.axs, fill = ID))+
  geom_tile(aes(fill = ID), color = 'black')+
  scale_fill_manual(values = setNames(dist.sample$hex, dist.sample$ID)) + 
  theme(legend.position = 'none',
        plot.title = element_text(color = 'grey30')) + 
  ggtitle('(b) observed distribution of shades') + 
  coord_cartesian(expand = FALSE)

# Plot a sample of the expected distribution of shades
p.expect <- ggplot(ideal.sample, aes(x = x.axs, y = y.axs, fill = ID))+
  geom_tile(aes(fill = ID), color = 'black')+
  scale_fill_manual(values = setNames(ideal.sample$hex, ideal.sample$ID)) + 
  theme(legend.position = 'none',
        plot.title = element_text(color = 'grey30')) + 
  ggtitle('(c) expected distribution of shades') + 
  coord_cartesian(expand = FALSE)


# Plot the title and body text 
pt.body <- ggplot() + 
  # Text body
  geom_textbox(
    data = data.frame(
      x = 0.1,
      y = 0.80,
      label = 'The distribution of the lightness of foundation shades listed on Sephora and Ulta is not uniform. Plot (a) shows the colour distribution of foundation shades from the 10 largest brands, as measured by the number of unique products the brand produces. Each tile represents a single foundation and its associated shade. For each brand, foundation shades are placed into 10 bins of equal width based on the lightness of the shade. Brands are presented in order of how biased towards lighter shades their range is - from least biased on the left, to most biased on the right. 
  
  Plot (b) shows a random sample of 1000 foundation shades from the observed distribution of foundation shades, whereas plot (c) shows a sample of 1000 foundation shades obtained from a uniform distribution of shades. It is clear that the observed distribution of shades is biased towards lighter shades, with deeper shades being under-represented.'),
    aes(x, y, label = label),
    color = 'grey30',
    box.color = NA,
    fill = NA,
    family = 'KiwiMaru-Regular',
    size = 4.5,
    width = grid::unit(0.73, "npc"), # 73% of plot panel width
    hjust = 0, vjust = 1
  ) +
  # Text title
  geom_richtext(
    data = data.frame(
      x = 0.5,
      y = 0.95,
      label = 'Bias Towards Lighter Tones <br> In Foundation Shades'),
    aes(x, y, label = label),
    color = 'black',
    label.color = NA,
    family = 'KiwiMaru-Regular',
    fill = NA,
    size = 7,
    hjust = 0.5, vjust = 0.5
  ) +
  # Plot caption
  geom_richtext(
    data = data.frame(
      x = 0.5,
      y = 0.01,
      label = "Visualisation by Joe O'Reilly (github.com/josephedwardoreilly) <br> data from tidyTuesday, ThePudding, Offune Amaka, and Amber Thomas"),
    aes(x, y, label = label),
    color = 'grey50',
    label.color = NA,
    family = 'KiwiMaru-Regular',
    fill = NA,
    size = 3,
    hjust = 0.5, vjust = 0.5
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme( #  stop the plot elements being rendered
    axis.line=element_blank(),
    axis.line.x = element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.caption = element_text(hjust = 0.5, colour = 'grey30'),
    plot.caption.position =  "plot",
    plot.margin = margin(0),
  )


# Arrange final plot ------------------------------------------------------
# patchwork used to arrange individual elements
pt.body +
  p.top.ten +
  p.obs +
  p.expect +
  plot_layout(
    design = "122
              134") +
  ggsave(
    filename = paste0(getwd(), '/plots/', tidy.week, '.png'),
    width = 16, height = 10.5, device = 'png')





