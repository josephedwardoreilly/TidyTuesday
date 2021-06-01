require(magick)
require(magrittr)

# Identify all the plots
all.files <- list.files(here::here('plots'), full.names = TRUE)
all.files <- all.files[grepl(pattern = 'png', all.files)]

# read and rescale all in one go
all.res <- sapply(all.files, FUN = function(x){
  image_read(x) %>%
    image_scale(geometry = geometry_size_pixels(500)) %>%
    image_trim()
})

# Build the collage
montage <- image_montage(
  image_join(all.res),
  tile = '4x0',
  bg = 'white',
  geometry = geometry_area(500, 300))

# Add a little border
montage <- image_border(montage, "white", "20x20")

# write to disk
image_write(
  montage,
  path = here::here('plots','collage.png'),
  format = "png")