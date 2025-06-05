library(maptiles)
library(tidyterra)
library(sf)
library(ggplot2)
library(openxlsx)

## imprort settings, mostly to set colors
source("../Fig5_ampl_sexes/settings.R")

#https://stackoverflow.com/questions/77244364/is-ggmap-sf-still-plotting-point-in-wrong-place

bb <- c(left = 103.5, bottom = 51.4, right = 105.6, top = 52.3)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 9, crop = TRUE, 
                          provider = 'Esri.OceanBasemap') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"

ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(height = unit(0.1, "cm")) -> source_map
source_map


WSdistrib <- read.xlsx("WS_distribution_Angara_source.xlsx")
WSdistrib$lat <- as.numeric(sapply(X = WSdistrib$Coordinate, FUN = function(X) {unlist(strsplit(X, split = " "))[1]}))
WSdistrib$lon <- as.numeric(sapply(X = WSdistrib$Coordinate, FUN = function(X) {unlist(strsplit(X, split = " "))[3]}))

WSdistrib_sf <- st_as_sf(WSdistrib, coords = c("lon", "lat"), crs=4326)

source_map + 
  geom_sf(data = WSdistrib_sf) + 
  theme_minimal()
ggsave("map.svg")

#devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)

waffle_data <- waffle_iron(WSdistrib, aes_d(group = Species))

ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle()

library(waffle)
waffle(parts = data.frame(names=c("W", "S"), vals=c(93, 7)), rows = 20, flip = T) + 
  scale_fill_manual(values = c(W, S))  + 
  ggtitle("Li, new")
ggsave("Li_waffle.svg")

waffle(parts = data.frame(names=c("W"), vals=c(11)), rows = 20, flip = T) + 
  scale_fill_manual(values = c(W))  + 
  ggtitle("Li, prev")


source("../Fig3_ampl_sexes/settings.R")
