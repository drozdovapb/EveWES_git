library(rnaturalearth)
library(rnaturalearthdata)
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(dplyr)

# get the data
sampling_points <- data.frame(lat = c(53.374878, 51.870608, 51.870684), 
                              lon = c(108.975189, 104.828101, 104.811648), 
                              col = c("#D81B60", "#F0E442", "#4477AA"),
                              label = c("E", "W", "S"))

russia <- ne_download(scale = 'large', returnclass = "sf", type='lakes', category='physical')
russia2 <- ne_download(scale = 'large', returnclass = "sf", type='rivers_lake_centerlines', category='physical')


#sampling_points %>% group_by(loc_name, coordinate, `Способ.сбора`) %>% summarise() -> sampling_points
#sampling_points$lat <- as.numeric(sapply(strsplit(sampling_points$coordinate, "N, "), "[", 1))
#sampling_points$lon <- as.numeric(gsub(" E", "",sapply(strsplit(sampling_points$coordinate, ", "), "[", 2)))
#names(sampling_points)[3] <- "Способ сбора"

#map <- 
  ggplot(data = russia) +
  geom_sf(color = "blue") +
  geom_sf(data = russia2, colour = "blue", linewidth = 0.2, alpha=0.5) + 
  coord_sf(xlim = c(102.5, 112), ylim = c(51, 56), expand = FALSE) +
  geom_point(data = sampling_points, aes(x = lon, y = lat), size=1.5, fill=col) + #+, 
    theme_minimal() + 
    xlab("") + ylab("") -> base_map

base_map

ggsave("map.png", first_map, width = 7, height=6, device = "png")
ggsave("map_col.png", second_map, width = 7, height=6, device = "png")
ggsave("map_col.svg", second_map, width = 7, height=6, device = "svg")
