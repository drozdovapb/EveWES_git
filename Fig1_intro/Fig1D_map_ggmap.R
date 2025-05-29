library(ggmap)
#library(scales)

bbox <- c(left=102, right=112, bottom=51, top=56.2)
BaikalMap <- get_stadiamap(bbox, zoom=9, maptype = "stamen_toner_background")

sampling_points <- data.frame(lat = c(53.374878, 51.870608, 51.870684), 
                              lon = c(108.975189, 104.828101, 104.811648), 
                              col = c("#D81B60", "#F0E442", "#4477AA"),
                              label = c("E", "W", "S"))

ggmap(BaikalMap) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = sampling_points, aes(x = lon, y = lat), col = sampling_points$col, alpha = 0.5, size = 0.5) +
  theme_minimal() # (base_size = 22) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


