
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(elevatr)
library(raster)
library(ggplot2)

amermap = read.csv("amer_color_updating.csv")
summary(amermap)
amermap$sp = as.factor(amermap$sp)
amermap$red = as.factor(amermap$red)
amermap_red = subset(amermap, is.na(red) == FALSE)
amermap_red_as = subset(amermap_red, sp == "ame" | sp == "san")
bbox = raster::extent(-140,-90,30,65)
bbox_sf = st_as_sf(as(bbox,"SpatialPolygons"))
st_crs(bbox_sf) = 4326

elevation_data = get_elev_raster(locations = bbox_sf, z = 6, clip = "bbox")
elevation_df = as.data.frame(rasterToPoints(elevation_data),xy = TRUE)
summary(elevation_df)
#have to replace the filexxx in later code from the output of summary()
elevation_df$file12c1c9597768f[elevation_df$file12c1c9597768f <= 0] <- NA
states = ne_states(country = "United States of America",returnclass = "sf")
canada = ne_states(country = "Canada",returnclass = "sf")
country = rbind(states, canada)
country_cropped <- st_crop(country, xmin = -140, xmax = -90, ymin = 30, ymax = 65)
points = st_as_sf(amermap_red_as, coords = c("lon", "lat"),crs = 4326)
emphasized_indices <- c(511, 621)  # location of populations we used

points$emphasize <- ifelse(points$ID %in% emphasized_indices, TRUE, FALSE)
ggplot()+
  geom_raster(data = elevation_df,aes(x = x, y = y, fill = file12c1c9597768f),alpha = .7)+
  scale_fill_gradient(low = "white", high = "black", na.value = "white") +
  geom_sf(data = subset(points, !emphasize), aes(color = as.factor(red), shape = sp), size = 2,alpha = .7) +
  geom_sf(data = subset(points, emphasize), aes(color = as.factor(red)), shape = "\u2605", size = 5, show.legend = FALSE) +
  geom_sf(data = country_cropped, fill = NA, color = "black",size = 0.5)+
  geom_sf(data = subset(points, emphasize), color = "black", shape = "\u2606", size = 5, show.legend = FALSE) +
  scale_color_manual(values = c("0" = "black","1" = "red"))+
  coord_sf(xlim = c(-135,-90),ylim = c(30,65))+
  theme_minimal()


