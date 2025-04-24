library(ggplot2)
library(terra)
#install.packages("elevatr")
#install.packages("mapview")
library(geodata)
library(predicts)
library(raster)
library(sp)
library(factoextra)
library(sf)
#install.packages("rnaturalearthdata")
library(rnaturalearth)
library(elevatr)
library(rnaturalearthdata)

bioclim_data <- worldclim_global(var = "bio",
                                 res = 2.5,
                                 path = ".")
#habmap = read.csv("Habro map.csv")
amermap = read.csv("amer_color_updating.csv")
summary(amermap)
amermap$sp = as.factor(amermap$sp)
#habmap$species = as.factor(habmap$species)
amermap$red = as.factor(amermap$red)
amermap_all = amermap
amermap = subset(amermap, sp == "ame" | sp == "kub" | sp == "bul" | sp == "san")
amermap_acc = subset(amermap, acc <= 1000 | is.na(acc) == TRUE)
amermap_all_acc = subset(amermap_all, acc <= 1000 | is.na(acc) == TRUE)

#habmap = habmap[!is.na(habmap$Lon), ]
#habmap = habmap[!is.na(habmap$complex_sig), ]
#habmap = habmap[habmap$countryCode!= "KG", ]
#habmap = habmap[habmap$countryCode!= "CL", ]
# Determine geographic extent of our data
max_lat <- ceiling(max(amermap_acc$lat))
min_lat <- floor(min(amermap_acc$lat))
max_lon <- ceiling(max(amermap_acc$lon))
min_lon <- floor(min(amermap_acc$lon))
# Store boundaries in a single extent object
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))
# Download data with geodata's world function to use for our base map
world_map <- world(resolution = 3,
                   path = ".")


cropped_map <- raster::crop(world_map, geographic_extent)
#my_map <- crop(x = world_map, y = geographic_extent)

# Plot the base map
plot(cropped_map,
     axes = TRUE, 
     col = "grey95")

unique_vals <- sort(unique(amermap_acc$red))
# Generate a color palette based on the number of unique values
color_palette <- c("grey30","red")
#habmap = habmap[order(habmap$complex_sig, decreasing = FALSE), ]
# Create a mapping of complex_sig values to colors
col_map <- color_palette[as.numeric(as.factor(amermap_acc$red))]
col_map <- adjustcolor(col_map, alpha.f = 0.5)

points(x = amermap_acc$lon, 
       y = amermap_acc$lat, 
       col = col_map,
       pch = 20,
       cex = 2)

legend("left",                     # position of the legend
       legend = unique_vals,           # unique values
       fill = color_palette,           # colors
       title = "Complex Signal Levels",
       cex = 0.8,
       xpd = TRUE)

### try topography map----
prj4<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# set plot bounds
bounds <- data.frame(x=c(-135,-90),y=c(30,65))

# retrieve elevation raster
elev<-get_elev_raster(locations = bounds, 
                      prj = prj4,
                      z=4, 
                      clip="locations")
elev[elev < -100] <- -100
grey_colors <- gray.colors(100, start = 0, end = 1)

# Plot the elevation raster using the grayscale palette
plot(elev, col = grey_colors)


# Add data points
dotcoords<-data.frame(x = amermap_acc$lon, y = amermap_acc$lat)
#rownames(dotcoords)<-c("locA","locB")
coordinates(dotcoords) <- c("x","y")
proj4string(dotcoords) <- CRS("+proj=longlat +datum=WGS84") 
pts_dotcoords <- spTransform(dotcoords, CRS(prj4))
plot(pts_dotcoords, add=T, col = col_map,pch=16)
# add elevation to dataset
coordinates_sf <- st_as_sf(amermap_acc, coords = c("lon", "lat"), crs = 4326)
elevation_data <- get_elev_point(locations = coordinates_sf, prj = "EPSG:4326", src = "aws")
amermap_acc = elevation_data
### Try extract environment measurements from coordinates
#install.packages("raster")
library(raster)
library(geodata)

# link to see each biox means:
#https://www.worldclim.org/data/bioclim.html#:~:text=They%20are%20coded%20as%20follows,Seasonality%20(standard%20deviation%20%C3%97100)
# Assuming `amermap_acc` is already defined with `lon` and `lat` columns
coordinates_sf <- st_as_sf(amermap_acc, coords = c("lon", "lat"), crs = 4326)

# Get bioclimatic data using geodata package
bio_data <- worldclim_global(var = 'bio', res = 10, path = tempdir())

# Convert the bioclimatic data to a raster stack
bio_stack <- stack(bio_data)  # Stack all bioclimatic variables

# Extract the bioclimatic data for the coordinates
coordinates <- st_coordinates(coordinates_sf)
bio_values <- raster::extract(bio_stack, coordinates)

# Convert the extracted values to a data frame
bio_values_df <- as.data.frame(bio_values)

# Add the extracted values back to the original data
amermap_acc <- cbind(amermap_acc, bio_values_df)
amermap_acc = subset(amermap_acc, is.na(red) == FALSE)
summary(amermap_acc)
notarmap = subset(amermap_acc, sp == "ame"| sp == "san")

ggplot(data = notarmap, aes (x = sp, y = wc2.1_10m_bio_15,
                             fill = red))+
  geom_boxplot(alpha = .7)+theme_classic()
graph2ppt(file = "amer map bio15.pptx",width = 7,height = 5)
ggplot(data = notarmap, aes (x = sp, y = pr,
                            fill = red))+
  geom_boxplot(alpha = .7)+theme_classic()
ggplot(data = notarmap, aes (x = sp, y = elevation,
                             fill = red))+
  geom_boxplot(alpha = .7)+
  theme_classic()
graph2ppt(file = "amer map elev.pptx",width = 7,height = 5)
ggplot(data = notarmap, aes (x = sp, y = tmmx,
                             fill = red))+
  geom_boxplot(alpha = .7)+
  theme_classic()
graph2ppt(file = "amer map tmmx.pptx",width = 7,height = 5)

#link for what each land cover means
#https://medium.com/@sam.21/computing-lulc-zonal-statistics-using-modis-on-google-earth-engine-7b97a408cf24
ggplot(data = amermap_acc, aes (x = red, y = land_cover,
                             fill = red))+geom_jitter()+
  geom_boxplot(alpha = .7)+theme_classic()
ggplot(data = amermap_acc, aes (x = red, y = tmmx,
                            fill = red))+geom_jitter()+
  geom_boxplot(alpha = .7)+theme_classic()
ggplot(data = notarmap, aes (x = red, y = pr,
                            fill = red))+geom_jitter()+
  geom_boxplot(alpha = .7)+theme_classic()
ggplot(data = notarmap, aes (x = red, y = vpd,
                            fill = red))+geom_jitter()+
  geom_boxplot(alpha = .7)+theme_classic()
#PCA
amermap_pca = na.omit(as.data.frame(amermap_acc[,c(4,10:14,18,20:38)])[,1:26])
summary(amermap_pca)
pca_map = prcomp(amermap_pca[,-1],center = T, scale = T)
summary(pca_map)
fviz_eig(pca_map, addlabels = F)+theme_classic()+labs(title = "",y = "",x = "")
corrplot::corrplot(pca_map$rotation,is.corr = FALSE)
pca_plot = pca_map$x
pca_plot = data.frame(pca_plot)
pca_plot$red = amermap_pca$red
#pca_plot$complex = habmap_pca$complex_sig
ggplot(data = pca_plot, aes(x = PC1, y = PC2,color = as.factor(red)))+
 theme_classic()+geom_point()+stat_ellipse(aes(group=as.factor(red)))#

ggplot(data = pca_plot, aes(x = red, y = PC6,fill = red))+
  theme_classic()+geom_boxplot(alpha = .5)

#PCA no tar
notarmap_pca = notarmap[!is.na(notarmap$pr),]
summary(notarmap_pca)
pca_map = prcomp(notarmap_pca[,10:32],center = T, scale = T)
summary(pca_map)
fviz_eig(pca_map, addlabels = F)+theme_classic()+labs(title = "",y = "",x = "")
corrplot::corrplot(pca_map$rotation,is.corr = FALSE)
pca_plot = pca_map$x
pca_plot = data.frame(pca_plot)
pca_plot$red = notarmap_pca$red

ggplot(data = pca_plot, aes(x = red, y = PC1,fill = red))+
  theme_classic()+geom_boxplot(alpha = .5)



#NMDS
library(vegan)
NMDS=metaMDS(amermap_pca[,-1],k=2,trymax=100)
plot(NMDS)
#dim(pca_plot_gg)
NMDS_plot = as.data.frame(NMDS$points) 
NMDS_plot$red = as.factor(amermap_pca$red)
#NMDS_plot$sp = habmap_pca$species
ggplot(data = NMDS_plot,aes(x = MDS1,y = MDS2,color = red))+
  geom_point()+theme_classic()+stat_ellipse(aes(group=red))



#random forest
library(randomForest)
library(datasets)
#install.packages("caret")
library(caret)
set.seed(NULL)
summary(amermap_pca)
ind <- sample(2, nrow(amermap_pca), replace = TRUE, prob = c(0.7, 0.3))
train <- droplevels(amermap_pca)
test <- droplevels(amermap_pca)
rf <- randomForest(red~., data=train,  proximity=TRUE,
                   importance = T,ntree = 5000)
rf#8.27%

p2 <- predict(rf, test)
p2_matrix = confusionMatrix(p2, test$red)#test data accuracy 1
mat_tab = as.data.frame(p2_matrix$table)
ggplot(data =  mat_tab, mapping = aes(x = Prediction, y = Reference,fill = Freq)) +
  geom_tile()+geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194")+theme_classic()

plot(rf)
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")#bio15(var in pr), elevation, pr

#permutation
nPermute <- 1000
resRFPermute_all <- matrix(0,nrow=nPermute,ncol=4)
resRFPermute <- array(0, dim=nPermute)

for (i in 1:nPermute) {
  ind <- sample(2, nrow(amermap_pca), replace = TRUE, prob = c(0.7, 0.3))
  train <- droplevels(amermap_pca[ind==1,])
  rf_all_amermap <- randomForest(red~., data=train,  proximity=TRUE,
                                 importance = T,ntree = 5000)
  test_all = varImpPlot(rf_all_amermap,
                        sort = T,
                        n.var = 7)
  resRFPermute_all[i,1] = rf_all_amermap$err.rate[5000,1]
  resRFPermute_all[i,2] = names(sort(test_all[,1],decreasing = T)[1])
  resRFPermute_all[i,3] = names(sort(test_all[,1],decreasing = T)[2])
  resRFPermute_all[i,4] = names(sort(test_all[,1],decreasing = T)[3])
}

resRFPermute_all = as.data.frame(resRFPermute_all)
resRFPermute_all$V1 = as.numeric(resRFPermute_all$V1)
resRFPermute_all$V2 = as.factor(resRFPermute_all$V2)
resRFPermute_all$V3 = as.factor(resRFPermute_all$V3)
resRFPermute_all$V4 = as.factor(resRFPermute_all$V4)
summary(resRFPermute_all)
permut_amermap = resRFPermute_all
library(tidyr)
library(dplyr)

# Assuming your dataframe is named 'df'

# Reshape the dataframe from wide to long format
df_long <-permut_amermap %>%
  pivot_longer(cols = c("V2","V3","V4"), names_to = "rank", values_to = "importance")

# Add a new column for 'Rank' based on the original column names
df_long <- df_long %>%
  mutate(rank = factor(rank, levels = c("V2", "V3", "V4")))

ggplot(data=df_long, aes(x=rank, fill = importance)) +
  geom_bar(position = "fill",color = "black")+theme_classic()+
  scale_fill_viridis(discrete = T,direction = -1)+
  geom_text(stat='count', aes(label=..count..), position = "fill",vjust = -0.5)
graph2ppt(file = "amer map rf importance.pptx",width = 7,height = 5)
df_long$Index <- seq_len(nrow(df_long))
ggplot(data = df_long, aes(x = Index,y = V1))+
  geom_point(color = "grey")+
  geom_hline(yintercept = mean(df_long$V1),color = "red")+
  theme_classic()
graph2ppt(file = "amer map rf error.pptx",width = 7,height = 5)
#### try plot map with state margin----
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggspatial)
library(elevatr)
library(raster)
amermap_red = subset(amermap, is.na(red) == FALSE)
amermap_red_as = subset(amermap_red, sp == "ame" | sp == "san")
bbox = raster::extent(-140,-90,30,65)
bbox_sf = st_as_sf(as(bbox,"SpatialPolygons"))
st_crs(bbox_sf) = 4326

elevation_data = get_elev_raster(locations = bbox_sf, z = 6, clip = "bbox")
elevation_df = as.data.frame(rasterToPoints(elevation_data),xy = TRUE)
summary(elevation_df)
elevation_df$file41d2c6970c9c0[elevation_df$file41d2c6970c9c0 <= 0] <- NA
states = ne_states(country = "United States of America",returnclass = "sf")
canada = ne_states(country = "Canada",returnclass = "sf")
country = rbind(states, canada)
country_cropped <- st_crop(country, xmin = -140, xmax = -90, ymin = 30, ymax = 65)
points = st_as_sf(amermap_red_as, coords = c("lon", "lat"),crs = 4326)
emphasized_indices <- c(511, 621)  # Replace with your actual indices

points$emphasize <- ifelse(points$ID %in% emphasized_indices, TRUE, FALSE)
gg = ggplot()+
  geom_raster(data = elevation_df,aes(x = x, y = y, fill = file41d2c6970c9c0),alpha = .7)+
  scale_fill_gradient(low = "white", high = "black", na.value = "white") +
  geom_sf(data = subset(points, !emphasize), aes(color = as.factor(red), shape = sp), size = 2,alpha = .7) +
  geom_sf(data = subset(points, emphasize), aes(color = as.factor(red)), shape = "\u2605", size = 5, show.legend = FALSE) +
  geom_sf(data = country_cropped, fill = NA, color = "black",size = 0.5)+
  geom_sf(data = subset(points, emphasize), color = "black", shape = "\u2606", size = 5, show.legend = FALSE) +
  scale_color_manual(values = c("0" = "black","1" = "red"))+
  coord_sf(xlim = c(-135,-90),ylim = c(30,65))+
  theme_minimal()
plot_gg(gg, width = 5, height = 5, scale = 300, multicore = TRUE, windowsize = c(800, 800), zoom = 0.75, phi = 30, theta = 30)
library(viridis)
gg <- ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = file41d2c6970c9c0), alpha = .7) +
  scale_fill_viridis(option = "C", na.value = "white") + 
  geom_sf(data = subset(points, !emphasize), aes(color = as.factor(red), shape = sp), size = 2,alpha = .7) +
  geom_sf(data = subset(points, emphasize), aes(color = as.factor(red)), shape = "\u2605", size = 5, show.legend = FALSE) +
  geom_sf(data = subset(points, emphasize), color = "black", shape = "\u2606", size = 5, show.legend = FALSE) +
  scale_color_manual(values = c("0" = "black","1" = "red"))+
  coord_sf(xlim = c(-135, -90), ylim = c(30, 65)) +
  theme_minimal()

# Render the simple plot using rayshader
plot_gg(gg, width = 5, height = 3, scale = 300, multicore = TRUE, windowsize = c(800, 800), zoom = 0.75, phi = 30, theta = 30)



### all amer and tar group ----
max_lat <- ceiling(max(amermap_all_acc$lat))
min_lat <- floor(min(amermap_all_acc$lat))
max_lon <- ceiling(max(amermap_all_acc$lon))
min_lon <- floor(min(amermap_all_acc$lon))
# Store boundaries in a single extent object
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))
# Download data with geodata's world function to use for our base map
world_map <- world(resolution = 3,
                   path = ".")


cropped_map <- raster::crop(world_map, geographic_extent)
#my_map <- crop(x = world_map, y = geographic_extent)

# Plot the base map
plot(cropped_map,
     axes = TRUE, 
     col = "grey95")

unique_vals <- sort(unique(amermap_all_acc$red))
# Generate a color palette based on the number of unique values
color_palette <- c("grey30","red")
#habmap = habmap[order(habmap$complex_sig, decreasing = FALSE), ]
# Create a mapping of complex_sig values to colors
col_map <- color_palette[as.numeric(as.factor(amermap_all_acc$red))]
col_map <- adjustcolor(col_map, alpha.f = 0.5)
points(x = jitter(habmap$Lon,amount = 0.5), 
       y = jitter(habmap$Lat,amount = 0.5), 
       col = col_map,
       pch = 20,
       cex = 1)

points(x = amermap_all_acc$lon, 
       y = amermap_all_acc$lat, 
       col = col_map,
       pch = 20,
       cex = 2)

legend("left",                     # position of the legend
       legend = unique_vals,           # unique values
       fill = color_palette,           # colors
       title = "Complex Signal Levels",
       cex = 0.8,
       xpd = TRUE)

### try plot everyone----
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggspatial)
library(elevatr)
library(raster)
amermap_red = subset(amermap_all, is.na(red) == FALSE)
amermap_red = subset(amermap_red, sp == "ame" | sp == "san" | sp == "tar" | sp == "oph" | sp == "mus" | sp == "kub")
amermap_red_as = subset(amermap_red, sp == "ame" | sp == "san")
bbox = raster::extent(-140,-90,30,65)
bbox_sf = st_as_sf(as(bbox,"SpatialPolygons"))
st_crs(bbox_sf) = 4326

elevation_data = get_elev_raster(locations = bbox_sf, z = 6, clip = "bbox")
elevation_df = as.data.frame(rasterToPoints(elevation_data),xy = TRUE)
summary(elevation_df)
elevation_df$file2033938562198[elevation_df$file2033938562198 <= 0] <- NA
states = ne_states(country = "United States of America",returnclass = "sf")
canada = ne_states(country = "Canada",returnclass = "sf")
country = rbind(states, canada)
country_cropped <- st_crop(country, xmin = -140, xmax = -90, ymin = 30, ymax = 65)
points = st_as_sf(amermap_red, coords = c("lon", "lat"),crs = 4326)
emphasized_indices <- c(511, 621)  # Replace with your actual indices

points$emphasize <- ifelse(points$ID %in% emphasized_indices, TRUE, FALSE)
gg = ggplot()+
  geom_raster(data = elevation_df,aes(x = x, y = y, fill = file2033938562198),alpha = .7)+
  scale_fill_gradientn(colors = viridis::viridis(10), na.value = "white") +
  geom_sf(data = country_cropped, fill = NA, color = "black",size = 0.5)+
  geom_sf(data = subset(points, !emphasize), aes(color = as.factor(red), shape = sp), size = 2,alpha = .7) +
  scale_color_manual(values = c("1" = "red","0" = "black"))+
  coord_sf(xlim = c(-135,-90),ylim = c(30,65))+
  theme_minimal()
ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = file2033938562198), alpha = 0.7) +
  scale_fill_gradientn(colors = terrain.colors(10), na.value = "white") +
  geom_sf(data = country_cropped, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = subset(points, !emphasize & red == 1), aes(shape = sp), color = "red", size = 2, alpha = 0.7) +
  geom_sf(data = subset(points, !emphasize & red == 0), aes(shape = sp), color = "black", size = 2, alpha = 0.7) +
  coord_sf(xlim = c(-135, -90), ylim = c(30, 65)) +
  theme_minimal()
plot_gg(gg, width = 5, height = 5, scale = 300, multicore = TRUE, windowsize = c(800, 800), zoom = 0.75, phi = 30, theta = 30)

ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = file2033938562198), alpha = .7) +
  scale_fill_gradientn(colors = terrain.colors(10), na.value = "white") +
  geom_sf(data = country_cropped, fill = NA, color = "black", size = 0.3) +
  geom_sf(data = subset(points, !emphasize & red == 1), aes(shape = sp), color = "red", size = 2, alpha = 0.5) +
  geom_sf(data = subset(points, !emphasize & red == 0), aes(shape = sp), color = "black", size = 2, alpha = 0.7) +
  geom_sf(data = subset(points, emphasize), aes(color = as.factor(red)), shape = "\u2605", size = 5, show.legend = FALSE) +
  geom_sf(data = subset(points, emphasize), color = "black", shape = "\u2606", size = 5, show.legend = FALSE) +
  scale_color_manual(values = c("0" = "black","1" = "red"))+
  coord_sf(xlim = c(-135, -90), ylim = c(30, 65)) +
  theme_minimal()
summary(elevation_df)
