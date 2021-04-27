########################################################################
# Title: Understanding Rural Settlement in Rwanda
# Author: Joy-Giovanni Matabishi
# Course: MB2- Introduction to Programming and Geostatistics.
# Affiliation: EAGLE master program, University of Würzburg
# April 2021
########################################################################

#******************************************#
#* 1. Loading Data and Preparation.
#******************************************#

# Import necessary packages
library(tidyverse)
library(sf)
library(raster)
library(rnaturalearth)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(ggplot2)
library(scales)

# custom functions:
rowMin <- function(x) {
  as.vector(apply(x, 1, min))
}

#Import shapefiles
rwa <- read_sf("./data/Country.shp")  #country boundary
rwa_grids <-
  read_sf("./data/sample_grids.shp") #88 (5x5) km sample grids
houses <- read_sf("./data/Houses.shp") #172893 houses
lakes <- read_sf("./data/Lakes.shp") # lakes in Rwanda
roads <- read_sf("./data/Main_Roads.shp") # Main roads
hospitals <- read_sf("./data/Hospitals.shp") # Hospitals

#create output files
lakes_out <- "./output/distance_to_lakes.tif"
roads_out <- "./output/distance_to_main_roads.tif"
hospitals_out <- "./output/distance_to_hospitals.tif"

# set area of interest (Country boundary)
aoi <- st_bbox(rwa) %>% st_as_sfc()

# create fishnet grid with 1 km spacing
grid <- st_make_grid(aoi, cellsize = 1000, what = "centers")
##exclude areas inside lakes from the fishnet
no_lake <- grid[lengths(st_within(grid, lakes)) == 0, ]

#convert lakes and main roads to "multilinestring"
lakes_line <- st_cast(lakes, "MULTILINESTRING")
roads_line <- st_cast(roads, "MULTILINESTRING")


#******************************************#
#* 2. Distance Calculation.
#******************************************#

#Calculate distance
lakes_dist <- st_distance(no_lake, lakes_line)
roads_dist <- st_distance(no_lake, roads_line)
hospi_dist <- st_distance(no_lake, hospitals)


# create dataframe with coords and distances to shapefiles 
# (swapped the column order to assign values for loop)
# Then add a column with minimum distance to any object.

#df1
df1 <- data.frame(st_coordinates(no_lake))
for (i in 1:ncol(lakes_dist)) {
  df1[, i + 2] <- as.vector(lakes_dist[, i]) / 1000
}

min_dist_lakes <- rowMin(df1[, c(3:(ncol(lakes_dist) + 2))])
df1$mindist_lakes <- min_dist_lakes

#df2
df2 <- data.frame(st_coordinates(no_lake))
for (i in 1:ncol(roads_dist)) {
  df2[, i + 2] <- as.vector(roads_dist[, i]) / 1000
}

min_dist_roads <- rowMin(df2[, c(3:(ncol(roads_dist) + 2))])
df2$mindist_roads <- min_dist_roads

#df3
df3 <- data.frame(st_coordinates(no_lake))
for (i in 1:ncol(hospi_dist)) {
  df3[, i + 2] <- as.vector(hospi_dist[, i]) / 1000
}

min_dist_hospi <- rowMin(df3[, c(3:(ncol(hospi_dist) + 2))])
df3$mindist_hospi <- min_dist_hospi

#******************************************#
#* 3. Rasterize distance Results.
#******************************************#
## extent
aoi_sp <- as_Spatial(aoi)
ext <- extent(aoi_sp)

## raster
r <- raster()
extent(r) <- ext
crs(r) <- crs(aoi_sp)
res(r) <- 1000

## convert the points to a spatial object class sf
dist_sf1 <- st_as_sf(df1, coords = c("X", "Y")) %>%
  st_set_crs(st_crs(aoi))

dist_sf2 <- st_as_sf(df2, coords = c("X", "Y")) %>%
  st_set_crs(st_crs(aoi))

dist_sf3 <- st_as_sf(df3, coords = c("X", "Y")) %>%
  st_set_crs(st_crs(aoi))

## create the distance raster
dist_lakes <- rasterize(dist_sf1, r, "mindist_lakes", fun = mean)
dist_roads <- rasterize(dist_sf2, r, "mindist_roads", fun = mean)
dist_hospi <- rasterize(dist_sf3, r, "mindist_hospi", fun = mean)

## crop raster to country boundaries
dist_lakes <- mask(dist_lakes, rwa)
dist_roads <- mask(dist_roads, rwa)
dist_hospi <- mask(dist_hospi, rwa)

## create matrix to reclassify raster to classes of 10 km
## first class == 1km
recl_matrix <- matrix(c(0,1,1,
                        1,10,2,
                        10,20,3,
                        20,30,4,
                        30,40,5,
                        40,50,6,
                        50,60,7,
                        60,70,8,
                        70,80,9,
                        80,90,10),ncol = 3, byrow = TRUE)


## reclassify distance raster
dist_lakes <- reclassify(dist_lakes, recl_matrix)
dist_roads <- reclassify(dist_roads, recl_matrix)
dist_hospi <- reclassify(dist_hospi, recl_matrix)

#******************************************#
#* 4. PLOTS
#******************************************#

#~~~~ 4.1. Raster

### lakes
png("./output/distance_to_lakes.png",
    width = 700,
    height = 700)
plot(dist_lakes)
plot(rwa$geometry, add = TRUE)
plot(lakes$geometry, col = "cyan", add = TRUE)
dev.off()

### roads
png("./output/distance_to_main_roads.png",
    width = 700,
    height = 700)
plot(dist_roads)
plot(rwa$geometry, add = TRUE)
plot(roads$geometry, col = "darkblue", add = TRUE)
dev.off()

### hospitals
png("./output/distance_to_hospitals.png",
    width = 700,
    height = 700)
plot(dist_hospi)
plot(rwa$geometry, add = TRUE)
plot(hospitals$geometry,
     pch = 3,
     col = "dodgerblue",
     add = TRUE)
dev.off()

#~~~~ 4.2. stat points

### extract raster values to House points
lakes_rv <- raster::extract(dist_lakes, houses) %>%
  data.frame()
roads_rv <- raster::extract(dist_roads, houses) %>%
  data.frame()
hospi_rv <- raster::extract(dist_hospi, houses) %>%
  data.frame()

### total number of rows for calculation of percentage
tot_obs_lakes <- nrow(lakes_rv)
tot_obs_roads <- nrow(roads_rv)
tot_obs_hospi <- nrow(hospi_rv)

### lakes
jpeg("./output/distance_to_lakes.jpg",
     width = 700,
     height = 700)
ggplot(data=subset(lakes_rv, !is.na(.))  , mapping = aes(x = factor(.))) +
  stat_count(geom = 'bar', aes(y = ..count..)) +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ . / tot_obs_lakes,
    labels = percent,
    name = "Percentage of Houses"
  )) +
  ylab("Number of Houses") +
  xlab("Distance Class") +
  labs(title = "Distance to Lakes") +
  guides(fill = FALSE)
dev.off()

### roads
jpeg("./output/distance_to_main_roads.jpg",
     width = 700,
     height = 700)
ggplot(data=subset(roads_rv, !is.na(.))  , mapping = aes(x = factor(.))) +
  stat_count(geom = 'bar', aes(y = ..count..)) +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ . / tot_obs_roads,
    labels = percent,
    name = "Percentage of Houses"
  )) +
  ylab("Number of Houses") +
  xlab("Distance Class") +
  labs(title = "Distance to Main roads") +
  guides(fill = FALSE)
dev.off()

### hospitals
jpeg("./output/distance_to_hospitals.jpg",
     width = 700,
     height = 700)
ggplot(data=subset(hospi_rv, !is.na(.))  , mapping = aes(x = factor(.))) +
  stat_count(geom = 'bar', aes(y = ..count..)) +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ . / tot_obs_hospi,
    labels = percent,
    name = "Percentage of Houses"
  )) +
  ylab("Number of Houses") +
  xlab("Distance Class") +
  labs(title = "Distance to Hospitals") +
  guides(fill = FALSE)
dev.off()

#Export distance raster.
writeRaster(dist_lakes, file = lakes_out, overwrite = TRUE)
writeRaster(dist_roads, file = roads_out, overwrite = TRUE)
writeRaster(dist_hospi, file = hospitals_out, overwrite = TRUE)

######################################################################