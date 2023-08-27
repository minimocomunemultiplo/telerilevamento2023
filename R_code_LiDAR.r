# Load needed packages
library(raster) #"Geographic Data Analysis and Modeling"
library(rgdal) #"Geospatial Data Abstraction Library"
library(viridis)
library(ggplot2)

# set working directory
setwd("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Universit� di Bologna/lezioni/UniBo/esercizi/risultati")

# load dsm 2013
dsm_2013 <- raster("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Universit� di Bologna/lezioni/UniBo/esercizi/dati/2013Elevation_DigitalElevationModel-0.5m.tif")

# view info about the raster.
dsm_2013

# plot the DSM 2013
plot(dsm_2013, main="Lidar Digital Surface Model San Genesio/Jenesien")

# load dtm 2013
dtm_2013 <- raster("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Universit� di Bologna/lezioni/UniBo/esercizi/dati/2013Elevation_DigitalTerrainModel-0.5m.tif")

#plot dtm 2013
plot(dtm_2013, main="Lidar Digital Terrain Model San Genesio/Jenesien")

# create CHM 2013 as difference between dsm and dtm
chm_2013 <- dsm_2013 - dtm_2013

# view CHM attributes
chm_2013


chm_2013d <- as.data.frame(chm_2013, xy=T)

ggplot() +
  geom_raster(chm_2013d, mapping =aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("CHM 2013 San Genesio/Jenesien")

#save the CHM on computer
writeRaster(chm_2013,"chm_2013_San_genesio.tif")


# load dsm 2004
dsm_2004 <- raster("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Universit� di Bologna/lezioni/UniBo/esercizi/dati/2004Elevation_DigitalElevationModel-2.5m.tif")

# view info about the raster.
dsm_2004

# plot the DSM 2004
plot(dsm_2004, main="Lidar Digital Surface Model San Genesio/Jenesien")

# load dtm 2004
dtm_2004 <- raster("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Universit� di Bologna/lezioni/UniBo/esercizi/dati/2004Elevation_DigitalTerrainModel-2.5m.tif")

#plot dtm 2004
plot(dtm_2004, main="Lidar Digital Terrain Model San Genesio/Jenesien")

# create CHM 2004 as difference between DSM and DTM
chm_2004 <- dsm_2004 - dtm_2004

# view CHM attributes
chm_2004

chm_2004d <- as.data.frame(chm_2004, xy=T)

#plot CHM 2004
ggplot() +
  geom_raster(chm_2004d, mapping =aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("CHM 2004 San Genesio/Jenesien")


#save CHM on computer
writeRaster(chm_2004,"chm_2004_San_genesio.tif")


#error
difference_chm<-chm_2013-chm_2004

#reseample 2013 to 2004 @2.5m
chm_2013_reseampled<-resample(chm_2013, chm_2004)

#calculate difference in CHM
difference<-chm_2013_reseampled-chm_2004

#plot the difference
ggplot() +
  geom_raster(difference, mapping =aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("difference CHM San Genesio/Jenesien")


#save the rasters
writeRaster(chm_2013_reseampled,"chm_2013_reseampled_San_genesio.tif")
writeRaster(difference,"difference chm San_genesio.tif")





## point cloud

library(lidR)

#load point cloud
point_cloud<-readLAS("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Universit� di Bologna/lezioni/UniBo/esercizi/dati/DownloadService/LIDAR-PointCloudCoverage-2013SolarTirol.laz")

#plot r3 point cloud
plot(point_cloud)
