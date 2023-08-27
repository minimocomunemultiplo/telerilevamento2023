# VARIABILITY

library(raster)
library(RStoolbox) # for image viewing and variability calculation
library(ggplot2) # for ggplot plotting
library(patchwork) # multiframe with ggplot2 graphs
install.packages("viridis")
library(viridis)

# ATTENTION: RStoolbox is currently out of cran. Some coding here might not be suitable for your
# version of R or RStudio. 

setwd("C:/lab/") # Windows users

# Exercise: import the image from Sentinel data.
sen <- brick("sentinel.png")

# band1 = NIR
# band2 = red
# band3 = green

# Plot the bands NIR,R,G of the picture using plotRGB (from raster package).
plotRGB(sen, 1, 2, 3, stretch="lin")

# NIR on g component. So now the Near infra red data is colored in green and the green spectrum is 
# given to the red color
plotRGB(sen, 2, 1, 3)

# calculation of variability over NIR
nir <- sen[[1]] # take the first element of sen (which is the NIR)

sd3 <- focal(nir, matrix(1/9, 3, 3), fun=sd) 
# sd stands for standard deviation 
# fun stands for function
# the line of code calculates the standard deviation of the near-infrared values within a 3x3 
# neighborhood for each cell in the nir raster and stores the results in the sd3 raster object. 

clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) #
plot(sd3, col=clsd) # plot everything 

# plotting with ggplot
sd3d <- as.data.frame(sd3, xy=T) #first turn it to dataframe

ggplot() +
geom_raster(sd3d, mapping=aes(x=x, y=y, fill=layer))

# Below we picture everything with different color palettes from viridis package

# with viridis
ggplot() +
geom_raster(sd3d, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis() +
ggtitle("Standard deviation by viridis package")

# cividis
ggplot() +
geom_raster(sd3d, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "cividis") +
ggtitle("Standard deviation by viridis package")

# magma
ggplot() +
geom_raster(sd3d, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "magma") +
ggtitle("Standard deviation by viridis package")

# inferno
ggplot() +
geom_raster(sd3d, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("Standard deviation by viridis package")

# patchwork
p1 <- ggplot() +
geom_raster(sd3d, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis() +
ggtitle("Standard deviation by viridis package")

p2 <- ggplot() +
geom_raster(sd3d, mapping =aes(x=x, y=y, fill=layer)) +
scale_fill_viridis(option = "inferno") +
ggtitle("Standard deviation by viridis package")

p1 + p2
