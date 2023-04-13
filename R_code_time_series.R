# Try this if raster package doesn't work
# install.packages("terra")
# library(terra)
# rast function from terra package
# test <- rast("lst_2005.tif")  # used rather than the raster function
# plot(test)

# Time series analysis
# Greenland increase of temperature
# Data and code from Emanuela Cosma

install.packages("raster")
install.packages("rasterVis")
install.packages("rgdal")
library(raster)
library(rasterVis)
library(rgdal)


setwd("C:/lab/greenland") # Windows


lst_2000 <- raster("lst_2000.tif")
lst_2005 <- raster("lst_2005.tif")
lst_2010 <- raster("lst_2010.tif")
lst_2015 <- raster("lst_2015.tif")

# par (in order to visualize data in a single sheet)
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

# list.files() allows you to select files by pattern
rlist <- list.files(pattern="lst")
rlist

# lapply() function allows you to visualize info for all the files listed with list.files
import <- lapply(rlist,raster)
import

# stack function allows you to pack them into a single raster, so you can plot the files using a single term
TGr <- stack(import)
TGr
plot(TGr)

plotRGB(TGr, 1, 2, 3, stretch="Lin") 
plotRGB(TGr, 2, 3, 4, stretch="Lin")
plotRGB(TGr, 4, 1, 2, stretch="Lin")
dev.off()

# levelplot(TGr) <- it is our last function to be used. 
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
plot(TGr, col=cl)

levelplot(TGr,col.regions=cl, main="LST variation in time",
           names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))
dev.off()


# This net exercise will rely on the raster images of folder EN, reporting the quality of air in northern Italy during lockdown.

setwd("C:/lab/EN") # Windows

# importing one file, use raster if it is a single file
EN_0001<-raster("EN_0001.png")
cl2 <- colorRampPalette(c("red","orange","yellow"))(100)
plot(EN_0001, col=cl2)

# Let's find, order and regroup our files
en_list <- list.files(pattern="EN")
en_list

en_import <- lapply(en_list,raster)
en_import

EN <- stack(en_import)
EN
plot(EN)
plot(EN, col=cl2)

# let's check the imported pic with the first element of the stack: we need them to be the same.
par(mfrow=c(1,2))
plot(EN_0001, col=cl2)
plot(EN[[1]], col=cl2)

# another way to check is by making a difference between the two pics, if values are 0,0 then they're the same
en_check <- EN[[1]]-EN_0001
plot(en_check) #let's plot it with standard colors. If the whole picture is 0 then it should result of the color for "0" value-
dev.off()


# First and last data
par(mfrow=c(1,2))
plot(EN[[1]])
plot(EN[[13]])
dev.off()

diff_EN <- EN[[1]]-EN[[13]]
diff_EN
plot(diff_EN, col=cl)
plotRGB(EN, 1,7,13, stretch="lin")
