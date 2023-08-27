# Calculating spectral indices

install.packages("raster") # set up needed libraries
library(raster)

setwd("C:/lab/") # Windows users

# Let's import out file, give it a name, plot it using plotRGB func

l1992 <- brick("defor1_.png")
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")

# layer 1 = NIR
# layer 2 = red
# layer 3 = green

# The same with our second file

l2006 <- brick("defor2_.png")
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# plot in a multiframe the two images with one on top of the other

par(mfrow=c(2,1)) # This sets the device layour as you wish
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

# DVI Difference Vegetation Index 1992
# it is a difference of bands, in order to 
# enhance the visualization of vegetation from the rest

dvi1992 = l1992[[1]] - l1992[[2]]
# or you could code: dvi1992 = l1992$defor1_.1 - l1992$defor1_.2
dvi1992
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
plot(dvi1992, col=cl) # plotting result

# DVI Difference Vegetation Index 2006
dvi2006 = l2006[[1]] - l2006[[2]]
dvi2006
plot(dvi2006, col=cl)

# DVI difference in time 
# Consists in applying a subtraction between the two extreme given years
dvi_dif = dvi1992 - dvi2006
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(dvi_dif, col=cld)

## Part 2. We wanto to compare the data between different file formats. We have to normalize the data.
## We will hence calculate NDVI (Normalized difference vegetation index) 

# Range DVI (8 bit): -255 a 255
# Range NDVI (8 bit): -1 a 1
# Range DVI (16 bit): -65535 a 65535
# Range NDVI (16 bit): -1 a 1

# Hence, NDVI can be used to compare images with a different radiometric resolution

# NDVI 1992
dvi1992 = l1992[[1]] - l1992[[2]]
ndvi1992 = dvi1992 / (l1992[[1]] + l1992[[2]])
# or
ndvi1992 = (l1992[[1]] - l1992[[2]]) / (l1992[[1]] + l1992[[2]]) # the second part of the expression is the normalization factor

cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100) # specifying a color scheme
plot(ndvi1992, col=cl)

# Make a multiframe with par()

# Multiframe with plotRGB and NDVI image below
par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plot(ndvi1992, col=cl)

# Now we do the same for 2006

ndvi2006 = dvi2006 / (l2006[[1]] + l2006[[2]])

# Multiframe with plotRGB and NDVI image below
par(mfrow=c(2,1))
plot(ndvi1992, col=cl)
plot(ndvi2006, col=cl)
