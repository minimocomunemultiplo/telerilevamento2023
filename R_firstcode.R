# My first code straight in GitHub (instead of copy 'n' paste on GitHub).
# this script contains workflow for remote sensing course

# we first install the needed packages

install.packages("raster")
library(raster)

# setting working directory for importing data

setwd("C:/lab/")

# Before importing data, here is a little explaination of how the game works for a landsat image of earth captured in space. 
# Reflectance is ratio between incident light and reflected light.
# Landsat first reflectance sensor is the blue one. then green, then red, then close infrared, then middle infrared, then thermical 
# infrared, last again middle infrared.
# "brick" function (included in "raster" package) swill store all these data together and project them into R. Let's create a new 
# file with all these data inside.

l2011 <- brick("p224r63_2011_masked.grd")
l2011

# Plot
# By this plot we can understand that objects absorbing high values of that particular wavelength will be white (by default), 
# Very colored will be the objects reflecting that wavelength

plot(l2011)

# now we change the colors of our graphs.

cl <- colorRampPalette(c("red","orange","yellow")) (100) # 100 is the shades
plot(l2011, col = cl) # col is the parameter to work on color

# our landsat picture is formed of 7 elements (the bands). What if we wanted to plot the pixels of a certain band?

l2011[[4]] # this is the way to pick an element in a bi-dimensional space (such as a matrix).
# we could even recall it by its name: l2011$b4_sre
# We could type: nir <- l2011[[4]] . Near infra red band is now assigned to a variable, easier to reach and recall.
plot(l2011[[4]], col = cl) # this is the plot of the band number 4.
