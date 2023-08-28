# Classification of remote sensing data via RStoolbox
# First of all install DevTools
install.packages("raster")
install.packages("devtools")
install.packages("RStoolbox")
library(devtools)
library(raster)
library(RStoolbox)

setwd("C:/lab/") # Windows users

# Let's import our image captured by the solar orbiter
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")

# Let's plot our picture changing the stretch parameter, which changes the color scale in terms of brightness
# (in a few and basic words)
plotRGB(so, 1, 2, 3, stretch="lin")
plotRGB(so, 1, 2, 3, stretch="hist") # this shifts the center point of our colors further in brightness

# CLASSIFYING SOLAR DATA
# https://rspatial.org/raster/rs/4-unsupclassification.html

# 1. Get all the single values
singlenr <- getValues(so)
singlenr

set.seed(99) #This is used to set the seed for the random number generator (for reproducibility)

# 2. Classify
# K-means clustering is an unsupervised machine learning algorithm used for partitioning a 
# dataset into groups (clusters) based on the similarity of data points
kcluster <- kmeans(singlenr, centers = 3)
kcluster

# 3. Set values to a raster on the basis of so

soclass <- setValues(so[[1]], kcluster$cluster) # assign new values to a raster object

# The code is assigning cluster assignments (results from k-means clustering) 
# to the values of the first element (so[[1]]) of a spatial object (e.g., a raster) 
# to associate each location with its corresponding cluster.

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soclass, col=cl)

# set.seed can be used for repeating the experiment in the same manner for N times
# http://rfunction.com/archives/62

####

# PART 2, GRAND CANYON

gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc

# red = 1
# green = 2
# blue = 3

plotRGB(gc, r=1, g=2, b=3, stretch="lin")

# change the stretch to histogram stretching for better visualization of differences
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# classification, repeat as before wisely

# 1. Get values
singlenr <- getValues(gc)
singlenr

# 2. Classify
kcluster <- kmeans(singlenr, centers = 3)
kcluster

# 3. Set values
gcclass <- setValues(gc[[1]], kcluster$cluster) # assign new values to a raster object

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(gcclass, col=cl)

frequencies <- freq(gcclass)
tot = 58076148
percentages = frequencies * 100 /  tot

# Exercise: classify the map with 4 classes

singlenr <- getValues(gc)
singlenr

# 2. Classify
kcluster <- kmeans(singlenr, centers = 4) 
# here we will set 4 centers of clustering pixels, so that we have 4 
# main types of pixel values, hence 4 classes.

kcluster

# 3. Set values
gcclass <- setValues(gc[[1]], kcluster$cluster) # assign new values to a raster object

cl <- colorRampPalette(c('yellow','black','red','blue'))(100) # give it 4 colors for the 4 classes
plot(gcclass, col=cl)

frequencies <- freq(gcclass)
tot = 58076148
percentages = frequencies * 100 /  tot


