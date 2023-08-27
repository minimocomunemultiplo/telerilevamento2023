# LET'S SHOW A CLASSIFICATION FOR LAND COVER

# Set up your library folks!
install.packages("raster")
library(raster)
install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")
library(patchwork) # for multiframe ggplot plotting

setwd("C:/lab/") # Windows users


# IMPORT SECTION, GET THE DATA IN

defor1 <- brick("defor1_.png")
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")

defor2 <- brick("defor2_.png")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# NIR 1, RED 2, GREEN 3

par(mfrow=c(1,2))
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")


# UNSUPERVISED CLASSIFICATION (without RStoolbox)
# This first part is actually a classification as we saw in the classification script

# For our first file defor1

# 1. Get all the single values for every pixel of the picture
singlenr1 <- getValues(defor1)
singlenr1

set.seed(99) # For reproducibility

# 2. Classify the single values as we saw in "Classification" script
kcluster1 <- kmeans(singlenr1, centers = 3)
kcluster1

# 3. Set values to a raster on the basis of defor1
defor1class <- setValues(defor1[[1]], kcluster1$cluster) # assign new values to a raster object

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(defor1class, col=cl)
# class 1: forest
# class 2: agriculture

# set.seed() would allow you to attain the same results ...

# defor2

# 1. Get all the single values
singlenr2 <- getValues(defor2)
singlenr2
#
# set.seed(99)

# 2. Classify
kcluster2 <- kmeans(singlenr2, centers = 3)
kcluster2

# 3. Set values to a raster on the basis of so
defor2class <- setValues(defor2[[1]], kcluster2$cluster) # assign new values to a raster object

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(defor2class, col=cl)
# class 1: forest
# class 2: agriculture

# Now we want to calculate some data, like frequencies and percentages
frequencies1 <- freq(defor1class)
tot1 = ncell(defor1class)
percentages1 = frequencies1 * 100 /  tot1

# percent forest: 89.83012
# percent agriculture: 10.16988

frequencies2 <- freq(defor2class)
tot2 = ncell(defor2class)
percentages2 = frequencies2 * 100 /  tot2

# build a dataframe hosting both percentages for the single years
cover <- c("Forest","Agriculture")
percent_1992 <- c(89.83, 10.16)
percent_2006 <- c(52.06, 47.93)

percentages <- data.frame(cover, percent_1992, percent_2006) # here is the code for the dataframe
percentages

# let's plot them together next to each other
ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")

ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

p1 <- ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")

p2 <- ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

p1+p2


# same percentages limits
p1 <- ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white") +
ylim(c(0,100))

p2 <- ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity",
fill="white") +
ylim(c(0,100))

p1+p2
