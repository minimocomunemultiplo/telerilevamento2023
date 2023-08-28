# Species Distribution Modelling script

# start by setting up your library

install.packages("sdm")
install.packages("rgdal", dependencies=T)
library(sdm)
library(raster) # predictors
library(rgdal) # species

# store into "file" the data you downloaded as "shp" using the right functions from the downloaded packages
file <- system.file("external/species.shp", package="sdm")
species <- shapefile(file) 
# specify to store into "species" the "file" with the shapefile function
# which can handle these files at best.

# looking at the set
species

# plot the set
plot(species)

# looking at the occurrences
species$Occurrence

# copy and then write:
plot(species[species$Occurrence == 1,],col='blue',pch=16) # plot the occurrence of that species when it is 1 in blue
points(species[species$Occurrence == 0,],col='red',pch=16) # plot the occurrence of that species when it is 0 in red

# predictors: look at the path given by the function system.file and store it.
path <- system.file("external", package="sdm")

# list the predictors. It's easier now because you can use the "path" stored before.
lst <- list.files(path=path,pattern='asc$',full.names = T) #
lst

# stack
preds <- stack(lst)

# plot preds variable
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl) # so far it's been done as we've seen for raster stacks and stack plotting

# plot predictors and occurrences
plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16) # this shows the species where the occurrence is 1 as points together 
# with the plot of the elevation attribute for the preds stack. Same does further on for other elements.

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# model

# set the data for the sdm
datasdm <- sdmData(train=species, predictors=preds)

# model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=datasdm, methods = "glm")

# make the raster output layer
p1 <- predict(m1, newdata=preds)

# plot the output
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# add to the stack
s1 <- stack(preds,p1)
plot(s1, col=cl)

# here is how to change the names for the elements of the stack
# choose a vector of names for the stack, looking at the previous graph, which are:
names(s1) <- c('elevation', 'precipitation', 'temperature', 'vegetation', 'model')

# and then replot!:
plot(s1, col=cl)
# you are done, with one line of code (as usual!)
