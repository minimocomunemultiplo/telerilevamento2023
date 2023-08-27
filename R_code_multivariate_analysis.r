install.packages("raster")
install.packages("ggplot2")
library(raster)
library(ggplot2)


setwd("C:/lab/") # Windows users

# IMPORT THE DATA

sen <- brick("sentinel.png")
plot(sen) # plot the result

# Build a stack (a raster) made of all the single raster bands
sen2 <- stack(sen[[1]], sen[[2]], sen[[3]])
plot(sen2) # plot the result (it will show up 3 raster, each is a band)

pairs(sen2) # Used to create scatterplot matrices (for rasters, it would be better to switch to dataframe
# for each layer, but anyway). 
# Scatterplot matrices can be useful for visualizing relationships between variables (in this case, bands), 
# but their effectiveness might be limited when dealing with high-dimensional data

# PCA (Principal Component Analysis) WITHOUT RStoolbox!
sample <- sampleRandom(sen2, 10000)
pca <- prcomp(sample)

# variance explained
summary(pca)

# correlation with original bands
pca

# pc map
pci <- predict(sen2, pca, index=c(1:2))

plot(pci[[1]])

# ggplot
pcid <- as.data.frame(pci[[1]], xy=T)

ggplot() +
geom_raster(pcid, mapping = aes(x=x, y=y, fill=PC1)) +
scale_fill_viridis()


# speeding up analyses
# aggregate cells: resampling (ricampionamento)
senres <- aggregate(sen, fact=10) # this makes a resampling by a factor of 10
# in order to make things lighter and easier and quicker.

# then repeat the PCA analysis
