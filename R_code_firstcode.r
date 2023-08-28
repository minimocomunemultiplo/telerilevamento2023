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


# Yet another example of showing a picture and edit its color parameters.
clb <- colorRampPalette(c("black","gray","light gray")) (100)
plot(l2011$B1_sre, col=clb)

# Now let's export the files.
pdf("banda1.pdf")
plot(l2011$B1_sre, col=clb)
dev.off()

png("banda1.png")
plot(l2011$B1_sre, col=clb)
dev.off()

# Plot b2 from dark green to green to light green
clg <- colorRampPalette(c("dark green", "green", "light green")) (100)
plot(l2011$B2_sre, col=clg)

# Multiframe lets you split the view on a single window, so you can compare more things at once = MULTIFRAME
par(mfrow=c(1,2))
plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)
dev.off()

pdf("multiframe.pdf") # this function needs to be set before it's content, so when you write dev.off() it ends the process and saves as
                      # PDF what you have done so far.
par(mfrow=c(1,2))
plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)
dev.off()

# Exercise: revert the multiframe
par(mfrow=c(2,1))
plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)

# Let's plot the first four bands
par(mfrow=c(2,2))
# Blue
plot(l2011$B1_sre, col=clb)
# Green
plot(l2011$B2_sre, col=clg)
# Red
clr <- colorRampPalette(c("dark red", "red", "pink")) (100)
plot(l2011$B3_sre, col=clr)
# NIR
clnir <- colorRampPalette(c("red", "orange", "yellow")) (100)
plot(l2011$B4_sre, col=clnir)

# Plot RGB layers
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="lin")
plotRGB(l2011, r=3, g=2, b=4, stretch="lin")

plotRGB(l2011, r=3, g=4, b=2, stretch="hist")

# Exercise: build a multiframe with visible RGB
# (linear stretch) on top of false colours
# (histogram stretch)
par(mfrow=c(2,1))
plotRGB(l2011, r=3, g=2, b=1, stretch="lin")
plotRGB(l2011, r=3, g=4, b=2, stretch="hist")

# Exercise: upload the image from 1988
l1988 <- brick("p224r63_1988_masked.grd")
l1988

par(mfrow=c(2,1))
plotRGB(l1988, r=4, g=3, b=2, stretch="lin")
plotRGB(l2011, r=4, g=3, b=2, stretch="lin")
dev.off()



# Exercise: plot the NIR band
# plot(l2011$B4_sre)
# dev.off()
# You have to think that RGB uses R, G and B colors to plot the bands you give. So if at R you give the 4th band (NIR), all the infrared
# will be depicted using R, red. If you give to R the band of red (1) so it will plot in red what is red.
# plotRGB(l2011, r=4, g=3, b=2, stretch="lin")
# plotRGB(l2011, r=3, g=2, b=4, stretch="lin")


# Build a multiframe with 2 rows and 2 cols where 1:1988 LIN, 2:2011 LIN, 3:1988 HIST, 4:2011 HIST.
par(mfrow=c(2,2))
plotRGB(l1988, r=4, g=2, b=1, stretch="lin")
plotRGB(l2011, r=4, g=2, b=1, stretch="lin")
plotRGB(l1988, r=4, g=2, b=1, stretch="hist")
plotRGB(l2011, r=4, g=2, b=1, stretch="hist")
dev.off()

