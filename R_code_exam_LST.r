## I WANTED TO MANAGE TO GET GOOD VALUES (INSTEAD OF NA'S) FROM LANDSAT IMAGERY, IN ORDER TO WORK ON THOSE IMAGES (FOR CLASSIFICATION) WHICH SEEMED TO BE WASTED OTHERWISE.
## WHILE WORKING ON THIS PROJECT, I ENCOUNTERED SOME ISSUES WITH THE OLD CALCULATIONS FROM THE R_CODE_EXAM.r PROJECT. HERE IS A BRIEF
## EXPLAINATION OF WHAT I SORTED OUT.

# R_code_exam.r seemed to have a fault to me. I realize it now. I am counting ice difference giving priority to the
# first element, which is 1985 ice cover. Because I take its pixels, classify them in snow and non snow, 
# then I subtract to this map the image from 2022 with its pixels classified in snow and non snow. It seems 
# legit, but this way I am producing these results:
# 1) snow pixel in 2018 (1) - snow pixel in 2022 (1) = 0 (black = "fake" equal cover)
# 2) snow pixel in 2023 (1) - non snow pixels in 2023 (0) = 1 (white) (actual snow diff)
# 3) non snow pixel in 1985 (0) - snow pixel in 2023 = -1 = ??? (this also ends up black) 
# The last result should be considered as well, as it shows the pixels present in 2023 and non present in 
# 2018. To these pixels I should moreover give a different color, like red for example. 
# This way we can have an actual cover difference.

## SO THIS NEW PROGRAM SHOULD TRY APPROACH TO THE PROBLEM WITH A SORT OF A FIX (I CALL I SORT OF, BECAUSE I'M STILL WORKING ON IT, AND BY THIS I MEAN THAT PERCENTAGES
## STILL LOOK WEIRD TO ME, SO I'LL KEEP IN INVESTIGATING. MEANWHILE I'LL KEEP YOU IN TOUCH WITH WHAT I AM DOING.

## BEWARE: THE SET OF PICTURES HERE IS DIFFERENT, AS IT IS FROM LANDSAT NOW. I CHOSE LANDSAT IMAGERY BECAUSE I WANTED TO SOLVE THE MISSING VALUES PROBLEM ON ALL VALUES
## OF A LANDSAT PICTURE. 

############## SET UP############

setwd("C:/Users/agoag/Desktop/ggg")

install.packages("raster")
install.packages("ggplot2")
install.packages("viridis")
install.packages("rasterVis")
install.packages("rgdal")
install.packages("sf")
install.packages("caret")
install.packages("gridExtra")
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(rgdal)
library(sf)
library(caret)
library(gridExtra)

warnings()

wkt <- sf::st_crs(4326)[[2]]
sp::CRS(wkt)

############# GET FILES #############

## CRS arguments: +proj=longlat +datum=WGS84 +no_defs

rlist <- list.files(pattern="LT05_19850617_B") # rlist let's us take all files with a certain name.
import <- lapply(rlist,raster) # lapply applies a function (in this case raster) to all elements from rlist.
sib85 <- stack(import) # stack function puts files imported as raster in one variable (memory reference) in R.
sib85 #let's show our result: 

rlist <- list.files(pattern="LT05_19980628_B")
import <- lapply(rlist,raster) 
sib98 <- stack(import)
sib98

rlist <- list.files(pattern="LE07_20100621_B")
import <- lapply(rlist,raster) 
sib10 <- stack(import)
sib10

rlist <- list.files(pattern="LC08_20220607_B")
import <- lapply(rlist,raster) 
sib22 <- stack(import)

# Check if plot works

plot(sib22[[1]])
plot(sib10[[1]])

# Cut these pics, they're uselessly enormous

ext <- c(400000,450000,8150000,8200000)
sib85 <- crop(sib85,ext)
sib98 <- crop(sib98,ext)
sib10 <- crop(sib10,ext)
sib22 <- crop(sib22,ext)

# See the content of the new cut variables
sib85
sib98
sib10
sib22

# Let's try to plotRGB them using some cool bands
par(mfrow = c(2, 2), bty = "n",oma = c(1, 2, 2, 1), bg = "gray")

plotRGB(sib85, 7,2,1, stretch="lin")
legend("topright", legend = "1985 SWIR")
plotRGB(sib98, 7,2,1, stretch="lin")
legend("topright", legend = "1998 SWIR")
plotRGB(sib10, 7,2,1, stretch="lin")
legend("topright", legend = "2010 SWIR")
plotRGB(sib22, 7,2,1, stretch="lin")
legend("topright", legend = "2022 SWIR")

mtext("RGB plot using SWIR bands on R", outer = TRUE, cex = 1.5)

dev.off()

# band 1 B
# band 2 G
# band 3 R
# band 4 NIR
# band 5 SWIR1
# band 6 THERMAL
# band 7 SWIR2

gc()

############# NBSI-MS #############

# Take the two extremes: 1985 and 2022

NBSI85 <- (0.36*(sib85[[2]] + sib85[[3]] + sib85[[4]])) -
  (((sib85[[1]] + sib85[[7]])/sib85[[2]]) + sib85[[5]])

NBSI22 <- (0.36*(sib22[[2]] + sib22[[3]] + sib22[[4]])) -
  (((sib22[[1]] + sib22[[7]])/sib22[[2]]) + sib22[[5]])

NBSI85 <- resample(NBSI85, NBSI22, method = "bilinear")

NBSI_diff <- NBSI85 - NBSI22

# Normalization between -1 and 1 of these results as well

NBSI85_min <- minValue(NBSI85)
NBSI85_max <- maxValue(NBSI85)
NBSI22_min <- minValue(NBSI22)
NBSI22_max <- maxValue(NBSI22)
NBSI_diff_min <- minValue(NBSI_diff)
NBSI_diff_max <- maxValue(NBSI_diff)

NBSI85_n <- (NBSI85 - NBSI85_min) / (NBSI85_max - NBSI85_min) * 2 - 1
NBSI22_n <- (NBSI22 - NBSI22_min) / (NBSI22_max - NBSI22_min) * 2 - 1
NBSI_diff_n <- (NBSI_diff - NBSI_diff_min) / (NBSI_diff_max - NBSI_diff_min) * 2 - 1

rm(NBSI85_min,NBSI85_max,NBSI22_min,NBSI22_max,NBSI_diff_min,NBSI_diff_max)
gc()

# Turning them to data frames as we want to use ggplot for the absolute colorscales from viridis package.

data85 <- as.data.frame(NBSI85_n, xy=TRUE)
data22 <- as.data.frame(NBSI22_n, xy=TRUE)
data_diff <- as.data.frame(NBSI_diff_n, xy=TRUE)
# plotting them using ggplot2

plot85 <- ggplot() + 
  geom_raster(data85, 
              mapping = aes(x = x, y = y, fill= layer)) + 
scale_fill_viridis(option="inferno") +
  ggtitle("NBSI-MS 1985")

plot22 <- ggplot() + 
  geom_raster(data22, 
              mapping = aes(x = x, y = y, fill= layer)) + 
  scale_fill_viridis(option="inferno") +
  ggtitle("NBSI-MS 2022")

plot_diff <- ggplot() + 
  geom_raster(data_diff, 
              mapping = aes(x = x, y = y, fill= layer)) + 
  scale_fill_viridis(option="inferno") +
  ggtitle("NBSI-MS difference")

plot85
plot22
plot_diff

grid.arrange(plot85, plot22, plot_diff, ncol = 3, respect = TRUE)

dev.off()

############### LAND COVER BASED ON NBSI-MS INDEX ###########

# WARNING : THIS SECTION IS A TRIAL. IT HAS ISSUES, THE NEXT SECTION WILL FIX THE DIFFERENCE OF THE TWO YEARS.

# We will use the same dataframe we edited before, so I will not recall it
# data_diff <- as.data.frame(NBSI_diff_n, xy =TRUE)

head(data_diff) # Let's check if it looks like there are numbers
threshold_value <- 0  # Adjust this threshold based on your data, we want it to be the borderline between our
# 2-class classification (ice-non ice)

# Assuming pixel values above the threshold represent ice
data_diff$Ice <- ifelse(data_diff$layer > threshold_value, 1, 0) # in this func, after the threshold we write 
# what the value becomes and then what it becomes if it's higher than threshold, then what it becomes
# if it doesnt go above the threshold.

# Get the dimensions and extent of the original raster
raster_dim_diff <- dim(NBSI_diff_n)
raster_extent_diff <- extent(NBSI_diff_n)
raster_dim_diff
raster_extent_diff

# Create a new raster layer with the same dimensions and extent
prediction_raster_diff <- raster(nrows = raster_dim_diff[1], ncols = raster_dim_diff[2], 
                                 ext = raster_extent_diff)

# Assign the predicted values to the new raster
values(prediction_raster_diff) <- matrix(data_diff$Ice, ncol = raster_dim_diff[2], byrow = TRUE)

# Plot the prediction raster

bw <- colorRampPalette(c("black","white"))(2)

par(bg="gray",bty = "n")
plot(prediction_raster_diff,
     col = bw,
     legend = FALSE,
     main = "Difference in Ice cover",
     xaxt = "n", yaxt = "n")
legend("right", legend = c("Ice diff"), fill = c("white"),
       x = "right", y = "top", bty = "n", inset = c(0.88, 0.5))


# Now I will show you separate plots for Ice cover pixels in 1985 and 2022

head(data85) 
threshold_value <- 0
data85$Ice <- ifelse(data85$layer > threshold_value, 1, 0) 
raster_dim85 <- dim(NBSI85_n)
raster_extent85 <- extent(NBSI85_n)
prediction_raster_85 <- raster(nrows = raster_dim85[1], ncols = raster_dim85[2], ext = raster_extent85)
values(prediction_raster_85) <- matrix(data85$Ice, ncol = raster_dim85[2], byrow = TRUE)
par(bg="gray",bty = "n")
plot(prediction_raster_85,
     col = bw,
     legend = FALSE,
     main = "Ice cover in 1985",
     xaxt = "n", yaxt = "n")
legend("right", legend = c("Ice"), fill = c("white"),
       x = "right", y = "top", bty = "n", inset = c(0.88, 0.5))


head(data22) 
threshold_value <- 0
data22$Ice <- ifelse(data22$layer > threshold_value, 1, 0) 
raster_dim22 <- dim(NBSI22_n)
raster_extent22 <- extent(NBSI22_n)
prediction_raster_22 <- raster(nrows = raster_dim22[1], ncols = raster_dim22[2], ext = raster_extent22)
values(prediction_raster_22) <- matrix(data22$Ice, ncol = raster_dim22[2], byrow = TRUE)
par(bg="gray",bty = "n")
plot(prediction_raster_22,
     col = bw,
     legend = FALSE,
     main = "Ice cover in 1985",
     xaxt = "n", yaxt = "n")
legend("right", legend = c("Ice"), fill = c("white"),
       x = "right", y = "top", bty = "n", inset = c(0.88, 0.5))



## LET'S PLOT ALL THEM TOGETHER IN A SINGLE LAYOUT

par(mfrow = c(1,3), bg="gray",bty = "n")
    
    plot(prediction_raster_85,
         col = bw,
         legend = FALSE,
         main = "Ice cover in 1985",
         xaxt = "n", yaxt = "n")
    legend("right", legend = c("Ice"), fill = c("white"),
           x = "right", y = "top", bty = "n", inset = c(0.88, 0.5))
    
    plot(prediction_raster_22,
         col = bw,
         legend = FALSE,
         main = "Ice cover in 2022",
         xaxt = "n", yaxt = "n")
    legend("right", legend = c("Ice"), fill = c("white"),
           x = "right", y = "top", bty = "n", inset = c(0.88, 0.5))
    
    plot(prediction_raster_diff,
         col = bw,
         legend = FALSE,
         main = "Difference in Ice cover",
         xaxt = "n", yaxt = "n")
    legend("right", legend = c("Ice diff"), fill = c("white"),
           x = "right", y = "top", bty = "n", inset = c(0.88, 0.5))
    
dev.off()

## I WILL REPEAT NOW, BECAUSE HERE WE ENTER AN ADJUSTMENT SECTION

# This program has a fault, I realize it now. I am counting ice difference giving priority to the
# first element, which is 1985 ice cover. Because I take its pixels, classify them in snow and non snow, 
# then I subtract to this map the image from 2022 with its pixels classified in snow and non snow. It seems 
# legit, but this way I am producing these results:
# 1) snow pixel in 1985 (1) - snow pixel in 2022 (1) = 0 (black = "fake" equal cover)
# 2) snow pixel in 1985 (1) - non snow pixels in 2022 (0) = 1 (white) (actual snow diff)
# 3) non snow pixel in 1985 (0) - snow pixel in 2022 = -1 = ??? (this also ends up black) 
# The last result should be considered as well, as it shows the pixels present in 2022 and non present in 
# 1985. To these pixels I should moreover give a different color, like red for example. 
# This way we can have an actual cover difference. 


############# FIX THE CLASSIFICATION ###########################################################################

# I decided to take the dataframes of 1985 and 2022, as they're each made of just 0 and 1 values, where for each
# 0 signs non snow and 1 snow. I created a new dataframe of the difference of these, now i have also -1 values,
# indicating snow in 2022 (as it is the second term of subtraction). This way, my new color palette takes in 
# blue the -1, in black the 0, in white the 1.

dataframe_diff <- data85$Ice - data22$Ice 
dataframe_diff
raster_dim_diff <- dim(NBSI_diff_n)
raster_extent_diff <- extent(NBSI_diff_n)
prediction_raster_diff <- raster(nrows = raster_dim_diff[1], ncols = raster_dim_diff[2], 
                               ext = raster_extent_diff)
values(prediction_raster_diff) <- matrix(dataframe_diff, ncol = raster_dim_diff[2], byrow = TRUE)

bwb <- colorRampPalette(c("blue","black","white"))(3)

par(bg="gray",bty = "n")
plot(prediction_raster_diff,
     legend = FALSE,
     col = bwb,
     main = "Difference in Ice cover",
     xaxt = "n", yaxt = "n")
legend("right", legend = c("Ice 1985", "equal cover", "Ice in 2022"), fill = c("white","black","blue"),
       x = "right", y = "top", bty = "n", inset = c(0.855, 0.5))

dev.off() 

############## CLEAN UP USELESS VARIABLES FOR GOD'S SAKE ##############

rm(raster_dim85,raster_dim22,raster_dim_diff,raster_extent85,raster_extent22,raster_extent_diff,dataframe_diff)
gc()

############## NOW TIME FOR THE PERCENTAGES ###########

## BEWARE, THEY DON'T CONVICE ME AT ALL. I NEED TO FIND ANOTHER TESTING METHOD TO SEE IF I CAN TRUST THESE RESULTS, WHICH SEEM SENSEFUL A BIT ON THE PREVIOUS PLOT,
## BUT I CAN'T SLEEP AT NIGHT ON THESE FINAL PERCENTAGES

dataframe_diff ## let's start from the previous dataframe of the difference. Here we calculate the 0, the equals
str(dataframe_diff)
snow_equal <- sum(dataframe_diff == 0, na.rm = TRUE)
cat("Total count of 0 values (EQUAL):", snow_equal, "\n") # There are 1750878 zeros

snow85 <- sum(dataframe_diff == 1, na.rm = TRUE)
cat("Total count of 1 values (SNOW '85):", snow85, "\n") # There are 1354016 ones (snow) for 1985

snow22 <- sum(dataframe_diff == -1, na.rm = TRUE)
cat("Total count of -1 values (SNOW '22):", snow22, "\n") # There are 1320437 ones (snow) for 2022

tot <- 2778889 # The number of total values (pixels)

perc_snow85 <- (snow85/tot)*100
perc_snow85 # 18.78269
perc_snow22 <- (snow22/tot)*100
perc_snow22 # 17.77703
perc_equal <- (snow_equal/tot)*100
perc_equal # 63.0064

## Re-plot for visualization

par(bg="gray",bty = "n")
plot(prediction_raster_diff,
     legend = FALSE,
     col = bwb,
     main = "Difference in Ice cover",
     xaxt = "n", yaxt = "n")
legend("right", legend = c("Ice 1985", "equal cover", "Ice in 2022"), fill = c("white","black","blue"),
       x = "right", y = "top", bty = "n", inset = c(0.855, 0.5))

dev.off() 

# Bye.
