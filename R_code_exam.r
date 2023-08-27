############################################# SET UP SECTION ############################################

# Hi, in this program I will elaborate some image data recovered from Sentinel satellite. The whole analysis consists of comparing two 
# Sentinel 2A pictures of Iceland (2018 and 2023), to see if the glacier extension has changed over the course of 5 years for the month of 
# April. Recently, many volcano eruptions (2021-2023) have happened on the southern part of this island, and I was interested in checking
# the ice-cover situation.

# Let's start by setting the required packages for our work.

setwd("C:/Progetto Remote Sensing/sentinel data")

install.packages("ggplot2")
install.packages("raster")
install.packages("rasterVis")
install.packages("rgdal")
install.packages("viridis")
install.packages("patchwork")
install.packages("gridExtra")
library(raster)
library(ggplot2)
library(viridis)
library(patchwork)
library(rasterVis)
library(rgdal)
library(gridExtra)


############################################# IMPORT AND PLOT SECTION ############################################

# In this first "import section" we will provide groups for all band files for each year

rlist <- list.files(pattern="SEN_IC_18_") # rlist let's us take all files with a certain name.
import <- lapply(rlist,raster) # lapply applies a function (in this case raster) to all elements from rlist.
S_IC18 <- stack(import) # stack function puts files imported as raster in one variable (memory reference) in R.
S_IC18 #let's show our result: 

rlist <- list.files(pattern="SEN_IC_23_")
import <- lapply(rlist,raster)
S_IC23 <- stack(import)
S_IC23


ext <- c(600000,630000,7070000,8000000)
S_IC23 <- crop(S_IC23,ext)
S_IC18 <- crop(S_IC18,ext)

S_IC23
S_IC18

## S_IC23
## class      : RasterBrick 
## dimensions : 1502, 1500, 2253000, 6  (nrow, ncol, ncell, nlayers)
## resolution : 20, 20  (x, y)
## extent     : 6e+05, 630000, 7070000, 7100040  (xmin, xmax, ymin, ymax)
## crs        : +proj=utm +zone=27 +datum=WGS84 +units=m +no_defs 
## source     : memory
## names      : SEN_IC_23_B02_20m, SEN_IC_23_B03_20m, SEN_IC_23_B04_20m, SEN_IC_23_B11_20m, SEN_IC_23_B12_20m, SEN_IC_23_B8A_20m 
## min values :               763,              1003,               992,               971,               991,               754 
## max values :             18140,             17853,             17252,             16177,             16088,             16633 

##  S_IC18
##  class      : RasterBrick 
##  dimensions : 1502, 1500, 2253000, 6  (nrow, ncol, ncell, nlayers)
##  resolution : 20, 20  (x, y)
##  extent     : 6e+05, 630000, 7070000, 7100040  (xmin, xmax, ymin, ymax)
##  crs        : +proj=utm +zone=27 +datum=WGS84 +units=m +no_defs 
##  source     : memory
##  names      : SEN_IC_18_B02_20m, SEN_IC_18_B03_20m, SEN_IC_18_B04_20m, SEN_IC_18_B11_20m, SEN_IC_18_B12_20m, SEN_IC_18_B8A_20m 
##  min values :                 1,                 1,                 1,                 3,                18,                 1 
##  max values :             15994,             15938,             15711,             10342,             11614,             15550 

# IMPORTANT:  1 -> B2=BLUE 
#             2 -> B3=GREEN 
#             3 -> B4=RED
#             4 -> B11=SWIR1
#             5 -> B12=SWIR2
#             6 -> B8a=NIR


# LET'S PLOT OUR PICTURES, EACH IN 2 WAYS. WE CLEARY DEFINE ICE COVER BETTER USING SWIR1 BAND THAN NIR, BECAUSE THE SNOW HIGHLY ABSORBS
# THE SWIR, WHEREAS THE VEGETATION AND THE CLOUDS REFLECT IT VERY POWERFULLY. 

par(mfrow = c(2, 2), bty = "n",oma = c(0, 0, 2, 0), bg = "gray")

plot_18_nir <- plotRGB(S_IC18, 6,2,1, stretch="lin")
legend("topright", legend = "2018 NIR")
plot_18_swir1 <- plotRGB(S_IC18, 4,2,1, stretch="lin")
legend("topright", legend = "2018 SWIR")

plot_23_nir <- plotRGB(S_IC23, 6,2,1, stretch="lin")
legend("topright", legend = "2023 NIR")
plot_23_swir1 <- plotRGB(S_IC23, 4,2,1, stretch="lin")
legend("topright", legend = "2023 SWIR")

mtext("RGB plot using different bands", outer = TRUE, cex = 1.5)

dev.off()

############################################# S3 INDEX SECTION ############################################

# Saito and Yamazaki proposed S3 as an improved snow mapping indices in 1999 at the same time as when NDSI was developed. In 2006, 
# Shimamura assessed the accuracy of S3 over NDSI, specifically in the areas where snow cover and forested areas overlapped each 
# other (cfr. https://www.mdpi.com/2072-4292/11/23/2774)

S3_18 <- ((S_IC18[[6]]*(S_IC18[[3]]-S_IC18[[4]])))/((S_IC18[[6]]+S_IC18[[3]])*(S_IC18[[6]]+S_IC18[[4]]))
S3_23 <- ((S_IC23[[6]]*(S_IC23[[3]]-S_IC23[[4]])))/((S_IC23[[6]]+S_IC23[[3]])*(S_IC23[[6]]+S_IC23[[4]]))
S3_diff <- S3_18 - S3_23

S3_18_min <- minValue(S3_18)
S3_18_max <- maxValue(S3_18)
S3_23_min <- minValue(S3_23)
S3_23_max <- maxValue(S3_23)
S3_diff_min <- minValue(S3_diff)
S3_diff_max <- maxValue(S3_diff)

S3_18_n <- (S3_18 - S3_18_min) / (S3_18_max - S3_18_min) * 2 - 1
S3_23_n <- (S3_23 - S3_23_min) / (S3_23_max - S3_23_min) * 2 - 1
S3_diff_n <- (S3_diff - S3_diff_min) / (S3_diff_max - S3_diff_min) * 2 - 1

## Let's plot all results

par(mfrow = c(1, 3), bty = "n",oma = c(0, 0, 2, 0), bg = "gray")
plot(S3_18_n,
     main = "S3 2018",
     xaxt = "n", yaxt = "n")
plot(S3_23_n,
     main = "S3 2023",
     xaxt = "n", yaxt = "n")
plot(S3_diff_n,
     main = "S3 diff",
     xaxt = "n", yaxt = "n")
mtext("S3 index", outer = TRUE, cex = 1.5)

dev.off()

############################################# SWI INDEX SECTION ############################################
# Precise extraction and mapping of the snow-covered area in mountainous regions need proficient tools that are not affected by the 
# other neighboring land cover variables, particularly water. In order to achieve more accurate snow cover mapping, in the current 
# study, we proposed a new index named as SWI (cfr. https://www.mdpi.com/2072-4292/11/23/2774).

SWI_18 <- (S_IC18[[2]]*(S_IC18[[6]]-S_IC18[[4]]))/((S_IC18[[2]]+S_IC18[[6]])*(S_IC18[[6]]+S_IC18[[4]]))
SWI_23 <- (S_IC23[[2]]*(S_IC23[[6]]-S_IC23[[4]]))/((S_IC23[[2]]+S_IC23[[6]])*(S_IC23[[6]]+S_IC23[[4]]))
SWI_diff <- SWI_18 - SWI_23

SWI_18_min <- minValue(SWI_18)
SWI_18_max <- maxValue(SWI_18)
SWI_23_min <- minValue(SWI_23)
SWI_23_max <- maxValue(SWI_23)
SWI_diff_min <- minValue(SWI_diff)
SWI_diff_max <- maxValue(SWI_diff)

SWI_18_n <- (SWI_18 - SWI_18_min) / (SWI_18_max - SWI_18_min) * 2 - 1
SWI_23_n <- (SWI_23 - SWI_23_min) / (SWI_23_max - SWI_23_min) * 2 - 1
SWI_diff_n <- (SWI_diff - SWI_diff_min) / (SWI_diff_max - SWI_diff_min) * 2 - 1

par(mfrow = c(1, 3), bty = "n",oma = c(0, 0, 2, 0), bg = "gray")
plot(SWI_18_n,
     main = "SWI 2018",
     xaxt = "n", yaxt = "n")
plot(SWI_23_n,
     main = "SWI 2023",
     xaxt = "n", yaxt = "n")
plot(SWI_diff_n,
     main = "SWI diff",
     xaxt = "n", yaxt = "n")
mtext("SWI index", outer = TRUE, cex = 1.5)

dev.off()


############################################# NDSI SECTION ############################################

# Now let's calculate our NDSI (normalized difference snow index), using a simple formula: (Bg - Bswir1 / Bg + Bswir1), ranging thus 
# from -1 to +1. Ice strongly absorbs SWIR1 wavelengths, and strongly reflects green wavelength. So making a difference between these
# two bands, will help us improve to delineate ice, distinguishing it from things that also reflect NIR and such wavelengths (so we
# are technically sharpening the work we did just before with NIR, which is also reflected by vegetation and this could compromise
# the results).

## CALCULATE NDSI FOR EACH YEAR AND THEIR DIFFERENCE

NDSI18 <- (S_IC18[[2]] - S_IC18[[4]])/(S_IC18[[2]] + S_IC18[[4]])
NDSI23 <- (S_IC23[[2]] - S_IC23[[4]])/(S_IC23[[2]] + S_IC23[[4]])
NDSI_diff <- NDSI18 - NDSI23

## Normalize all the NDSI values from -1 to 1.

NDSI18_min <- minValue(NDSI18)
NDSI18_max <- maxValue(NDSI18)
NDSI23_min <- minValue(NDSI23)
NDSI23_max <- maxValue(NDSI23)
NDSI_diff_min <- minValue(NDSI_diff)
NDSI_diff_max <- maxValue(NDSI_diff)

NDSI18_n <- (NDSI18 - NDSI18_min) / (NDSI18_max - NDSI18_min) * 2 - 1
NDSI23_n <- (NDSI23 - NDSI23_min) / (NDSI23_max - NDSI23_min) * 2 - 1
NDSI_diff_n <- (NDSI_diff - NDSI_diff_min) / (NDSI_diff_max - NDSI_diff_min) * 2 - 1

## Plot the results together

icecol <- colorRampPalette(c("black","red","cadetblue1","white")) (100)
par(mfrow = c(1,3), bg="gray",bty = "n",oma = c(0, 0, 2, 0), bg = "gray") # Let's plot all snow indexes
plot(NDSI18_n, col= icecol,
     main = "NDSI 2018",
     xaxt = "n", yaxt = "n")
plot(NDSI23_n, col= icecol,
     main = "NDSI 2023",
     xaxt = "n", yaxt = "n")
plot(NDSI_diff_n, col= icecol,
     main = "NDSI difference",
     xaxt = "n", yaxt = "n")
mtext("NDSI index", outer = TRUE, cex = 1.5)


dev.off()


############################################# NBSI-MS SECTION ############################################

## WHAT HAPPENS IS THAT THERE IS SOME WRONG CLOUDS OVERLAY. TO REMOVE THIS, LET'S OPERATE AS SHOWN IN THIS ARTICLE 
## https://arxiv.org/pdf/2107.05574.pdf . WE NEED NON BINARY SNOW INDEX FOR MULTI COMPONENT SURFACES (NBSI-MS). 
## THIS METHOD LEADS US TO ACHIEVE A CLEAN PIXEL SEPARATION, WITHOUT CONFUSING MUCH CLOUDS (OR WATER OR LAND) AS SNOW.

NBSI_MS_18 <- (0.36*(S_IC18[[2]] + S_IC18[[3]] + S_IC18[[6]])) -
  (((S_IC18[[1]] + S_IC18[[5]])/S_IC18[[2]]) + S_IC18[[4]])

NBSI_MS_23 <- (0.36*(S_IC23[[2]] + S_IC23[[3]] + S_IC23[[6]])) -
  (((S_IC23[[1]] + S_IC23[[5]])/S_IC23[[2]]) + S_IC23[[4]])

NBSI_MS_diff <- NBSI_MS_18 - NBSI_MS_23

# Normalization between -1 and 1 of these results as well

NBSI_MS_18_min <- minValue(NBSI_MS_18)
NBSI_MS_18_max <- maxValue(NBSI_MS_18)
NBSI_MS_23_min <- minValue(NBSI_MS_23)
NBSI_MS_23_max <- maxValue(NBSI_MS_23)
NBSI_MS_diff_min <- minValue(NBSI_MS_diff)
NBSI_MS_diff_max <- maxValue(NBSI_MS_diff)

NBSI_MS_18_n <- (NBSI_MS_18 - NBSI_MS_18_min) / (NBSI_MS_18_max - NBSI_MS_18_min) * 2 - 1
NBSI_MS_23_n <- (NBSI_MS_23 - NBSI_MS_23_min) / (NBSI_MS_23_max - NBSI_MS_23_min) * 2 - 1
NBSI_MS_diff_n <- (NBSI_MS_diff - NBSI_MS_diff_min) / (NBSI_MS_diff_max - NBSI_MS_diff_min) * 2 - 1

# See the difference? 

icecol2 <- colorRampPalette(c("black","red","blue","white")) (100)
par(mfrow = c(1,3), bg="gray",bty = "n",oma = c(0, 0, 2, 0), bg = "gray") # Let's plot all snow indexes
plot(NBSI_MS_18_n,
     col = icecol2,
     main = "NBSI-MS 2018",
     xaxt = "n", yaxt = "n")
plot(NBSI_MS_23_n,
     col = icecol2,
     main = "NBSI-MS 2023",
     xaxt = "n", yaxt = "n")
plot(NBSI_MS_diff_n,
     col = icecol2,
     main = "NBSI-MS difference",
     xaxt = "n", yaxt = "n")
mtext("NBSI-MS index", outer = TRUE, cex = 1.5)

dev.off()


############################################# SOME CLEANING ############################################

## DATA CLEAN: LET'S REMOVE ALL VARIABLES USED FOR NORMALIZATION

rm(NBSI_MS_18_min,NBSI_MS_18_max,NBSI_MS_23_min,NBSI_MS_23_max,NBSI_MS_diff_min,NBSI_MS_diff_max)
rm(NDSI18_min,NDSI18_max,NDSI23_min,NDSI23_max,NDSI_diff_min,NDSI_diff_max)
rm(SWI_18_min,SWI_18_max,SWI_23_min,SWI_23_max,SWI_diff_min,SWI_diff_max)
rm(S3_18_min,S3_18_max,S3_23_min,S3_23_max,S3_diff_min,S3_diff_max)
gc()

############################################# PCA SECTION ############################################

## Calculation of Principal Components Analysis for the given indexes (S3, SWI, NDSI, NBSI-MS)

set.seed(1)
box <- stack(S3_diff_n, SWI_diff_n, NDSI_diff_n, NBSI_MS_diff_n)
plot(box, main = "Index differences",
     xaxt = "n", yaxt = "n")
sr <- sampleRandom(box, 10000)
pca <- prcomp(sr)
## variance explained
summary(pca)
## Correlation with original bands
plot(pca)
## PC MAP
pci <- predict(box, pca, index = 1:4)
plot(pci[[1]], col = icecol2)
pc_final <- as.data.frame(pci[[1]], xy=T)
ggplot() + 
  geom_raster(pc_final, mapping = aes(x = x, y = y, fill = PC1)) + 
  scale_fill_viridis(option="inferno")


# My PCA looks like it's saying that where the values go very low, the situation in terms of snow cover really changed. As values
# rise up the situation is predicted to change less.

############################################# CLASSIFICATION SECTION ############################################

## Classification of pixel values in order to count ice cover for both years using NBSI-MS elaboration.

singlenr <- getValues(S_IC18)
singlenr
set.seed(99)
kcluster <- kmeans(singlenr, centers = 2)
kcluster
S_IC18_class <- setValues(S_IC18[[1]], kcluster$cluster)

singlenr <- getValues(S_IC23)
singlenr
set.seed(1)
kcluster <- kmeans(singlenr, centers = 2)
kcluster
S_IC23_class <- setValues(S_IC23[[1]], kcluster$cluster)

par(mfrow = c(1,2), bg="gray",bty = "n")

bw <- colorRampPalette(c("white","black"))(2)
plot(S_IC18_class, col=bw,
     legend = FALSE,
     main = "SNOW IN 2018",
     xaxt = "n", yaxt = "n")
legend("bottom", legend = c("snow"), fill = c("white"))

bw <- colorRampPalette(c("white","black"))(2)
plot(S_IC23_class, col=bw,
     legend = FALSE,
     main = "SNOW IN 2023",
     xaxt = "n", yaxt = "n")
legend("bottom", legend = c("snow"), fill = c("white"))

frequencies1 <- freq(S_IC18_class)
tot1 = ncell(S_IC18_class)
percentages1 = frequencies1 * 100 /  tot1
percentages1 # percent snow 2018: 78,78 %

frequencies2 <- freq(S_IC23_class)
tot2 = ncell(S_IC23_class)
percentages2 = frequencies2 * 100 /  tot2
percentages2 # percent snow 2023: 24,33 %

############################################# CLASSIFICATION ON NBSI-MS SECTION ############################################

# Here I am trying to classify starting from the NBSI-MS difference for both years. I want to see how this process will classify
# and count the pixels I have for each class. If better or worse. I will eventually make a mean of the results of both 
# classifications 

singlenr <- getValues(NBSI_MS_diff_n)
singlenr
set.seed(99)
kcluster <- kmeans(singlenr, centers = 2)
kcluster
NBSI_diff_class <- setValues(NBSI_MS_diff_n, kcluster$cluster)

singlenr <- getValues(NBSI_MS_18_n)
singlenr
set.seed(99)
kcluster <- kmeans(singlenr, centers = 2)
kcluster
NBSI_18_class <- setValues(NBSI_MS_18_n, kcluster$cluster)

singlenr <- getValues(NBSI_MS_23_n)
singlenr
set.seed(99)
kcluster <- kmeans(singlenr, centers = 2)
kcluster
NBSI_23_class <- setValues(NBSI_MS_23_n, kcluster$cluster)

## PLOTTING THE RESULTS

par(mfrow = c(1,3), bg="gray",bty = "n")

bw <- colorRampPalette(c("white","black"))(2)
plot(NBSI_18_class, col=bw,
     legend = FALSE,
     main = "SNOW IN 2018 ",
     sub = "NBSI-MS 2018",
     xaxt = "n", yaxt = "n")
legend("bottom", legend = c("Non-Snow", "Snow"), fill = c("black", "white"))
plot(NBSI_23_class, col=bw,
     legend = FALSE,
     sub = "NBSI-MS 2023",
     main = "SNOW IN 2023",
     xaxt = "n", yaxt = "n")
legend("bottom", legend = c("Non-Snow", "Snow"), fill = c("white", "black"))
plot(NBSI_diff_class, col=bw,
     legend = FALSE,
     sub = "NBSI-MS difference",
     main = "SNOW DIFF",
     xaxt = "n", yaxt = "n")
legend("bottom", legend = c("snow diff"), fill = c("white"))

dev.off()

# frequencies
frequencies3 <- freq(NBSI_18_class)
tot3 = ncell(NBSI_18_class)
percentages3 = frequencies3 * 100 /  tot3
percentages3 # percent snow 2018: 79,10 %

frequencies4 <- freq(NBSI_23_class)
tot4 = ncell(NBSI_23_class)
percentages4 = frequencies4 * 100 /  tot4
percentages4 # percent snow 2023: 22,93 %

frequencies5 <- freq(NBSI_diff_class)
tot5 = ncell(NBSI_diff_class)
percentages5 = frequencies5 * 100 /  tot5
percentages5 # decrease in snow cover: 64,83 %

# Calculate now the mean of the percentages shown for each year and their difference

mean_snow_18 <- (78.78 + 79.10)/2
mean_snow_23 <- (24.33 + 22.93)/2
mean_snowdiff <- ((78.78-24.33) + (64.83))/2

# Let's make a barplot showing the percentages of the results together 

values <- c(mean_snow_18, mean_snow_23, mean_snowdiff)
categories <- c("2018", "2023", "diff")
barplot(values, names.arg = categories, main = "Means between values of N-RC and values of NBSI-MS-RC", 
        xlab = "years", ylab = "% snow",ylim = c(0, 100))
text(x = 1:length(values), y = values + 2, labels = values, pos = 3)
