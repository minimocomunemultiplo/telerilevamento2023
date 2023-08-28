# Using copernicus downloaded material (from Sentinel satellites by ESA)

# set up your libraries

install.packages("raster")
library(raster)
install.packages("ncdf4")
library(ncdf4)
library(ggplot2)
library(viridis)

# COPERNICUS PROGRAM OFFERS A NICE WEBSITE WHERE TO DOWNLOAD DATA FOR VEGETATION, TEMPERATURE, BATHIMETRY ETC GLOBALLY. 
# IT IS THOUGH NEEDED AN ACCOUNT
# IT IS THOUGH NOT SO EASY TO HANDLE, BE ACCURATE WHEN SETTING THE TIME PERIOD NEEDED. THERE MIGHT BE ISSUES FOR THE AREA YOU
# ARE LOOKING FOR.

# register and download data from:
# https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

ssoil <- raster("~/Downloads/c_gls_SSM1km_202305090000_CEURO_S1CSAR_V1.2.1.nc")
ssoil # see its values

plot(ssoil) # plot using base R function

ggplot() + # with ggplot2 package
+ geom_raster(ssoil, mapping=aes(x=x, y=y, fill=Surface.Soil.Moisture))

# with viridis
ggplot() +
geom_raster(ssoil, mapping=aes(x=x, y=y, fill=Surface.Soil.Moisture)) +
scale_fill_viridis(option="magma")
