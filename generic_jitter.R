#generic jitter code

#set working directory
setwd(choose.dir())

#load library
library(raster)

#load shapefile
shp <- shapefile(file.choose())

#random values for offsetting
shp$x_offset <- sample(482:804, size =nrow(shp), replace = TRUE)
shp$y_offset <- sample(482:804, size =nrow(shp), replace = TRUE)


#jitter (offsetting) calculation
##########################double check X and Y coordinate names here##########################
shp$calc_X_jitter <- shp$long_x + sample(c(-1,1), size=nrow(shp), replace = TRUE)*shp$x_offset
shp$calc_Y_jitter <- shp$lat_y + sample(c(-1,1), size=nrow(shp), replace = TRUE)*shp$y_offset
##############################################################################################


#turn into new data frame
new <- data.frame(shp)

#more packages
library(maptools)
library(rgdal)
library(sp)

#new data frame to spatial coordinates based on jittered values
coordinates(new) =~ calc_X_jitter+calc_Y_jitter
#define projection as UTM
proj4string(new) <- CRS("+init=epsg:26913")

#fix the shitty naming above
library(gdata)
new <- rename.vars(new, from = c("coords.x1", "coords.x2"), to = c("jitter_x", "jitter_y"))


#save new file output as .shp
raster::shapefile(new, "jittered_pts.shp", overwrite = TRUE)



################################################################################################

#install.packages('maps')
#install.packages('mapdata')

library(maps)
library(mapdata)

map('state', xlim = c(-110, -100), ylim = c(35, 45), col = "gray90", fill = TRUE)
points(shp, pch = 19, col = "red", cex = 0.5)



