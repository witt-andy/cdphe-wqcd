setwd("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data")

#Setting things up, and preparing the data
###############################################################################################################

#load useful spatial packages
library(dismo)
library(raster)
library(rgdal)
library(randomForest)
library(plyr)
library(rgeos)

#import csv data
mg <- read.csv("final_mg.csv", header = TRUE)
sg <- read.csv("final_sg_jan.csv", header = TRUE)
sm <- read.csv("final_sm_jan.csv", header = TRUE)
sea <- read.csv("final_seas_jan.csv", header = TRUE)
blue_carbon <-rbind.fill(mg, sg, sm, sea)

###############################################################################################################
#create exportable  blue_carbon .shp layer to examine
coordinates(blue_carbon) =~ Longitude.dd + Latitude.dd
proj4string(blue_carbon)<- CRS("+proj=longlat +datum=WGS84")
require(raster)
shapefile(blue_carbon, "blue_carbon_fnl_jan.shp", overwrite = TRUE)
bc <-shapefile('blue_carbon_fnl_jan.shp')
###############################################################################################################

#3/26/19 Using only ocean input data points, taken from ArcGIS
bc <-shapefile('blue_carbon_ocean.shp')

# Global Model

#set extent for all following raster variables
ext <- extent(-180.0, 180.0, -80.0, 80.0)

#load raster variables
mean_temp <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/temp_re10.tif")
mean_temp <- setExtent(mean_temp, ext, keepres = TRUE)
precip <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/precip_re3.tif")
precip <- setExtent(precip, ext, keepres = TRUE)
chla <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/chla_re3.tif")
chla <- setExtent(chla, ext, keepres = TRUE)
dem <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/dem_re4.tif")
dem <- setExtent(dem, ext, keepres = TRUE)
ssh <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/sea_ht_re2.tif")
ssh <- setExtent(ssh, ext, keepres = TRUE)
sst <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/sst_re2.tif")
sst <- setExtent(sst, ext, keepres = TRUE)
sss <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/sss_re2.tif")
sss <- setExtent(sss, ext, keepres = TRUE)
dist_land <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/dist_land2_re2.tif")
dist_land <- setExtent(dist_land, ext, keepres = TRUE)
dist_river <-raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/dist_river_re.tif")
dist_river <-setExtent(dist_river, ext, keepres = TRUE)
max_temp <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/max_temp_re2.tif")
max_temp <- setExtent(max_temp, ext, keepres = TRUE)
min_temp <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/min_temp_re2.tif")
min_temp <- setExtent(min_temp, ext, keepres = TRUE)

#stack predictor variables into 1 raster stack
preds_glob <- stack(mean_temp, precip, chla, dem, ssh, sst, sss, dist_land, dist_river, max_temp, min_temp)

#name variables in raster stack
names(preds_glob)[1] <- "temp_re10"
names(preds_glob)[2] <- "precip_re3"
names(preds_glob)[3] <- "chla_re3"
names(preds_glob)[4] <- "dem_re4"
names(preds_glob)[5] <- "sea_ht_re2"
names(preds_glob)[6] <- "sst_re2"
names(preds_glob)[7] <- "sss_re2"
names(preds_glob)[8] <- "dist_land2_re2"
names(preds_glob)[9] <- "dist_river_re"
names(preds_glob)[10] <- "max_temp_re2"
names(preds_glob)[11] <- "min_temp_re2"

#build datafram of bc data inputs matched with predictor variables for each point
bc_covar <- data.frame(coordinates(bc), bc, extract(mean_temp, bc, method = 'bilinear'), extract(precip, bc, method = 'bilinear'), 
                       extract(chla, bc, method = 'bilinear'), extract(dem, bc, method = 'bilinear'), 
                       extract(ssh, bc, method = 'bilinear'), extract(sst, bc, method = 'bilinear'),
                       extract(sss, bc, method = 'bilinear'), extract(dist_land, bc, method = 'bilinear'),
                       extract(dist_river, bc, method = 'bilinear'), extract(max_temp, bc, method = 'bilinear'),
                       extract(min_temp, bc, method = 'bilinear'))


#fixing databased names for rf models
names(bc_covar)[8] <- "stock.1m"
names(bc_covar)[12] <- "temp_re10"
names(bc_covar)[13] <- "precip_re3"
names(bc_covar)[14] <- "chla_re3"
names(bc_covar)[15] <- "dem_re4"
names(bc_covar)[16] <- "sea_ht_re2"
names(bc_covar)[17] <- "sst_re2"
names(bc_covar)[18] <- "sss_re2"
names(bc_covar)[19] <- "dist_land2_re2"
names(bc_covar)[20] <- "dist_river_re"
names(bc_covar)[21] <- "max_temp_re2"
names(bc_covar)[22] <- "min_temp_re2"

###############################################################################################################

#run rf model
library(randomForest)
print(bc_rf <- randomForest(stock.1m ~ temp_re10 + precip_re3 + chla_re3 + dem_re4 + sea_ht_re2
                            + sst_re2 + sss_re2 + dist_land2_re2 + dist_river_re + max_temp_re2 + min_temp_re2, 
                            data= bc_covar, mtry=2, importance=TRUE, na.action=na.omit))
#stats about rf model
plot(bc_rf)
varImpPlot(bc_rf)
importance(bc_rf)
# the greater the %incMSE the more important the variable, so low values mean bad predictor variables

###############################################################################################################

rp_raster <- predict(preds_glob, bc_rf, na.rm = TRUE, filename="bcstock_global_april.tif", overwrite = TRUE)
spplot(rp_raster)

resample_grid <- raster("C:/Users/Andy/Desktop/Carbon Mapping/Spatial Data/resample_grid3.tif")
rp_raster <- resample(rp_raster, resample_grid, method = 'ngb', filename="bcstock_rf_output_april.tif", overwrite = TRUE)

#How does it look?
spplot(rp_raster)

###############################################################################################################

# Finally, need to mask out the known MG and SM areas (studies have already determined there values)
#load dist of MG and SM
mg <- shapefile('mg_r2p.shp')
sm <- shapefile ('sm_r2p.shp')

#mask them out
rp_raster_mask <- mask(rp_raster, mg, inverse= TRUE)
rp_raster_mask2 <- mask(rp_raster_mask, sm, inverse = TRUE)



#Final Output
writeRaster(rp_raster_mask, filename= "bcstock_rf_fnl_output_april.tif", overwrite = TRUE)


###############################################################################################################

#Now lets produce an alternate output where kelp and coral areas are also removed

#load kelp and coral distributions
kelp <- shapefile("kelp_dissolve.shp")
reefs <- shapefile("reefs_dissolve.shp")

#mask them out (same as above)
rp_raster_mask3 <- mask(rp_raster_mask2, kelp, inverse = TRUE)
rp_raster_mask4 <-mask(rp_raster_mask3, reefs, inverse = TRUE)



#Final Output
writeRaster(rp_raster_mask4, filename = "bcstock_rf_fnl_output_april_cut.tif", overwrite = TRUE)





