# 7/24/2019 All code fully functional, and integrates with ArcGIS nicely to produce a SDWIS.gdb
# 10/26/2020 added report print out at the end
# 5/24/2022 new SDWIS link

#This code represents a workflow for compiling SDWIS data trimming, spatializing, and exporting for upload to ArcOnline SWAP Viewer

#Data is pulled from below URL and digested into feature classes and eventually pushed to the SWAP viewer: https://cdpheapp002/cdphe_swap_data_viewer/

#need to be connected to network vpn or in office
##########################################################################################################
#load excel file from SDWIS: http://cdphesqp105/Reports/report/DrinkingWaterReports/Inventory/Facility_Info

#facility type = ALL
#PWS Status = A - Active & I - Inactive
# Facility Status = A - Active & I - Inactive
#include locational data (YES)
#download report Excel.xlsx


#set working directory, included hard path and interface to choose directory
setwd("C:/Users/awitt/Desktop/SDWIS updates")
#alt: set directory
#setwd(choose.dir())
library(readxl)

#interface to choose where the SDWIS excel file is located. 
data <- read_excel(file.choose(), skip = 2)
#View(data)

#any errors that show up are from within the database and the goal here isn't database maintenance
##########################################################################################################

#build a new dataframe, only for the things we need
new_data <- data.frame(data$`PWS ID`, data$`System Name`, data$`Facility ID`, data$`Facility Name`, data$`Facility Type`, data$`Facility Active Status` ,data$`Facility Water Type`, data$`CDPHE Verified`, data$`Latitude Decimal Degrees`, data$`Longitude Decimal Degrees`)

#fix up the names
names(new_data)[1] <- "PWS_ID"
names(new_data)[2] <- "System_Name"
names(new_data)[3] <- "Facility_ID"
names(new_data)[4] <- "Facility_Name"
names(new_data)[5] <- "Facility_Type"
names(new_data)[6] <- "Facility_Active_Status"
names(new_data)[7] <- "Facility_Water_Type"
names(new_data)[8] <- "CDPHE_Verified"
names(new_data)[9] <- "Latitude_DD"
names(new_data)[10] <- "Longitude_DD"

new_data$Latitude <- new_data$Latitude_DD
new_data$Longitude <- new_data$Longitude_DD


##########################################################################################################
# now were going to start spatializing the data
library(maptools)
library(rgdal)
library(sp)
library(raster)

#omit missing lat long data (Na.omit culled too many, so I chose a different approach)
new_data_naomit <- (subset(new_data, new_data$Latitude_DD >0 & new_data$Longitude_DD <0))

#now assign coordinates and create spatial data frame
coordinates(new_data_naomit) = ~ Longitude_DD + Latitude_DD


#define coordinate system, using DD from GPS so we want WGS84
proj4string(new_data_naomit) <- CRS("+proj=longlat + datum=WGS84")
#testing the below line (it works)
#new_data_naomit <- spTransform(new_data_naomit, CRS("+init=epsg:3857"))


#save the new data into a shapefile (intermediate step, but by doing this here the subset below is easier)
require(raster)
shapefile(new_data_naomit, 'sdwis_update.shp', overwrite = TRUE)
spatial_new_data <- shapefile('sdwis_update.shp')

#warning messages produced: but output is correct

#rename the files again, thanks ESRI...
names(spatial_new_data)[1] <- "PWS_ID"
names(spatial_new_data)[2] <- "System_Name"
names(spatial_new_data)[3] <- "Facility_ID"
names(spatial_new_data)[4] <- "Facility_Name"
names(spatial_new_data)[5] <- "Facility_Type"
names(spatial_new_data)[6] <- "Facility_Active_Status"
names(spatial_new_data)[7] <- "Facility_Water_Type"
names(spatial_new_data)[8] <- "CDPHE_Verified"

names(spatial_new_data)[9] <- "Latitude"
names(spatial_new_data)[10] <- "Longitude"

#now subset out each Facility Type into unique shapefiles (still intermediate, but this helps if the gdb export gets buggy, thanks ESRI...)

#active wells
WL_active <- subset(spatial_new_data, Facility_Type == 'WL' & Facility_Active_Status == 'A')
shapefile(WL_active, 'active_wells.shp', overwrite = TRUE)
crs(WL_active) <- "epsg:4326"
#inactive wells
WL_inactive <- subset(spatial_new_data, Facility_Type == 'WL' & Facility_Active_Status == 'I')
shapefile(WL_inactive, 'inactive_wells.shp', overwrite = TRUE)
crs(WL_inactive) <- "+init=epsg:4326"
#infiltration gallery
IG <- subset(spatial_new_data, Facility_Type == 'IG')
shapefile(IG, 'infiltration_gallery.shp', overwrite = TRUE)
crs(IG) <- "+init=epsg:4326"
#intakes
IN <- subset(spatial_new_data, Facility_Type == 'IN')
shapefile(IN, 'intakes.shp', overwrite = TRUE)
crs(IN) <- "+init=epsg:4326"
#springs
SP <- subset(spatial_new_data, Facility_Type == 'SP')
shapefile(SP, 'springs.shp', overwrite = TRUE)
crs(SP) <- "+init=epsg:4326"
#storm tanks
ST <- subset(spatial_new_data, Facility_Type == 'ST')
shapefile(ST, 'storage_tanks.shp', overwrite = TRUE)
crs(ST) <- "+init=epsg:4326"
#water treatment plants
TP <- subset(spatial_new_data, Facility_Type == 'TP')
shapefile(TP, 'water_treatment_plants.shp', overwrite = TRUE)
crs(TP) <- "+init=epsg:4326"

##########################################################################################################

#A cleaner way to distribute the data is to load the shapefiles as feature classes and add to a Geodatabase
#installed the arcgisbinding (works outside of the CRAN system, so you can't install.packages, instead Tools >Install Packages> pick zip file)
library(arcgisbinding)
#verifies the license, but you must call the library funciton first, thanks ESRI...
arc.check_product()

#create .gdb to save feature classes into, it won't show up until the first feature class is added
fgdb_path <- file.path("C:/Users/awitt/Desktop/SDWIS updates", "SDWIS_update.gdb")
#alt: choose where to save .gdb 
#fgdb_path <- file.path(file.choose(), "SDWIS_update.gdb")

#create individual feature classes, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "Active_Wells"), data = WL_active)
arc.write(file.path(fgdb_path, "Inactive_Wells"), data = WL_inactive)
arc.write(file.path(fgdb_path, "Infiltration_Gallery"), data = IG)
arc.write(file.path(fgdb_path, "Intakes"), data = IN)
arc.write(file.path(fgdb_path, "Springs"), data = SP)
arc.write(file.path(fgdb_path, "Storage_Tanks"), data = ST)
arc.write(file.path(fgdb_path, "Water_Treatment_Plants"), data = TP)

##########################################################################################################

#finally (outside of RStudio),copy the full .gdb into appropriate directory for upload to SWAP Viewer

####################################################################################################################


#  R  E  P  O  R  T  # 

print(paste("WaterTreatmentPlants", nrow(TP), sep = ": "))        #3309
print(paste("StorageTanks", nrow(ST), sep = ": "))                #3125
print(paste("Springs", nrow(SP), sep = ": "))                     #143
print(paste("Intakes", nrow(IN), sep = ": "))                     #498
print(paste("InfiltrationGallery", nrow(IG), sep = ": "))         #22
print(paste("InactiveWells", nrow(WL_inactive), sep = ": "))      #813
print(paste("ActiveWells", nrow(WL_active), sep = ": "))          #3587




#COMPLETE

# now need to go reproject to web mercator in arcgis (the R code for web mercator epsg:3857 is causing problems)
# known bug where arc.write doesn't replicate rgdal epsg codes correctly, this is the work around (mega eye roll)





############################################## extra code in case #########################################################

#new addition?, need to convert each shapefile from WGS84 to Web Mercator to match the web map projections
#unfortunately this only seems to work with the "sf" package, which creates other headaches for putting shapefiles into feature classes
setwd("C:/Users/awitt/Desktop/SDWIS updates")

#taking existing shapefiles (created above)
active_wells <- st_read("C:/Users/awitt/Desktop/SDWIS updates/active_wells.shp")
#converting from epsg: 4326 to epsg:3857 (web mercator)
active_wells_Web <- st_transform(active_wells, 3857)
#exporting as shapefile
st_write(active_wells_Web, "C:/Users/awitt/Desktop/SDWIS updates/active_wells_Web.shp", delete_layer = TRUE)