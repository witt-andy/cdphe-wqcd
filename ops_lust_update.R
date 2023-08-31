# Pulling OPS Lust data from web to update permit layers

#Data is pulled from URL: https://data.colorado.gov/Environment/Petroleum-Release-Events-in-Colorado-Oil-Public-Sa/iimu-7683
# export as .csv

# this update should probably take place alongside the permits update: C:/Users/awitt/Desktop/permit updates/permit_updates.R

#need to be connected to network vpn or in office
#####################################################################################################################################################

#set working directory, included hard path and interface to choose directory
setwd("C:/Users/awitt/Desktop/permits updates")
#alt: set directory
#setwd(choose.dir())
library(readxl)
library(maptools)
library(rgdal)
library(sp)
library(raster)

#load the spreadsheet (takes forever wtf)
data <- read.csv(file.choose(), fileEncoding = "latin1")

#Specify update month and Year
date <- "Aug2023"

#####################################################################################################################################################
#####################################################################################################################################################

#build data frame and prep for shapefile export
#as a published data source I'm not going to correct errors in Lat/Long entries, I will not include lat long processing here

new_data <- data.frame(data)

#do need to change NA Lat/Long to 0,0 so we don't lose attributes 

new_data$Latitude <- ifelse(is.na(new_data$Latitude), 0.0001, new_data$Latitude)
new_data$Longitude <- ifelse(is.na(new_data$Longitude), -0.0001, new_data$Longitude)

#omit missing lat long data and assign spatial X Y
coordinates (new_data) = ~ Longitude + Latitude

#define coordinate system, using DD from GPS so we want WGS84
proj4string(new_data) <- CRS("+proj=longlat + datum=WGS84")

#save into new geodatabase
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/permits updates/OPSlust.gdb")

#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "LUST_All"), data = new_data)

date_stamp <- paste("lastupdated", date, sep = " ")
arc.write(file.path(fgdb_path, "lastupdated"), data = date_stamp, coords = list((x=0.0001), y=(-0.0001)), shape_info = list(type='Point'))

#successful



