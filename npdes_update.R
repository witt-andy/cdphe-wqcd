#This code represents a workflow for transforming permits extract data into NPDES outfall and WWTP spatial data, used in the Permit Contamination Map

#Data is provided by veronica.kenkel@state.co.us on a quarterly basis. no clear update timeline established

#pain in the ass to keep the 0,0 coordinates in here but they are in here to maintain an attributes presence, all other coordinates have a corrector to make sure they plot in CO

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

#load the spreadsheet (data directly provided from Veronica)
data <- read_excel(file.choose(), skip = 3) #who the fuck formats excel files like this?
summary(data)

#Specify update month and Year
date <- "Oct2022"

date_stamp <- paste("NPDESoutfalls_", date, sep = "")

data <- data.frame(data)

#####################################################################################################################################################
#####################################################################################################################################################

#following the permits_updates.R next we would rebuild the data frame, but because this is already queried, the data should already be culled

#now need to correct the horrenduous state of the lat long fields, the following correction CANNOT fix errors in entry, but it can greatly reduce error rates
#values are spit out into new columns for Lat/Long

#all latitudes will be taken as positive to remove errors on the +/-
#if latitudes are less than 1, values are shifted to 0.0001 (not 0 to stay positive) and shipped to null island
data$Latitude.in.Decimal.Degrees <-abs(as.numeric(data$Latitude.in.Decimal.Degrees))
data$Latitude.in.Decimal.Degrees <- ifelse(data$Latitude.in.Decimal.Degrees < 30, 0.0001, data$Latitude.in.Decimal.Degrees)

#all longitudes will be taken as negatives to remove errors in +/-
#if latitudes are greater than 1, values are shifted to -0.0001 (not 0 to stay negative) and shipped to null island
data$Longitude.in.Decimal.Degrees <- abs(as.numeric(data$Longitude.in.Decimal.Degrees))*-1
data$Longitude.in.Decimal.Degrees <- ifelse(data$Longitude.in.Decimal.Degrees > -80, -0.0001, data$Longitude.in.Decimal.Degrees)

#####################################################################################################################################################

#now we can do the real task at hand: identify outfalls
new_data <- subset(data, Perm.Feature.Type.Desc == 'External Outfall')

#now we will spatially plot these data based on the corrected Fields for Lat and Long
#omit missing lat long data and assign spatial X Y
new_data <- (subset(new_data, Latitude.in.Decimal.Degrees >0 & Longitude.in.Decimal.Degrees <0))

#remove terminated permits
new_data <- subset(new_data, new_data$Permit.Status.Desc != "Terminated")

coordinates(new_data) = ~Longitude.in.Decimal.Degrees + Latitude.in.Decimal.Degrees

#define coordinate system, using DD from GPS so we want WGS84
proj4string(new_data) <- CRS("+proj=longlat + datum=WGS84")

#save into new geodatabase
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/permits updates", paste("NPDESUpdates",".gdb", sep = ""))


#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, date_stamp), data = new_data)
