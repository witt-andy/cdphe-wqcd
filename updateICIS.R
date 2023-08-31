# 8/6/2019 update ICIS for quarterly refresh
# 10/26/2020 added report print out at the end

#This code represents a workflow for compiling ICIS data trimming, spatializing, and exporting for upload to ArcOnline SWAP Viewer

#Data comes from ICIS and is provided by david.kurz@state.co.us on a quarterly basis

#Follows the same process as the updateSDWIS.R workflow

#need to be connected to network vpn or in office
##########################################################################################################
#set working directory, included hard path and interface to choose directory
setwd("C:/Users/awitt/Desktop/SDWIS updates")
#alt: set directory
#setwd(choose.dir())
library(readxl)

data <- read_excel(file.choose(), skip = 3)
#View(data)

#rename long, lat column names
names(data)[1] <- "NPDES ID"
names(data)[2] <- "Permit Status"
names(data)[3] <- "Permit Name"
names(data)[4] <- "Facility Name"
names(data)[5] <- "SIC Code"
names(data)[6] <- "Latitude_DD"
names(data)[7] <- "Longitude_DD"
names(data)[8] <- "Major or Minor"

new_data <- data.frame(data$`NPDES ID`, data$`Permit Name`, data$`Permit Status`, data$`Facility Name`, data$`SIC Code`, data$Latitude_DD, data$Longitude_DD, data$`Major or Minor`)

names(new_data)[1] <- "NPDES ID"
names(new_data)[2] <- "Permit Status"
names(new_data)[3] <- "Permit Name"
names(new_data)[4] <- "Facility Name"
names(new_data)[5] <- "SIC Code"
names(new_data)[6] <- "Latitude_DD"
names(new_data)[7] <- "Longitude_DD"
names(new_data)[8] <- "Major or Minor"

new_data$Longitude_DD <- as.numeric(as.character(new_data$Longitude_DD))
new_data$Latitude_DD <- as.numeric(as.character(new_data$Latitude_DD))

new_data$Longitude <- new_data$Longitude_DD
new_data$Latitude <- new_data$Latitude_DD

names(new_data)[9] <- "Longitude"
names(new_data)[10] <- "Latitude"



library(maptools)
library(rgdal)
library(sp)
library(raster)

#must remove NAs for coordinates() to work
new_data <- (subset(new_data, Latitude_DD >0 & Longitude_DD <0))

coordinates(new_data) = ~ Longitude_DD + Latitude_DD

proj4string(new_data) <- CRS("+proj=longlat + datum=WGS84")

library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/SDWIS updates", "ICIS_update.gdb")
#fgdb_path <- file.path(file.choose(), "ICIS_update.gdb")
arc.write(file.path(fgdb_path, "swap_colorado_wastewater_treatment_plants"), data = new_data)

#########################################
##### COX: discharge to groundwater #####

data_cox <- read_excel(file.choose(), sheet = 2, skip = 1)
#View(data_cox)

names(data_cox)[1] <- "NPDES ID"
names(data_cox)[2] <- "Permit Name"
names(data_cox)[3] <- "Permit Status"
names(data_cox)[4] <- "Facility Name"
names(data_cox)[5] <- "SIC Code"
names(data_cox)[6] <- "Latitude_DD"
names(data_cox)[7] <- "Longitude_DD"
names(data_cox)[8] <- "Major or Minor"

data_cox$Longitude_DD <- as.numeric(as.character(data_cox$Longitude_DD))
data_cox$Latitude_DD <- as.numeric(as.character(data_cox$Latitude_DD))

data_cox$Longitude <- data_cox$Longitude_DD
data_cox$Latitude <- data_cox$Latitude_DD

names(data_cox)[9] <- "Longitude"
names(data_cox)[10] <- "Latitude"

library(maptools)
library(rgdal)
library(sp)
library(raster)

#must remove NAs for coordinates() to work
data_cox <- (subset(data_cox, Latitude_DD >0 & Longitude_DD <0))

coordinates(data_cox) = ~ Longitude_DD + Latitude_DD

proj4string(data_cox) <- CRS("+proj=longlat + datum=WGS84")

library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/SDWIS updates", "ICIS_update.gdb")
#fgdb_path <- file.path(file.choose(), "ICIS_update.gdb")
arc.write(file.path(fgdb_path, "swap_discharge_to_groundwater"), data = data_cox)


#  R  E  P  O  R  T  # 

print(paste("WasteWater", nrow(new_data), sep = ": "))       #555
print(paste("Discharge to GW", nrow(data_cox), sep = ": "))  #117



