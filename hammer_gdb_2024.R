
#READ ANNOTATIONS, THERE ARE LOTS OF TRICKS TO THIS
#TO BE SAFE I WORK OFF OF A LOCAL COPY AND NOT RIGHT FROM L DRIVE
#MUST CREATE NEW LISTCHANGE -> LISTCHANGE2
#MUST CREATE NEW AUID_STATUS -> AUID_STATUS2
# ADDING (LISTCHANGE2 and AUIDSTATUS2) AS ROBERT MANIPULATIONS MAKE THE ORIGINAL VERSIONS UNACCESSIBLE IN R (UNKNOWN CAUSE)


############################ setup ###############################################
#############################setup ##################################################

#setup
  
#installed the arcgisbinding (works outside of the CRAN system, so you can't install.packages, instead Tools >Install Packages> pick zip file)
library(rgdal)
library(foreign)
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
library(arcgisbinding)
#verifies the license, but you must call the library funciton first, thanks ESRI...
arc.check_product()

#link to hybrid
#hybrid_ir <- "L:/EDU/Shape/HybridProject/DO_NOT_EDIT/HybridIR.gdb"
hybrid_ir <- "C:/Users/awitt/Desktop/HammerExport/HybridIR.gdb"
setwd(hybrid_ir)

#list all feature classes in .gdb
#subset(ogrDrivers(), grepl("GDB", name))
hybrid_list <- ogrListLayers(hybrid_ir)
print(hybrid_list)
########################end setup ########################################

#Specify cycle year
year <- "2024"

######################## section 1 ###########################################
#create analytes field

#when robert created the new list change there is a file typing issue where R could not read ListChange
#solved with export to geodatabase and renamed ListChange -> ListChange2


#start by loading list change, one to many join new fields of Cat 5, 4a, and 3b
library(sf)
data <- sf::st_read(dsn = hybrid_ir, "ListChange2")

#when robert created the new list change there is a file typing issue where R could not read ListChange
#solved with export to geodatabase and renamed ListChange -> ListChange2


#subset only the information we need, select correct Cycle, and Categories 5, and 3b, edit 5/19/2022 need to add in 4a to this code
data <-subset(data, data$Cycle == year)
data <-subset(data, data$Cat == "5" | data$Cat == "3b"  | data$Cat == "4a")


#compress only the needed information into a new data frame
data2 <-data.frame(data$AUID, data$Analyte, data$Cat)
data2$data.Cat <- as.character(data2$data.Cat)
data2$data.Cat[data2$data.Cat == "5"] <- "303(d)"
data2$data.Cat[data2$data.Cat == "3b"] <- "M&E"
data2$data.Cat[data2$data.Cat == "4a"] <- "TMDL"
data2$data.Full <- paste(data2$data.Cat, ":", data2$data.Analyte)

#must call to get only unique values needed for the below cast from long to wide, it won't work otherwise (there should be no loss of data only removing values that are duplicates, introduced from the 4a category)
data2 <- unique(data2)

#convert from a long format to a wide formatted data frame (deprecated)
library(reshape2)
hammer <- reshape2::dcast(data2, data.AUID ~ data.Full, value.var = "data.Full")


#clean up the data, outputs hammer analytes 
library(tidyr)
united_hammer <- unite(hammer, "Analyte", 2:ncol(hammer) ,na.rm = TRUE, sep = "; ")

############################section 1 end ##########################################
############################section 2 ##########################################

#build streams hammer


library(rgdal)
streams <- readOGR(hybrid_ir, layer= "Streams")

library(sf) # errors in this section are not an issue
auid_history <- st_read(hybrid_ir, "AUID_History")
#auid_history <- st_read(auid_history, auid_history$Retired == "N")

#need to export auid status like i did for listchange
auid_status <- st_read(hybrid_ir, "AUID_Status2")
auid_status <- subset(auid_status, auid_status$Cycle == year)

adb_history <- st_read(hybrid_ir, paste("ADBHistory", year, sep = ""))

#fix naming
names(united_hammer)[1] <- "AUID"


library(sp)

#merge all tables to make final hammer
tables_merge <- sp::merge(streams, adb_history, by.x = "SGEOID", by.y = "GEOID", duplicateGeoms = TRUE)
tables_merge <- sp::merge(tables_merge, auid_status, by.x = "Historic_A", by.y = "AUID", duplicateGeoms = TRUE)
tables_merge <- sp::merge(tables_merge, united_hammer, by.x = "Historic_A", by.y = "AUID", duplicateGeoms = TRUE)
tables_merge <- sp::merge(tables_merge, auid_history, by.x = "Historic_A", by.y = "AUID", duplicateGeoms = TRUE)


#mabye fix historic_A to AUID here?
names(tables_merge)[1] <- "AUID"


################################section 2 end #################################################################
#################################section 3 ##################################################################
#Export as shapefile

#library(raster)

#shapefile(tables_merge, paste("C:/Users/awitt/Desktop/HammerExport/StreamsHammer", year, ".shp", sep = ""), overwrite = TRUE)

#Export to geodatabase (Needed to preserve DESC full length)
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/HammerExport", paste("HammerUpdate",".gdb", sep = ""))

#create individual feature class, and puts them into the .gdb we just created

# getting topology error, but that's not suprising
arc.write(file.path(fgdb_path, "StreamsHammer2024"), data = tables_merge)



##################################section 3 end ##########################################################
##################################section 4 ###################################################################

#build lakes hammer

library(rgdal)
lakes <- readOGR(hybrid_ir, layer= "Lakes2")

library(sp)

tables_merge2 <- sp::merge(lakes, adb_history, by.x = "SGEOID", by.y = "GEOID", duplicateGeoms = TRUE)
tables_merge2 <- sp::merge(tables_merge2, auid_status, by.x = "Historic_A", by.y = "AUID")
tables_merge2 <- sp::merge(tables_merge2, united_hammer, by.x = "Historic_A", by.y = "AUID")
tables_merge2 <- sp::merge(tables_merge2, auid_history, by.x = "Historic_A", by.y = "AUID")

names(tables_merge2)[1] <- "AUID"


#Export to shapefile
library(raster)

#shapefile(tables_merge2, paste("C:/Users/awitt/Desktop/HammerExport/Lakes", year, "Hammer.shp", sep = ""), overwrite = TRUE)

#Export to geodatabase (Needed to preserve DESC full length)
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/HammerExport", paste("HammerUpdate",".gdb", sep = ""))

#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "Lakes2024Hammer"), data = tables_merge2)


####################################section 4 end ##############################################################
####################################section 5  #################################################################

# AD_Tiers should be within hybrid, but check for OW, Reviewable, and UP classifications from the standards DB, as of now this is added automatically


####################################section 5 end ##############################################################
####################################section 6  #################################################################

# here is some quick QA/QC ### reminder to load hammer as shapefile, due to my lazy coding below

old <- shapefile("C:/Users/awitt/Desktop/HammerExport/2022_update/StreamsHammer2022.shp")
new <- shapefile("C:/Users/awitt/Desktop/HammerExport/2024_update/StreamsHammer2024.shp")

#5
old5 <- print(nrow(subset(old, Cat == "5")))
new5 <- print(nrow(subset(new, Cat == "5")))

#4a 
old4a <- print(nrow(subset(old, Cat == "4a")))
new4a <- print(nrow(subset(new, Cat == "4a")))

#3b 
old3b <- print(nrow(subset(old, Cat == "3b")))
new3b <- print(nrow(subset(new, Cat == "3b")))

#3a
old3a <- print(nrow(subset(old, Cat == "3a")))
new3a <- print(nrow(subset(new, Cat == "3a")))

# 2a
old_2 <- print(nrow(subset(old, Cat == "2")))
new2 <- print(nrow(subset(new, Cat == "2")))

# 1b
old_1b <- print(nrow(subset(old, Cat == "1b")))
new1b <- print(nrow(subset(new, Cat == "1b")))

#1a
old_1a <- print(nrow(subset(old, Cat == "1a")))
new1a <- print(nrow(subset(new, Cat == "1a")))


# old nulls
print(full_old <- (nrow(old)) - old5 - old4a - old3b - old3a - old_2 - old_1b - old_1a)

# new nulls
print(full_new <- (nrow(new)) - new5 - new4a - new3b - new3a - new2 - new1b - new1a)




