#######  Instructions: https://docs.google.com/document/d/1ON_ZPPG6Oit9esj8rwS9Mb1c7xsaiz-zO3cp9p0bdFw/

#need to be connected to network vpn or in office with all network folders connected

##### import permits extract #####

#### everything now works in 64 bit R 4.3.1

#set working directory, included hard path and interface to choose directory
#setwd("I:/PERMITS/PERMIT_EXTRACTS")
#alt: set directory
#setwd(choose.dir())
library(readxl)
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(RODBC)
library(svDialogs)



#NOTE: 12/8/2022: reading in the spreadsheet from .xlsx is causing issues with fields coming into R, specifically SWConstructionActivity, SWConstructionTotalAcres, SWConstructionDistrubedAcres, SWConstructionStartDate, SWConstructionEndDate
# WORKAROUND: save down the permits extract as a .csv and use that in this script: lines 42-42

#load the spreadsheet (takes forever wtf) trying to specify col_types would need me to specify all 180+ columns
#data <- read_excel(file.choose(), col_types = c("text", "text", "text", "text", "date", "date", "date", "date"), guess_max=21474836)

#importing as Permits extract as csv is the workaround, but is there a way to automate the .xlsx to .csv conversion?
setwd("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/PCAT Automation")

data <- read.csv(file.choose(), fileEncoding = "latin1")


#Specify update month and Year

#manually enter date here, #hashing out for now in favor of below prompted date entry
#date <- "Aug2023"


#prompted date entry here, both month and year are treated as "strings", but that's what we want!
date_month <- svDialogs::dlgInput("Enter Month of Update as First Three Letters (Ex: Jan)", Sys.info()["user"])$res
date_year <- svDialogs::dlgInput("Enter Year of Update (Ex: 2000)", Sys.info()["user"])$res

date <- paste(date_month, date_year, sep = "")



# need a special date for the COR400 terminated search window later
term_date <- as.Date(paste0("01", date), format = "%d%b%Y")
term_date <- seq(term_date, length = 2, by = "-6 months")[2]

#summary(data)
print(categories <- unique(data$GeneralPermitType))

# All Permits: subset the data from 200+ columns down, create new cleaner dataframe

new_data <- data.frame(data$PermitSector, data$GeneralPermitType, data$PermitID, data$PreviousPermitID, data$EffectiveDate, data$Permittee, data$FacilityName,
                       data$FacilityAddress1, data$FacilityCity, data$FacilityState, data$FacilityZip, data$FacilityCounty, data$FacilityLatitude,
                       data$FacilityLongitude, data$LegalFName, data$LegalLName, data$LegalTitle, data$LegalAddress1, data$LegalCity, data$LegalState,
                       data$LegalZipcode, data$LegalPhone, data$LegalEmail, data$FacilityContactOrganization, data$FacilityContactFName, data$FacilityContactLName,
                       data$FacilityContactTitle, data$FacilityContactAddress1, data$FacilityContactCity, data$FacilityContactState, data$FacilityContactZipcode, 
                       data$FacilityContactPhone, data$FacilityContactEmail, data$FormerPermittee, data$ImmediateWater, data$ReceivingWater, data$StreamSegment,
                       data$MajorRiverBasin, data$SWConstructionActivity, data$SWConstructionTotalAcres, data$SWConstructionDisturbedAcres, data$SWConstructionStartDate,
                       data$SWConstructionEndDate, data$PropertyOwnerOrganization, data$PropertyOwnerFName, data$PropertyOwnerLName, data$PropertyOwnerTitle,
                       data$PropertyOwnerAddress1, data$PropertyOwnerCity, data$PropertyOwnerState, data$PropertyOwnerZipcode, data$PropertyOwnerPhone, data$PropertyOwnerEmail, data$PermitStatus, data$TerminationDate)


#fix up the names (there has to be a better way for naming a new data frame)
names(new_data)[1] <- "PermitSector"
names(new_data)[2] <- "GeneralPermitType"
names(new_data)[3] <- "PermitID"
names(new_data)[4] <- "PreviousPermitID"
names(new_data)[5] <- "EffectiveDate"
new_data$EffectiveDate <- as.Date(new_data$EffectiveDate, format = "%m/%d/%Y")
names(new_data)[6] <- "Permittee"
names(new_data)[7] <- "FacilityName"
names(new_data)[8] <- "FacilityAddress1"
names(new_data)[9] <- "FacilityCity"
names(new_data)[10] <- "FacilityState"
names(new_data)[11] <- "FacilityZip"
names(new_data)[12] <- "FacilityCounty"
names(new_data)[13] <- "FacilityLatitude"
names(new_data)[14] <- "FacilityLongitude"
names(new_data)[15] <- "LegalFName"
names(new_data)[16] <- "LegalLName"
names(new_data)[17] <- "LegalTitle"
names(new_data)[18] <- "LegalAddress1"
names(new_data)[19] <- "LegalCity"
names(new_data)[20] <- "LegalState"
names(new_data)[21] <- "LegalZipcode"
names(new_data)[22] <- "LegalPhone"
names(new_data)[23] <- "LegalEmail"
names(new_data)[24] <- "FacilityContactOrganization"
names(new_data)[25] <- "FacilityContactFName"
names(new_data)[26] <- "FacilityContactLName"
names(new_data)[27] <- "FacilityContactTitle"
names(new_data)[28] <- "FacilityContactAddress1"
names(new_data)[29] <- "FacilityContactCity"
names(new_data)[30] <- "FacilityContactState"
names(new_data)[31] <- "FacilityContactZip"
names(new_data)[32] <- "FacilityContactPhone"
names(new_data)[33] <- "FacilityContactEmail"
names(new_data)[34] <- "FormerPermittee"
names(new_data)[35] <- "ImmediateWater"
names(new_data)[36] <- "ReceivingWater"
names(new_data)[37] <- "StreamSegment"
names(new_data)[38] <- "MajorRiverBasin"
names(new_data)[39] <- "SWConstructionActivity"
names(new_data)[40] <- "SWConstructionTotalAcres"
names(new_data)[41] <- "SWConstructionDisturedAcres"
names(new_data)[42] <- "SWConstructionStartDate"
new_data$SWConstructionStartDate <- as.Date(new_data$SWConstructionStartDate, format = "%m/%d/%Y")
names(new_data)[43] <- "SWConstructionEndDate"
new_data$SWConstructionEndDate <- as.Date(new_data$SWConstructionEndDate, format = "%m/%d/%Y")
names(new_data)[44] <- "PropertyOwnerOrganization"
names(new_data)[45] <- "PropertyOwnerFName"
names(new_data)[46] <- "PropertyOwnerLName"
names(new_data)[47] <- "PropertyOwnerTitle"
names(new_data)[48] <- "PropertyOwnerAddress1"
names(new_data)[49] <- "PropertyOwnerCity"
names(new_data)[50] <- "PropertyOwnerState"
names(new_data)[51] <- "PropertyOwnerZipcode"
names(new_data)[52] <- "PropertyOwnerPhone"
names(new_data)[53] <- "PropertyOwnerEmail"
names(new_data)[54] <- "PermitStatus"
names(new_data)[55] <- "TerminationDate"
new_data$TerminationDate <- as.Date(new_data$TerminationDate, format = "%m/%d/%Y")



# now load in the access database where I can get information on whether each permit is inspected or not inspected

##################### import access database ###########################
# https://rpubs.com/vermanica/SQL_finalProject_MicrosoftAccess

# file path to access database
wb <- "I:/Data Management Group/PCAT Inspection Projects/PCAT Inspection Database backend/PCATInspectionsMaster_ver5.1.accdb"

# open connection to access database
channel <- odbcConnectAccess2007(wb)

# view existing tables within access database
tables <- sqlTables(channel)
tables$TABLE_NAME

# specify table of interest from access database, save as variable
tracker <- sqlFetch(channel, "PCATBE_ComplianceTracking")

tracker <- tracker[1:142]


# good idea to close all connections at end of R session
odbcCloseAll() 


#cut down to only needed data from complianceTracking
tracker <- data.frame(tracker$PermitID, tracker$Status)

tracker$tracker.Status[tracker$tracker.Status == 2] <- "Downloaded"
tracker$tracker.Status[tracker$tracker.Status == 3] <- "Pending"
tracker$tracker.Status[tracker$tracker.Status == 4] <- "Inspected"
tracker$tracker.Status[tracker$tracker.Status == 5] <- "CASE"
tracker$tracker.Status[tracker$tracker.Status == 6] <- "Closed"
tracker$tracker.Status[tracker$tracker.Status == 8] <- "Hold"


##########################################################################


#subsetting to Inspected, Closed, or CASE
inspected <- subset(tracker, tracker$tracker.Status == "Inspected" | tracker$tracker.Status == "Closed" | tracker$tracker.Status == "CASE")

#merging the inspected data into the larger dataset
new_data <- merge(new_data, inspected, by.x = "PermitID", by.y = "tracker.PermitID", all = TRUE)
names(new_data)[56] <- "InspectionStatus"

# adding tag for duplicates based on Facility Name and Facility Address
new_data$Duplicate <- c(duplicated(new_data$FacilityName, new_data$FacilityAddress1, fromLast = TRUE) | duplicated(new_data$FacilityName, new_data$FacilityAddress1))
new_data$Duplicate <-as.character(ifelse(new_data$Duplicate == TRUE, "Yes", "No"))

#split out CSEP from non-csep: NOTE this list is incomplete and subject to change, not exactly sure how to deal with that going forward
# if a new member is added and wasn't previously considered as a CSEP member, this code will miss those new members

#list of CSEP members for the imported permits extract
CSEP_members <- data.frame(sort(unique(new_data$Permittee)))

###### subsetting new_data to search for CSEP organizations #####
csep <- subset(new_data, grepl(glob2rx("*Adolfson and Peterson*"), Permittee) |
                         grepl(glob2rx("*Adolfson & Peterson*"), Permittee) |
                         grepl(glob2rx("*AP Mountain*"), Permittee) |
                         grepl(glob2rx("*Brinkman Co*"), Permittee) | 
                         grepl(glob2rx("*Fransen Pittman*"), Permittee) | 
                         grepl(glob2rx("*Fiore & Sons*"), Permittee) |
                         grepl(glob2rx("*Fiore and Sons*"), Permittee) |
                         grepl(glob2rx("*GH Phipps*"), Permittee) |
                         grepl(glob2rx("*Golden Triangle*"), Permittee) |
                         grepl(glob2rx("*GTC*"), Permittee) |
                         grepl(glob2rx("*Haselden Co*"), Permittee) |
                         grepl(glob2rx("*Howell Construction*"), Permittee) |
                         grepl(glob2rx("*James R Howell*"), Permittee) |
                         grepl(glob2rx("*Hyder Co*"), Permittee) |
                         grepl(glob2rx("*JHL Co*"), Permittee) |
                         grepl(glob2rx("*Milender White*"), Permittee) |
                         grepl(glob2rx("*MW Residential*"), Permittee) |
                         grepl(glob2rx("*PCL Co*"), Permittee) |
                         grepl(glob2rx("*PCl Co*"), Permittee) |
                         grepl(glob2rx("*Swinerton*"), Permittee) |
                         grepl(glob2rx("*Beck Group*"), Permittee) |
                         grepl(glob2rx("*Waner Con*"), Permittee)
            )


# now remove the csep entries from the new_data sheet
new_data2 <- dplyr::setdiff(new_data, csep)

##### need to fix Legal Zip and Phone again, for new_data2
#zips are easy, lake left 5 characters
new_data2$LegalZipcode <- as.character(substring(new_data2$LegalZipcode, 1, 5))
new_data2$FacilityZip <- as.character(substring(new_data2$FacilityZip, 1, 5))

#remove non-numeric characters from phone numbers, ie " ", "(", ")", "-", "."
new_data2$LegalPhone <- gsub("[ ()-.]", "", new_data2$LegalPhone)
new_data2$FacilityContactPhone <- gsub("[ ()-.]", "", new_data2$FacilityContactPhone)

#now can take the left 10 numbers to get a clean phone number
new_data2$LegalPhone <- as.character(substring(new_data2$LegalPhone, 1, 10))
new_data2$FacilityContactPhone <- as.character(substring(new_data2$FacilityContactPhone, 1, 10))



###### write spreadsheets #####

#write non_csep
write.csv(new_data2, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/permits_", date, ".csv", sep = ""))
#write csep
csep_sheet <- subset(csep, PermitStatus != "Terminated" & PermitStatus != "Withdrawn" & PermitStatus != "Expired")
write.csv(csep_sheet, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/csep_", date, ".csv", sep = ""))

#write dewatering
dewatering_sheet <- subset(new_data2, new_data2$GeneralPermitType == "COG070000-Construction dewatering" | 
                                      new_data2$GeneralPermitType == "COG080000-Construction dewatering" |
                                      new_data2$GeneralPermitType == "COG315000-Remediation activities discharging to surface water" |
                                      new_data2$GeneralPermitType == "COG316000-Remediation activities discharging to ground water" | 
                                      new_data2$GeneralPermitType == "COG317000-Short-Term Remediation Activities" |
                                      new_data2$GeneralPermitType == "COG318000-Long-Term Remediation Activities")

dewatering_sheet <- subset(dewatering_sheet, PermitStatus != "Terminated" & PermitStatus != "Withdrawn" & PermitStatus != "Expired" & PermitStatus != "Denied")

write.csv(dewatering_sheet, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/DewateringRemediation_", date, ".csv", sep = ""))

###### spatialize for non_csep #####

#clean up lat long fields and ensure they're plotting in Colorado, or that NA's don't get lost. they plot to null island :(
new_data2$FacilityLatitude <- abs(as.numeric(as.character(new_data2$FacilityLatitude)))
new_data2$FacilityLatitude <- ifelse(new_data2$FacilityLatitude < 30, 0.0001, new_data2$FacilityLatitude)

new_data2$FacilityLongitude <- abs(as.numeric(as.character(new_data2$FacilityLongitude)))*-1
new_data2$FacilityLongitude <- ifelse(new_data2$FacilityLongitude > -80, -0.0001, new_data2$FacilityLongitude)


#omit missing lat long data and assign spatial X Y
new_data2 <- (subset(new_data2, FacilityLatitude >0 & FacilityLongitude <0))
coordinates (new_data2) = ~ FacilityLongitude + FacilityLatitude

#define coordinate system, using DD from GPS so we want WGS84
proj4string(new_data2) <- CRS("+proj=longlat + datum=WGS84")

# still fighting with the effective and termination dates, so I'll hard code them again
new_data2@data$EffectiveDate <- as.character(new_data2$EffectiveDate)
new_data2@data$TerminationDate <- as.character(new_data2$TerminationDate)
new_data2@data$SWConstructionStartDate <- as.character(new_data2$SWConstructionStartDate)
new_data2@data$SWConstructionEndDate <- as.character(new_data2$SWConstructionEndDate)



##### export to geodatabase #####

#save into new geodatabase
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/PCAT Automation", paste("PCAT_Updates",".gdb", sep = ""))

#export to shapefile: construction stormwater: Active
cor400 <- subset(new_data2, new_data2$GeneralPermitType == "COR400000-Stormwater discharge associated with construction activities" & (new_data2$PermitStatus == "Admin continued" | new_data2$PermitStatus == "Effective"))
arc.write(file.path(fgdb_path, "COR400"), data = cor400)

#export to shapefile: construction stormwater: Terminated in last 6 months
cor400_term <- subset(new_data2, new_data2$GeneralPermitType == "COR400000-Stormwater discharge associated with construction activities" & (new_data2$PermitStatus == "Terminated") & new_data2$TerminationDate > term_date)
arc.write(file.path(fgdb_path, "COR400_terminated"), data = cor400_term)

#create new_data3 to remove terminated, withdrawn, etc permits for the following dewatering and remediation
new_data3 <- subset(new_data2, PermitStatus != "Terminated" & PermitStatus != "Withdrawn" & PermitStatus != "Expired" & PermitStatus != "Denied")


# export prioritization terminated
cor400_term <- subset(data.frame(cor400_term), PermitStatus == "Terminated")
write.csv(cor400_term, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/prioritizationTerminated_", date, ".csv", sep = ""))

#export csep terminated
csep_term <- subset(data.frame(csep), PermitStatus == "Terminated")
write.csv(csep_term, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/csepTerminated_", date, ".csv", sep = ""))



#export to shapefile: construction dewatering NO LONGER AN ACTIVE PERMIT TYPE
#cog070 <- subset(new_data2, new_data2$GeneralPermitType == "COG070000-Construction dewatering")
#arc.write(file.path(fgdb_path, "COG070"), data = cog070)

cog080 <- subset(new_data3, new_data3$GeneralPermitType == "COG080000-Construction dewatering")
arc.write(file.path(fgdb_path, "COG080"), data = cog080)

#export to shapefile: remediation to surface water
cog315 <- subset(new_data3, new_data3$GeneralPermitType == "COG315000-Remediation activities discharging to surface water")
arc.write(file.path(fgdb_path, "COG315"), data = cog315)

cog316 <- subset(new_data3, new_data3$GeneralPermitType == "COG316000-Remediation activities discharging to ground water")
arc.write(file.path(fgdb_path, "COG316"), data = cog316)

#export to shapefile: short/long term remediation
cog317 <- subset(new_data3, new_data3$GeneralPermitType == "COG317000-Short-Term Remediation Activities")
arc.write(file.path(fgdb_path, "COG317"), data = cog317)

cog318 <- subset(new_data3, new_data3$GeneralPermitType == "COG318000-Long-Term Remediation Activities")
arc.write(file.path(fgdb_path, "COG318"), data = cog318)

#export to shapefile: Hydrostatic testing of pipelines, tanks, 
cog604 <- subset(new_data3, new_data3$GeneralPermitType == "COG604000-Hydrostatic testing of pipelines, tanks and similar vessels")
arc.write(file.path(fgdb_path, "COG604"), data = cog604)

date_stamp <- paste("lastupdated", date, sep = " ")
arc.write(file.path(fgdb_path, "lastupdated"), data = date_stamp, coords = list((x=0.0001), y=(-0.0001)), shape_info = list(type='Point'))


##### spatialize for csep #####

#clean up lat long fields and ensure they're plotting in Colorado, or that NA's don't get lost. they plot to null island :(
csep$FacilityLatitude <- abs(as.numeric(as.character(csep$FacilityLatitude)))
csep$FacilityLatitude <- ifelse(csep$FacilityLatitude < 30, 0.0001, csep$FacilityLatitude)

csep$FacilityLongitude <- abs(as.numeric(as.character(csep$FacilityLongitude)))*-1
csep$FacilityLongitude <- ifelse(csep$FacilityLongitude > -80, -0.0001, csep$FacilityLongitude)


#omit missing lat long data and assign spatial X Y
csep <- (subset(csep, FacilityLatitude >0 & FacilityLongitude <0))
coordinates (csep) = ~ FacilityLongitude + FacilityLatitude

#define coordinate system, using DD from GPS so we want WGS84
proj4string(csep) <- CRS("+proj=longlat + datum=WGS84")

# convert back the dates for shapefile
csep@data$EffectiveDate <- as.character(csep$EffectiveDate)
csep@data$TerminationDate <- as.character(csep$TerminationDate)

#save into existing geodatabase

csep_active <- subset(csep, PermitStatus != "Terminated" & PermitStatus != "Withdrawn" & PermitStatus != "Expired")
arc.write(file.path(fgdb_path, "CSEP"), data = csep_active)

csep_term <-subset(csep, PermitStatus == "Terminated")
arc.write(file.path(fgdb_path, "CSEP_terminated"), data = csep_term)



##### prioritization for construction stormwater COR400 #####


#### everything now works in 64 bit R 4.3.1



library(rgdal)
library(raster)
library(spatialEco)
library(sf)

fgdb <- "C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/PCAT Automation/PCAT_Updates.gdb"

cor400 <- readOGR(dsn = fgdb, layer = "COR400")
#nrow(cor400@data)

#here I did some arcgis prep to bring in the a layer used for analysis: 1st created buffered hammer streams to .25 miles, spatial joined with strahler stream order, projected to WGS84
#streams <- shapefile("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/PCAT Automation/streamsBuf025mi_Strahler.shp")
streams <- read_sf("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/PCAT Automation/streamsBuf025mi_Strahler.shp")

#convert spatial objects to sf for join
cor400 <- st_as_sf(cor400)
streams <- st_as_sf(streams)

#spatial join stream (analyte, strahler, buffered streams) to cor400 points
join_streams <- st_join(cor400, streams)
join_streams$SWConstructionDisturedAcres <- as.numeric(join_streams$SWConstructionDisturedAcres)


# Calculate Priority Score (1. within 0.25 mi of surface water, +3
join_streams$Join_Count <- as.numeric(join_streams$Join_Count)
join_streams$buffer_score <- as.numeric(ifelse(join_streams$Join_Count >= 1, 3, 0))

# Calculate Priority Score (2. Surface water impaired by selenium, sediment, Macros, +4

    # Macros query: 303(d) : Bugs | 303(d) : Bugs(P) 
    join_streams$bugScore <- ifelse((grepl(glob2rx("*303(d) : Bugs*"), join_streams$Analyte)), 4, 0)

    # Selenium query: 303(d) : Se-T | 303(d) : Se-D
    join_streams$seScore <- ifelse((grepl(glob2rx("*303(d) : Se-*"), join_streams$Analyte)), 4, 0)
    
    # Sediment query: 303(d) : Sediment
    join_streams$sedScore <- ifelse((grepl(glob2rx("*303(d) : Sediment*"), join_streams$Analyte)), 4, 0)
    
    #combine all scores
    join_streams$analyte_score <- join_streams$bugScore + join_streams$seScore + join_streams$sedScore
    join_streams$analyte_score <- ifelse(join_streams$analyte_score >= 4, 4, 0)

# Calculate Priority Score (3. Strahler >= 4, +1 
join_streams$strahler_score <- ifelse(join_streams$StreamOrde >=4, 1, 0)

# Calculate Priority Score (4. Disturbance acreage > 5acres & <200 acres, +2
join_streams$disturb_score <- ifelse(join_streams$SWConstructionDisturedAcres >5 & join_streams$SWConstructionDisturedAcres <200, 2, 0)



# Calculate Priority Score (Final Score. Buffer + Analytes + Strahler + Disturbances
join_streams$Priority <- join_streams$buffer_score + join_streams$analyte_score + join_streams$strahler_score + join_streams$disturb_score

#replacing NA values in the priority score calculation with 0s
join_streams$Priority[is.na(join_streams$Priority)] <- 0





##### Clean Up Fields in Final Layer before export ######
# removing fields 52 through 105, all things that came from the hammer and the above intermediate analysis
export <- join_streams[c(1:55, 111)]

# to remove duplicates I need to go from spatial to dataframe and back to spatial
###### write prioritization spreadsheet #####

#converting spatial to normal dataframe
export2 <- export

# remove confluence duplicates from priority score
export3 <-export2[order(export$PermitID, -abs(export$Priority)), ]
export3 <- export3[!duplicated(export3$PermitID), ]
export3 <- export3[c(1:56)]

###### export spreadsheets ######

#export prioritization
write.csv(export3, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/prioritization_", date, ".csv", sep = ""))


#save into new geodatabase
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/PCAT Automation", paste("PCAT_Updates",".gdb", sep = ""))


#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "COR400_priority"), data = export3)


##### now they want a spreadsheet of COG500s and COR900s, which I'll pull from the main L geodatabase on permit codes
fgdb <- "L:/Permits/BigMap/PermitsUpdates.gdb"

cog500 <- readOGR(dsn = fgdb, layer = "COG500")
cog500 <- data.frame(cog500)
write.csv(cog500, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/cog500_", date, ".csv", sep = ""))

cor900 <- readOGR(dsn = fgdb, layer = "COR900")
cor900 <- data.frame(cor900)
write.csv(cor900, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/cor900_", date, ".csv", sep = ""))

conox <- readOGR(dsn = fgdb, layer = "CONO_NoExposure")
conox <- data.frame(conox)
write.csv(conox, paste("W:/Permits/PCAT Workgroup/Database Spreadsheets/Prioritization Automation Updates/conox_", date, ".csv", sep = ""))



###### TO DO, remaining steps #####
# change file paths for output data and spreadsheets
# refine code for quick completion (date)












##### code notes #####

#Compliance Automation 1: Spatialize permits extract by permit type (Non CSEP) 
# - add inspected vs non inspected tag, scrub fields

#Compliance Automation 2: Spatialize permits extract by CSEP only

#Compliance Automation 3: For Construction Stormwater, apply prioritization
# - 1. within 0.25 mi of surface water, +3
# - 2. surface water impaired with selenium, sediment, or Macros, +4
# - 3. Strahler >= 4, +1
# - 4. disturbance acreage > 5 acres & < 200 acres, +2


