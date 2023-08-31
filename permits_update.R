#This code represents a workflow for transforming the permit extracts data into spatial data used in the Permit Contamination Map
#pain in the ass to keep the 0,0 coordinates in here but they are in here to maintain an attributes presence, all other coordinates have a fix to make sure they plot in CO

#Data is provided by veronica.kenkel@state.co.us via I:/PERMITS/PERMITS_EXTRACTS/Permit Extract with CEOS.xlsx

#keep in mind "pending" and other types of status aren't able to be distributed to the public, so this data can't be converted to a Web Service :(

#move layers from PermitsUPdate.gdb to L/Permits/BigMap and X drive as well

#need to be connected to network vpn or in office
#####################################################################################################################################################
#set working directory, included hard path and interface to choose directory
#setwd("I:/PERMITS/PERMIT_EXTRACTS")
#alt: set directory
#setwd(choose.dir())
library(readxl)
library(maptools)
library(rgdal)
library(sp)
library(raster)

#load the spreadsheet (takes forever wtf)
#data <- read_excel(file.choose())

# read in csv due to corruption with .xlsx
setwd("C:/Users/awitt/Desktop/permits updates")

#more new problems: solution here: https://stackoverflow.com/questions/14363085/invalid-multibyte-string-in-read-csv
data <- read.csv(file.choose(), fileEncoding = "latin1")

#Specify update month and Year
date <- "Aug2023"

#summary(data)
print(categories <- unique(data$GeneralPermitType))

#####################################################################################################################################################
#####################################################################################################################################################

# All Permits: subset the data from 200+ columns down, create new cleaner dataframe
new_data <- data.frame(data$PermitSector, data$GeneralPermitType, data$PermitID, data$PermitStatus, data$ApplicationReceivedDate, data$EffectiveDate,
                       data$ExpirationDate, data$TerminationDate, data$Permittee, data$FacilityName, data$FacilityAddress1, data$FacilityAddress2,
                       data$FacilityCity, data$FacilityState, data$FacilityZip, data$FacilityCounty, data$FacilityLatitude, data$FacilityLongitude,
                       data$FacilitySICCode, data$PermitSIC1, data$RegulationId, data$ImmediateWater, data$ReceivingWater, data$StreamSegment,
                       data$MajorRiverBasin, data$ActivityDescription, data$Out1Lat, data$Out1Long, data$PreviousPermitID)


#fix up the names (there has to be a better way for naming a new data frame)
names(new_data)[1] <- "PermitSector"
names(new_data)[2] <- "GeneralPermitType"
names(new_data)[3] <- "PermitID"
names(new_data)[4] <- "PermitStatus"
names(new_data)[5] <- "ApplicationReceivedDate"
names(new_data)[6] <- "EffectiveDate"
names(new_data)[7] <- "ExpirationDate"
names(new_data)[8] <- "TerminationDate"
names(new_data)[9] <- "Permittee"
names(new_data)[10] <- "FacilityName"
names(new_data)[11] <- "FacilityAddress1"
names(new_data)[12] <- "FacilityAddress2"
names(new_data)[13] <- "FacilityCity"
names(new_data)[14] <- "FacilityState"
names(new_data)[15] <- "FacilityZip"
names(new_data)[16] <- "FacilityCounty"
names(new_data)[17] <- "FacilityLatitude"
names(new_data)[18] <- "FacilityLongitude"
names(new_data)[19] <- "FacilitySICCode"
names(new_data)[20] <- "PermitSIC1"
names(new_data)[21] <- "RegulationId"
names(new_data)[22] <- "ImmediateWater"
names(new_data)[23] <- "ReceivingWater"
names(new_data)[24] <- "StreamSegment"
names(new_data)[25] <- "MajorRiverBasin"
names(new_data)[26] <- "ActivityDescription"
names(new_data)[27] <- "Out1Lat"
names(new_data)[28] <- "Out1Long"
names(new_data)[29] <- "PreviousPermitID"

#clean up lat long fields and ensure they're plotting in Colorado, or that NA's don't get lost. they plot to null island :(
new_data$FacilityLatitude <- abs(as.numeric(as.character(new_data$FacilityLatitude)))
new_data$FacilityLatitude <- ifelse(new_data$FacilityLatitude < 30, 0.0001, new_data$FacilityLatitude)

new_data$FacilityLongitude <- abs(as.numeric(as.character(new_data$FacilityLongitude)))*-1
new_data$FacilityLongitude <- ifelse(new_data$FacilityLongitude > -80, -0.0001, new_data$FacilityLongitude)


#omit missing lat long data and assign spatial X Y
new_data <- (subset(new_data, FacilityLatitude >0 & FacilityLongitude <0))
coordinates (new_data) = ~ FacilityLongitude + FacilityLatitude

#define coordinate system, using DD from GPS so we want WGS84
proj4string(new_data) <- CRS("+proj=longlat + datum=WGS84")

#save into new geodatabase
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/permits updates", paste("PermitsUpdates",".gdb", sep = ""))

#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "AllPermits"), data = new_data)


#####################################################################################################################################################
#####################################################################################################################################################
#Now we can subset off of (new_data) for all the various permit codes, and export into the above .gdb we created

#I could loop this at a later point, but using this setup now for quality control purposes and clarity

biosolids_billing <- subset(new_data, new_data$GeneralPermitType == "COK-Biosolids billing only")
arc.write(file.path(fgdb_path, "Biosolids_Billing"), data = biosolids_billing)

biosolids_user <- subset(new_data, new_data$GeneralPermitType == "COBMP-Biosolids user")
arc.write(file.path(fgdb_path, "Biosolids_User"), data = biosolids_user)

co_individual <- subset(new_data, new_data$GeneralPermitType == "CO-Individual permit")
arc.write(file.path(fgdb_path, "CO_individual"), data = co_individual)

cog070 <- subset(new_data, new_data$GeneralPermitType == "COG070000-Construction dewatering")
arc.write(file.path(fgdb_path, "COG070"), data = cog070)

cog080 <- subset(new_data, new_data$GeneralPermitType == "COG080000-Construction dewatering")
arc.write(file.path(fgdb_path, "COG080"), data = cog080)

cog130 <- subset(new_data, new_data$GeneralPermitType == "COG130000-Aquatic animal production")
arc.write(file.path(fgdb_path, "COG130"), data = cog130)

cog315 <- subset(new_data, new_data$GeneralPermitType == "COG315000-Remediation activities discharging to surface water")
arc.write(file.path(fgdb_path, "COG315"), data = cog315)

cog316 <- subset(new_data, new_data$GeneralPermitType == "COG316000-Remediation activities discharging to ground water")
arc.write(file.path(fgdb_path, "COG316"), data = cog316)

cog317 <- subset(new_data, new_data$GeneralPermitType == "COG317000-Short-Term Remediation Activities")
arc.write(file.path(fgdb_path, "COG317"), data = cog317)

cog318 <- subset(new_data, new_data$GeneralPermitType == "COG318000-Long-Term Remediation Activities")
arc.write(file.path(fgdb_path, "COG318"), data = cog318)

cog500 <- subset(new_data, new_data$GeneralPermitType == "COG500000-Sand and gravel mining process water and stormwater combined")
arc.write(file.path(fgdb_path, "COG500"), data = cog500)

cog588 <- subset(new_data, new_data$GeneralPermitType == "COG588000-Domestic (wastewater treatment plants with chronic low flow: design flow ratio) discharge 100 to 1 dilution")
arc.write(file.path(fgdb_path, "COG588"), data = cog588)

cog589 <- subset(new_data, new_data$GeneralPermitType == "COG589000-Domestic discharge (wastewater treatment facilities)")
arc.write(file.path(fgdb_path, "COG589"), data = cog589)

cog590 <- subset(new_data, new_data$GeneralPermitType == "COG590000-Domestic (wastewater treatment plants with chronic low flow: design flow ratio) discharge 100 to 1 dilution")
arc.write(file.path(fgdb_path, "COG590"), data = cog590)

cog600 <- subset(new_data, new_data$GeneralPermitType == "COG600000-Minimal discharge")
arc.write(file.path(fgdb_path, "COG600"), data = cog600)

cog603 <- subset(new_data, new_data$GeneralPermitType == "COG603000-Subterranean dewatering or well development")
arc.write(file.path(fgdb_path, "COG603"), data = cog603)

cog604 <- subset(new_data, new_data$GeneralPermitType == "COG604000-Hydrostatic testing of pipelines, tanks and similar vessels")
arc.write(file.path(fgdb_path, "COG604"), data = cog604)

cog605 <- subset(new_data, new_data$GeneralPermitType == "COG605000-Non-contact cooling water")
arc.write(file.path(fgdb_path, "COG605"), data = cog605)

cog606 <- subset(new_data, new_data$GeneralPermitType == "COG606000-Discharge from hot springs and/or geothermal wells")
arc.write(file.path(fgdb_path, "COG606"), data = cog606)

cog607 <- subset(new_data, new_data$GeneralPermitType == "COG607000-Commercial washing of outdoor structures")
arc.write(file.path(fgdb_path, "COG607"), data = cog607)

cog608 <- subset(new_data, new_data$GeneralPermitType == "COG608000-Well Development Discharges to Surface Water" | new_data$GeneralPermitType == "COG608000 Well Development")
arc.write(file.path(fgdb_path, "COG608"), data = cog608)

cog641 <- subset(new_data, new_data$GeneralPermitType == "COG641000-Water treatment plant wastewater discharge")
arc.write(file.path(fgdb_path, "COG641"), data = cog641)

cog840 <- subset(new_data, new_data$GeneralPermitType == "COG840000-Produced water treatment facilities")
arc.write(file.path(fgdb_path, "COG840"), data = cog840)

cog850 <- subset(new_data, new_data$GeneralPermitType == "COG850000-Coal mining process water and stormwater combined")
arc.write(file.path(fgdb_path, "COG850"), data = cog850)

cog860 <- subset(new_data, new_data$GeneralPermitType == "COG860000-Pesticides application")
arc.write(file.path(fgdb_path, "COG860"), data = cog860)

cono_noexposure <- subset(new_data, new_data$GeneralPermitType == "CONOX000-No exposure certification for exclusion from CDPS stormwater permitting" | new_data$GeneralPermitType == "CONOX000 -No Exposure Exclusion")
arc.write(file.path(fgdb_path, "CONO_NoExposure"), data = cono_noexposure)

cop_pretreatment <- subset(new_data, new_data$GeneralPermitType == "COP-Pretreatment")
arc.write(file.path(fgdb_path, "COP_Pretreatment"), data = cop_pretreatment)

copb_preatreatmentbilling <- subset(new_data, new_data$GeneralPermitType == "COPB-Pretreatment billing only")
arc.write(file.path(fgdb_path, "COPB_PretreatmentBilling"), data = copb_preatreatmentbilling)

cor040 <- subset(new_data, new_data$GeneralPermitType == "COR040000-Metal mining stormwater")
arc.write(file.path(fgdb_path, "COR040"), data = cor040)

cor070 <- subset(new_data, new_data$GeneralPermitType == "COR070000-Non-standard MS4 general permit")
arc.write(file.path(fgdb_path, "COR070"), data = cor070)

cor080 <- subset(new_data, new_data$GeneralPermitType == "COR080000-Cherry Creek reservoir basin MS4 general permit")
arc.write(file.path(fgdb_path, "COR080"), data = cor080)

cor090 <- subset(new_data, new_data$GeneralPermitType == "COR090000-Standard (Statewide) MS4 general permits")
arc.write(file.path(fgdb_path, "COR090"), data = cor090)

cor030 <- subset(new_data, new_data$GeneralPermitType == "COR030000-Stormwater discharge associated with construction activities")
arc.write(file.path(fgdb_path, "COR030"), data = cor030)

cor340 <- subset(new_data, new_data$GeneralPermitType == "COR340000-Sand and gravel stormwater only")
arc.write(file.path(fgdb_path, "COR340"), data = cor340)

cor400 <- subset(new_data, new_data$GeneralPermitType == "COR400000-Stormwater discharge associated with construction activities")
arc.write(file.path(fgdb_path, "COR400"), data = cor400)

cor900 <- subset(new_data, new_data$GeneralPermitType == "COR900000-Industrial stormwater")
arc.write(file.path(fgdb_path, "COR900"), data = cor900)

cos_individual <- subset(new_data, new_data$GeneralPermitType == "COS-Individual permit")
arc.write(file.path(fgdb_path, "COS_Individual"), data = cos_individual)

cox_individual <- subset(new_data, new_data$GeneralPermitType == "COX-Individual permit")
arc.write(file.path(fgdb_path, "COX_individual"), data = cox_individual)

cox620 <- subset(new_data, new_data$GeneralPermitType == "COX620000-Groundwater GPCF")
arc.write(file.path(fgdb_path, "COX620"), data = cox620)

cox621 <- subset(new_data, new_data$GeneralPermitType == "COX621000-Domestic on site systems ISDS")
arc.write(file.path(fgdb_path, "COX621"), data = cox621)

cox622 <- subset(new_data, new_data$GeneralPermitType == "COX622000-Domestic on site systems ISDS no wells")
arc.write(file.path(fgdb_path, "COX622"), data = cox622)

cox630 <- subset(new_data, new_data$GeneralPermitType == "COX630000-Domestic lagoon systems")
arc.write(file.path(fgdb_path, "COX630"), data = cox630)

cox631 <- subset(new_data, new_data$GeneralPermitType == "COX631000-Domestic discharge with land disposal of effluent")
arc.write(file.path(fgdb_path, "COX631"), data = cox631)

cox632 <- subset(new_data, new_data$GeneralPermitType == "COX632000-Domestic discharge with land treatment of effluent")
arc.write(file.path(fgdb_path, "COX632"), data = cox632)

cox633 <- subset(new_data, new_data$GeneralPermitType == "COX633000-Domestic discharge with land treatment of effluent at agronomic rates")
arc.write(file.path(fgdb_path, "COX633"), data = cox633)

cox634 <- subset(new_data, new_data$GeneralPermitType == "COX634000-Groundwater discharge with land treatment and groundwater monitoring")
arc.write(file.path(fgdb_path, "COX634"), data = cox634)

reuse_treater <- subset(new_data, new_data$GeneralPermitType == "COE-Reuse treater")
arc.write(file.path(fgdb_path, "Reuse_Treater"), data = reuse_treater)

reuse_user <- subset(new_data, new_data$GeneralPermitType == "COE-Reuse user")
arc.write(file.path(fgdb_path, "Reuse_User"), data = reuse_user)

rwaiver <- subset(new_data, new_data$GeneralPermitType == "Rwaiver000-R-factor waiver for stormwater discharges associated with construction")
arc.write(file.path(fgdb_path, "Rwaiver"), data = rwaiver)

date_stamp <- paste("lastupdated", date, sep = " ")
arc.write(file.path(fgdb_path, "lastupdated"), data = date_stamp, coords = list((x=0.0001), y=(-0.0001)), shape_info = list(type='Point'))


##### not using this anymore #####
#now split active vs terminated COG315
#fgdb_path2 <- file.path("C:/Users/awitt/Desktop/permits updates", paste("COG315to318_", date, ".gdb", sep = ""))
#
##315
#cog315active <- subset(cog315, cog315$PermitStatus == "Admin Continued" | cog315$PermitStatus == "Admin continued" | cog315$PermitStatus == "Effective")
#arc.write(file.path(fgdb_path2, "cog315active"), data = cog315active)
#
#cog315term <- subset(cog315, cog315$PermitStatus == "Terminated" | cog315$PermitStatus == "Expired")
#arc.write(file.path(fgdb_path2, "cog315term"), data = cog315term)
#
##317
#cog317active <- subset(cog317, cog317$PermitStatus == "Admin Continued" | cog317$PermitStatus == "Admin continued" | cog317$PermitStatus == "Effective")
#arc.write(file.path(fgdb_path2, "cog317active"), data = cog317active)
#
#cog317term <- subset(cog317, cog317$PermitStatus == "Terminated" | cog317$PermitStatus == "Expired")
#arc.write(file.path(fgdb_path2, "cog317term"), data = cog317term)
#
#
##318
#cog318active <- subset(cog318, cog318$PermitStatus == "Admin Continued" | cog318$PermitStatus == "Admin continued" | cog318$PermitStatus == "Effective")
#arc.write(file.path(fgdb_path2, "cog318active"), data = cog318active)
#
#cog318term <- subset(cog318, cog318$PermitStatus == "Terminated" | cog318$PermitStatus == "Expired")
#arc.write(file.path(fgdb_path2, "cog318term"), data = cog318term)
#

#####################################################################################################################################################3
# 6/4/2021 this is working really nicely, now that I've gotten it all set up. Now if the GeneralPermitType names change, we're fucked

#Final steps: 1. update the layers in the Permits Contamination Map. 2. update the COG315 AGOL layers.


#####################################################################################################################################################
#####################################################################################################################################################
#Additional layer updates will be run monthly as well: PELs, NPDES Outfalls


#PELs
#add from google spreadsheet, sharing must be enabled
library(googlesheets4)
library(maptools)
library(rgdal)
library(sp)
library(raster)

url <- "https://docs.google.com/spreadsheets/d/1pM4v_jNiy1OtZRDq-J3oJzakTNFSWCa94IqKzJIc_9M/edit?usp=sharing"
pels <- read_sheet(url, sheet = "PEL Information")

1 #select preauthorized account

#looks like I need to reformat everything out of the google spreadsheet into a new data frame and repeat the steps from the start of this document

new_pels <- data.frame(pels)

#clean up lat long fields and ensure they're plotting in Colorado, or that NA's don't get lost. they plot to null island :(
new_pels$DischargeLatitude1 <- abs(as.numeric(as.character(new_pels$DischargeLatitude1)))
new_pels$DischargeLatitude1 <- ifelse(is.na(new_pels$DischargeLatitude1), 0.0001, new_pels$DischargeLatitude1)

new_pels$DischargeLongitude1 <- -1*(abs(as.numeric(as.character(new_pels$DischargeLongitude1))))
new_pels$DischargeLongitude1 <- ifelse(is.na(new_pels$DischargeLongitude1), -0.0001, new_pels$DischargeLongitude1)


new_pels$DischargeLatitude1 <- ifelse(abs(new_pels$DischargeLatitude1) + abs(new_pels$DischargeLongitude1) < 120 | 
                                      abs(new_pels$DischargeLatitude1) + abs(new_pels$DischargeLongitude1) > 170, 0.0001, new_pels$DischargeLatitude1)
new_pels$DischargeLongitude1 <- ifelse(abs(new_pels$DischargeLatitude1) + abs(new_pels$DischargeLongitude1) < 120 |
                                       abs(new_pels$DischargeLatitude1) + abs(new_pels$DischargeLongitude1) > 170, -0.0001, new_pels$DischargeLongitude1)



#omit missing lat long data and assign spatial X Y
new_pels <- (subset(new_pels, DischargeLatitude1 >0 & DischargeLongitude1 <0))

#omit missing lat long data and assign spatial X Y
coordinates (new_pels) = ~ DischargeLongitude1 + DischargeLatitude1

#define coordinate system, using DD from GPS so we want WGS84
proj4string(new_pels) <- CRS("+proj=longlat + datum=WGS84")


#save into new geodatabase
library(arcgisbinding)
arc.check_product()
fgdb_path <- file.path("C:/Users/awitt/Desktop/permits updates", paste("PermitsUpdates", ".gdb", sep = ""))

#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "PELs"), data = new_pels)

#WARNINGS BUT SUCCESSFUL EXPORT
#####################################################################################################################################################
#####################################################################################################################################################



#next steps are to prevent NAs in Lat/Long from leaving out entries (need to replace NAs with basically 0,0)


# remember to move outputs to the bigmap folder and the X drive Unit 1 folder

# installing R- ArcGIS bridge package: install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
