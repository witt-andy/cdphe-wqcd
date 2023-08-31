

install.packages("dataRetrieval")

library(dataRetrieval)

##### Calling USGS data into R with the dataRetrieval package #####
# https://waterdata.usgs.gov/blog/dataretrieval/ 

pcode <- readNWISpCode("all")
head(pcode)

siteNo <- "06701900" #site No found from usgs webpage for this gauging station
pCode <- "00060" #pcode for flow
start.date <- "2016-01-01" #note the formatting YYYY-MM-DD
end.date <- "2022-07-31" #note the formatting YYYY-MM-DD

#get data from USGS
deckers <- readNWISuv(siteNumbers = siteNo, parameterCd = pCode, startDate = start.date, endDate = end.date)

#clean up names
deckers <- renameNWISColumns(deckers)
names(deckers)

#access attributes
url <- attr(deckers, "url")

#plotting hydrograph
library(ggplot2)

ts <- ggplot(data = deckers, aes(dateTime, Flow_Inst)) +
      geom_line()

ts

#can now use attributes from data frame to create better labels in the graph
parameterInfo <- attr(deckers, "variableInfo")
siteInfo <- attr(deckers, "siteInfo")

ts <- ts +
      xlab("") +
      ylab(parameterInfo$variableDescription) +
      ggtitle(siteInfo$station_nm)

ts

##### Colorado Sites Mapping #####

#how many sites in CO monitor phosphorus (00665)
co_sites <-whatNWISsites(stateCd = "CO", parameterCd = "00665")
nrow(co_sites)

# lets map these sites
library(ggplot2)
library(ggsn)
library(sf)
library(dplyr)
library(maps)

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                crs = 4269)

sf_co <- st_as_sf(co_sites, 
                  coords = c("dec_long_va", "dec_lat_va"),
                  crs = 4269)
ggplot() +
  geom_sf(data = usa[ usa$ID == "colorado" ,]) +
  geom_sf(data = sf_co) + 
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  north(sf_co, symbol=10, location="bottomleft") +
  scalebar(usa[ usa$ID == "colorado" ,],
           dist=100, dist_unit="mi", st.size = 3,
           transform=TRUE, model="WGS84")


