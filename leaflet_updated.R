#use as template for future leaflet maps

setwd("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Talbott")

data <- read.csv(file.choose())


library(sf)
library(leaflet)
library(raster)




ccp <- shapefile("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Talbott/CCP.shp")
cch <- shapefile("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Talbott/CCH.shp")
sch <- shapefile("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Talbott/SCH.shp")
counties <- shapefile("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Talbott/Counties_wgs84.shp")
counties$county <- counties$COUNTY



#use leaflet to create interactive web map
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addPolygons(data = counties, fillOpacity = 0, color = "black", weight = 0.5, popup = paste("County: ", counties$county)) %>%
  addCircles(data = ccp, color = "blue", popup = paste("Name: ", ccp$Contact_Na, "|", "Phone: ", ccp$Contact_ph, "|", "Email: ", ccp$Contact_em), group = "Child Care Providers") %>%
  addCircles(data = cch, color = "orange", popup = paste("Name: ", ccp$Contact_Na, "|", "Phone: ", ccp$Contact_ph, "|", "Email: ", ccp$Contact_em), group = "Child Care Homes") %>%
  addCircles(data = sch, color = "green", popup = paste("Name: ", ccp$Contact_Na, "|", "Phone: ", ccp$Contact_ph, "|", "Email: ", ccp$Contact_em), group = "Schools") %>%
  addLayersControl(overlayGroups = c("Child Care Providers", "Child Care Homes", "Schools"), options = layersControlOptions(collapsed=FALSE)) %>%
  addLegend("bottomright", colors =c("blue", "orange", "green"), labels= c("Child Care Providers", "Child Care Homes", "Schools"))




m


# save it!
htmlwidgets::saveWidget(m, "LeadTesting.html", selfcontained = T)

