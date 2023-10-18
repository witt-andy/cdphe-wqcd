###### new jittering procedure working with the sf package ######
library(sf)


# load Decimal Degree shapefile
points <- read_sf("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Bauer/unjittered_points.shp")

# plot points for future comparison
plot(points$geometry)

# setting jitter threshold
# testing reveals, approximately 0.5 mi buffering
points_jitter <- st_jitter(points, 0.005)

# plot jittered points to visually check against original points
plot(st_jitter(points, 0.005), add = TRUE, col = 'red')


###### interactive mapping to check the thresholds ######

library(leaflet)

#use leaflet to create interactive web map
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addCircles(data = points, color = "blue", popup = paste("Name: ", points$Case_Name), group = "Unjittered") %>%
  addCircles(data = points_jitter, color = "orange", popup = paste("Name: ", points_jitter$Case_Name), group = "Jittered") %>%
  addLayersControl(overlayGroups = c("Unjittered", "Jittered"), options = layersControlOptions(collapsed=FALSE)) %>%
  addLegend("bottomright", colors =c("blue", "orange"), labels= c("Unjittered", "Jittered"))




m






###### save into new geodatabase ###### 
library(arcgisbinding)
arc.check_product()

# define path
fgdb_path <- file.path("C:/Users/awitt/Desktop/Deliverables/CDPHE_WQCD/Margaret Bauer", paste("jitter",".gdb", sep = ""))


#create individual feature class, and puts them into the .gdb we just created
arc.write(file.path(fgdb_path, "jittered_points"), data = points_jitter)



