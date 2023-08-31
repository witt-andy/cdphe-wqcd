# RS unsupervised classification & NDVI

# https://rspatial.org/raster/rs/4-unsupclassification.html

#data: https://landlook.usgs.gov/viewer.html
#using recent imagery, 2018?
setwd("C:/Users/awitt/Desktop/Remote Sensing")

library(raster)

####################################################################################################

#imagery of wolford reservoir and muddy creek, near kremmling CO
naip <- stack('wolford_naip.tif')
names(naip) <- c('blue', 'green', 'red', 'NIR')

#calculate NDVI for vegetation
ndvi <- (naip[['NIR']] - naip[['red']]) / (naip[['NIR']] + naip[['red']])
writeRaster(ndvi, "ndvi.tif", overwrite = TRUE)

#https://rspatial.org/raster/rs/3-basicmath.html
#NDVI histogram
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab = "Frequency",
     col = "wheat",
     xlim = c(-1, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-1, 1, 0.05), labels = seq(-1, 1, 0.05))

#threshold the NDVI by masking all cells that might not be vegetation (based on histogram)
veg <- reclassify(ndvi, cbind(-Inf, 0.25, NA))
plot(veg, main = "Vegetation")

#calculate NDWI for water
ndwi <- (naip[['green']] - naip[['NIR']]) / (naip[['green']] + naip[['NIR']])
writeRaster(ndwi, "ndwi.tif", overwrite = TRUE)

#NDWI histogram
hist(ndwi,
     main = "Distribution of NDWI values",
     xlab = "NDWI",
     ylab = "Frequency",
     col = "wheat",
     xlim = c(-1, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-1, 1, 0.05), labels = seq(-1, 1, 0.05))

#threshold the NDWI by masking all cells that might not be vegetation (based on histogram)
water <- reclassify(ndwi, cbind(-Inf, 0.3, NA))
plot(water, main = "Water")
####################################################################################################
#https://rspatial.org/raster/rs/3-basicmath.html#principal-component-analysis
# PRINCIPLE COMPONENTS ANALYSIS

#might be wise to trnasform multi-spectral data to redce dimensionality and noise of data
#use PCA to identify fewer, uncorrelated bands from a larger set of correlated bands

set.seed(99)
sr <- sampleRandom(naip, 10000)
plot(sr[,c(3,4)], main = "Vegetation and Soil-line plot(NIR-Red plot)")

pca <- prcomp(sr, scale = TRUE)
pca

screeplot(pca)

pci <-predict(naip, pca, index = 3:4)
plot(pci[[1]], main = "PCA-1")

#the first principal component pci[[1]] highlights the boundaries between land use classes or spatial details
#which is the most common information among all wavelengths

#it's difficult to understand what the second principal component is highlighting, see link for more: https://wiki.landscapetoolbox.org/doku.php/remote_sensing_methods:principal_components_analysis
####################################################################################################
#now lets do a kmeans clustering of the NDVI

#convert raster to vector/matrix

nr <- getValues(ndvi)
str(nr)

set.seed(99)
#10 clusters, 500 iterations, 5 random sets using Lloyd method
kmncluster_ndvi <- kmeans(na.omit(nr), centers = 10, iter.max = 500, nstart = 5, algorithm = "Lloyd")
str(kmncluster_ndvi)

#use ndvi object to set cluster values to new raster
knr <- raster(ndvi)
values(knr) <- kmncluster_ndvi$cluster
knr

writeRaster(knr, "ndvi_unsupervised.tif", overwrite = TRUE)
###########################################

#plot results: need to better define how to set colors later

mycolor <- rev(terrain.colors(10))

par(mfrow = c(1,2))
plot(ndvi, col = rev(terrain.colors(10)), main = 'NAIP-NDVI')
plot(knr, main = 'Unsupervised Classification: NDVI', col = mycolor )


####################################################################################################


par(mfrow = c(1,1))
plot(ndwi, col = rev(terrain.colors(10)), main = 'NAIP-NDWI')

####################################################################################################
####################################################################################################

#more exploration in RS data: https://rspatial.org/raster/rs/2-exploration.html

#crs of raster
crs(naip)

#number of cells
ncell(naip)

#spatial resolution (cell size)
res(naip)

#number of bands in raster, can also call compareRaster() to see how to different rasters differ
nlayers(naip)

#plot the bands because GIS

par(mfrow = c(2,2))
plot(naip$blue, main = "Blue", col = gray(0:100/100))
plot(naip$green, main = "Green", col = gray(0:100/100))
plot(naip$red, main = "Red", col = gray(0:100/100))
plot(naip$NIR, main = "NIR", col = gray(0:100/100))

par(mfrow=c(1,1)) #can also reset graphing with dev.off()
plotRGB(naip, axes = TRUE, stretch = 'lin', main = "NAIP True Color Composite")


#relation between bands: blue and green
pairs(naip[[1:2]], main = "Blue versus Green")
#relation between bands: red and NIR
pairs(naip[[3:4]], main = "Red versus NIR")

#notes about above code:
#1. high correlations will let you remove one wavelength without losing much information
#2. If the bototm left graph shows a large triangle that suggests one value on the X axis still leads to multiple signatures on the  y axis

#for this image, blue vs green are very highly correlated (r2 = 0.98)
#for this image, red vs NIR are not correlated (r2 = 0.12)

##########################################

#Getting spectral bands from NLCD

#to do that we first need to extract NLCD control points

samp <- shapefile("nlcd_poly.shp")
#rename the nlcd classes
names(samp)[3] <- "class"
#generate 300 point samples from the polygons
ptsamp <- spsample(samp, 10000, type = 'regular')
#add land cover class to the points
ptsamp$class <- over(ptsamp, samp)$class

#extract values with ponts
df <- extract(naip, ptsamp)
head(df)

#note, cannot remove NA's (if shapefile has different extent than raster), it throws off the lengths in aggregate below

#compute mean reflectance values for each class and each band
ms <- aggregate(df, list(ptsamp$class), mean, na.rm = TRUE)

#instead of first column, we use row names
rownames(ms) <- ms [,1]
ms <- ms[, -1]
ms

#now lets plot the mean spectra of these features

#colors for plotting
mycolors <- c('#B2ADA3', '#AA7028', '#68AA63', '#AA0000', '#D89382', '#ED0000', '#DDC9C9', '#70A3BA', '#1C6330', '#DBD83C', '#E2E2C1', '#B5C98E', '#476BA0', '#CCBA7C', '#BAD8EA')

#transform ms from data.frame to matrix
ms <- as.matrix(ms)

#start with empty plot
plot(0, ylim = c(0, 250), xlim = c(1, 7), type = 'n', xlab = "Bands", ylab = "Reflectance")

#add the different classes
for (i in 1:nrow(ms)){
  lines(ms[i, ], type = "l", lwd = 3, lty = 1, col = mycolors[i])
}

#Title and legend
title(main = "Spectral Profile from NAIP", font.main = 2)
par(xpd=TRUE) #allows things outside the plot region
legend(4.5, 250, rownames(ms), cex=0.8, col=mycolors, lty=1, lwd=3, bty="n")

#interpreting graph

#1. water has low reflectance in all wavelengths
#2. Hay/Pasture, Emergent Herbaceuous Wetlands, Mixed Forest, Herbaceuous, Deciduous Forest (among others), have relatively high reflectance in longer wavelengths


