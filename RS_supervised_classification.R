# image classification with R, RF
# setwd("C:/Users/awitt/Desktop/Remote Sensing")
# Code Guide: http://amsantac.co/blog/en/2015/11/28/classification-r.html
# Code Guide: https://rspatial.org/raster/rs/5-supclassification.html

# LandSat Data: https://www.coloradoview.org/data-landsat/

library(rgdal)
library(raster)
library(caret)

###################################################################################################################
#Load NLCD, classify using ratify to classify raster data where RAT = raster attribute table

nlcd <- raster("C:/Users/awitt/Desktop/Remote Sensing/nlcd_co_2011.tif")

nlcd <- ratify(nlcd)
rat <- levels(nlcd)[[1]]

rat$landcover <- c("Open Water", "Perennial Snow/Ice", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity",
               "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Herbaceuous", "Hay/Pasture", "Cultivated Crops", 
               "Woody Wetlands", "Emergent Herbaceuous Wetlands")
rat$code <- c(11,12,21,22,23,24,31,41,42,43,52,71,81,82,90,95)
levels(nlcd) <- rat

classcolor <- c("#476BA0", "#D1DDF9", "#DDC9C9", "#D89382", "#ED0000", "#AA0000", "#B2ADA3", "#68AA63", "#1C6330", "#B5C98E",
                "#CCBA7E", "#E2E2C1", "#DBD83C", "#AA7028", "#BAD8EA", "#70A3BA")


###################################################################################################################
# construct training site locations based on NLCD

set.seed(99)
#sample points based on NLCD, inherits CRS of raster
#size = # is how many are sampled in each category
samp2011 <- sampleStratified(nlcd, size = 500, na.rm = TRUE, sp = TRUE)

library(rasterVis)
nlcd <- stack(nlcd)
plt <- levelplot(nlcd, col.regions = classcolor, main = "Distribution of Training Data Sites")
print(plt + layer(sp.points(samp2011, pch = 3, cex = 0.5, col = 1)))


###################################################################################################################
#load Landsat image into R (Central Colorado, 2000)


img <- stack("C:/Users/awitt/Desktop/Remote Sensing/N-13-35-ul-2000.tif")
#now lets name the bands
names(img) <- c("red", "green", "blue")

#need to reproject into NAD83 UTM Zone13N
#newproj <- "+init=epsg:26913 +proj=utm +zone=13 +datum=NAD83 +units=m"
#this takes a long time (apparently), so it might be best to reproject in ArcGIS
#img_re <- projectRaster(img, crs = newproj)

###################################################################################################################
#extract values from remote sensed imagery to sample points

sampvals <- extract(img, samp2011, df = TRUE)
#drop ID column
sampvals <- sampvals[,-1]
#combine class information with extracted values
sampdata <- data.frame(classvalue = samp2011@data$nlcd_co_2011, sampvals)
sampdata <- na.omit(sampdata)
###################################################################################################################
#train the classifier (create classification tree)

#Recursive Partitioning and Regression Trees

library(rpart)
#train the model
cart <- rpart(as.factor(classvalue)~., data = sampdata, method = 'class', minsplit = 10)
plot(cart, uniform = TRUE, main = "Classification Tree", asp = 5)
text(cart, cex = 0.8)

#now we have the trained classification model: cart
#can use cart to make predictions across all cells in the landsat data: img_re

###################################################################################################################

#Predict classification based on trained data

pr2011 <- predict(img, cart, type ="class")

#now plot with ratify
pr2011 <- ratify(pr2011)

# **don't know a better way here, but to call pr2011@data, view the attributes slot, and edit down the NLCD list below based on these results**
pr2011@data@attributes


rat2 <- levels(pr2011)[[1]]
rat2$legend <- c("Open Water", "Developed, Low Intensity", "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", 
                 "Shrub/Scrub", "Pasture Hay", "Woody Wetlands", "Emergent Herbaceuous Wetlands")


levels(pr2011) <- rat2

writeRaster(pr2011, "classified_img.tif", overwrite = TRUE)

###################################################################################################################

#plot the results

#need new colors to match the above legend categories, same issues as above
newclasscolor <- c("#476BA0", "#D89382", "#B2ADA3", "#68AA63", "#1C6330", "#B5C98E",
                "#CCBA7E", "#DBD83C", "#BAD8EA", "#70A3BA")

rasterVis::levelplot(pr2011, col.regions = newclasscolor, main = "Decision Tree Classification of Landsat: Central CO")


###################################################################################################################

#Model Evaluation

#In RS, two measures are widely used: overall accuracy, and kappa

#Evaluate with K-folds cross validation, splits into k groups (usually 5) where 1 is testing and 4 are training

library(dismo)
set.seed(99)

#use the sampdata where we extracted both raster layer values to points
j <- kfold(sampdata, k = 5, by= sampdata$classvalue)
table(j)

#now we train and test the model five times, each time computing a confusion matrix stored in a list

#Recursive Partitioning and Regression Trees
x<-list()
for (k in 1:5) {
  train <- sampdata [j!=k,]
  test <- sampdata[j==k,]
  cart <-rpart(as.factor(classvalue)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type = 'class')
  #create data frame using reference and prediction
  x[[k]] <- cbind(test$classvalue, as.integer(pclass))
}

#combine the five lists into a single data frame, compute confusion matrix
y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)

colnames(conmat) <- rat2$legend
rownames(conmat) <- rat$landcover
conmat #review the misclassifications

#need to run this piece to remove the NLCD that do not apply
#if you call conmat before changing colnames and rownames you can use colnumbs to define the code below
conmat2 <- conmat[c(1,3,4,8,9,10,11,12,13,15), ]


#####################################################

#Compute Overall Accuracy

#number of cases
n <- print(sum(conmat2))
#number of correctly classified cases per class
diag <- diag(conmat2)
#overall accuracy
OA <- print(sum(diag) / n)

#####################################################

#Compute Kappa Statistic

# observed (true) cases per class
rowsums <- apply(conmat2, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat2, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- print((OA - expAccuracy) / (1 - expAccuracy))

#####################################################

#Producer and User Accuracy

PA <- diag / colsums
UA <- diag / rowsums

OutAcc <- print(data.frame(producerAccuracy = PA, userAccuracy = UA))

###################################################################################################################
###################################################################################################################

#now try the RandomForest method

library(randomForest)
#train the model
rf <- randomForest(as.factor(classvalue)~., data = sampdata, mtry=2, importance=TRUE, na.action=na.omit)

rf
#can use cart to make predictions across all cells in the landsat data: img_re

###################################################################################################################

#Predict classification based on trained data

pr2011_rf <- predict(img, rf, type ="class")

#now plot with ratify
pr2011_rf <- ratify(pr2011_rf)

# **don't know a better way here, but to call pr2011@data, view the attributes slot, and edit down the NLCD list below based on these results**
pr2011_rf@data@attributes


rat2_rf <- levels(pr2011_rf)[[1]]
rat2_rf$legend <- c("Open Water", "Perennial Snow/Ice", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity",
                    "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Herbaceuous", "Hay/Pasture", "Cultivated Crops", 
                    "Woody Wetlands", "Emergent Herbaceuous Wetlands")


levels(pr2011_rf) <- rat2_rf

writeRaster(pr2011_rf, "classified_img_rf.tif", overwrite = TRUE)

###################################################################################################################

#plot the results

#need new colors to match the above legend categories, same issues as above
newclasscolor <- c("#476BA0", "#D1DDF9", "#DDC9C9", "#D89382", "#ED0000", "#AA0000", "#B2ADA3", "#68AA63", "#1C6330", "#B5C98E",
                                 "#CCBA7E", "#E2E2C1", "#DBD83C", "#AA7028", "#BAD8EA", "#70A3BA")

rasterVis::levelplot(pr2011_rf, col.regions = newclasscolor, main = "RF Classification of Landsat: Central CO")


################################################

# now the statistics

conmat_rf <- rf$confusion

#Compute Overall Accuracy

#number of cases
n <- print(sum(conmat_rf))
#number of correctly classified cases per class
diag <- diag(conmat_rf)
#overall accuracy
OA <- print(sum(diag) / n)

#####################################################

#Compute Kappa Statistic

# observed (true) cases per class
rowsums <- apply(conmat_rf, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat_rf, 2, sum)
q <- colsums / n
q <- q[1:16]
expAccuracy <- sum(p*q)
kappa <- print((OA - expAccuracy) / (1 - expAccuracy))

#####################################################

#Producer and User Accuracy

PA <- diag / colsums[1:16]
UA <- diag / rowsums

OutAcc <- print(data.frame(producerAccuracy = PA, userAccuracy = UA))



###################################################################################################################
###################################################################################################################

#easier (?) method with the RStoolbox, to try the RandomForest 
library(raster)

#load supervisor classes
nlcd <- raster("C:/Users/awitt/Desktop/Remote Sensing/nlcd_co_2011.tif")

#create attribute table for raster layer
nlcd <- ratify(nlcd)
rat <- levels(nlcd)[[1]]

rat$landcover <- c("Open Water", "Perennial Snow/Ice", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity",
                   "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Herbaceuous", "Hay/Pasture", "Cultivated Crops", 
                   "Woody Wetlands", "Emergent Herbaceuous Wetlands")
rat$code <- c(11,12,21,22,23,24,31,41,42,43,52,71,81,82,90,95)
levels(nlcd) <- rat

classcolor <- c("#476BA0", "#D1DDF9", "#DDC9C9", "#D89382", "#ED0000", "#AA0000", "#B2ADA3", "#68AA63", "#1C6330", "#B5C98E",
                "#CCBA7E", "#E2E2C1", "#DBD83C", "#AA7028", "#BAD8EA", "#70A3BA")

#sample points for each class
set.seed(99)
samp2011_rf <-sampleStratified(nlcd, size = 100, na.rm = TRUE, sp = TRUE)
samp2011_rf@data$class <- as.character(samp2011_rf@data$nlcd_co_2011)

#####################################################

#load the image we want to apply the classes to
img <- brick("C:/Users/awitt/Desktop/Remote Sensing/N-13-35-ul-2000_utm.tif")
#now lets name the bands
names(img) <- c("red", "green", "blue")

#####################################################

#run a supervised classification
library(RStoolbox)

rf <- superClass(img, trainData = samp2011_rf, responseCol = "nlcd_co_2011", minDist = 1, model = "rf", kfold = 10, mode = "classification", predict = TRUE)
rf
getValidation(rf)

#####################################################

newclasscolor <- c("#476BA0", "#D1DDF9", "#DDC9C9", "#D89382", "#ED0000", "#AA0000", "#B2ADA3", "#68AA63", "#1C6330", "#B5C98E",
                   "#CCBA7E", "#E2E2C1", "#DBD83C", "#AA7028", "#BAD8EA", "#70A3BA")

rasterVis::levelplot(rf$map, col.regions = newclasscolor, main = "RF Classification of Landsat: Central CO")


plot(rf$map)
