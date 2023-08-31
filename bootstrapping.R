setwd("C:/Users/awitt/Downloads/USGS_NAS_NZMS/bootstrapping")

dat <- read.csv("rm_huc12_bluerib_nzms.csv", header = TRUE)

#x <- as.numeric(dat$bluerib)
#y <- as.numeric(sub$bluerib)

#this works to get the ratio I want
#set.seed(4)
#rand <- dat[sample(nrow(dat), 100), ]
#rand_prop <- sum(rand$bluerib == 1)/nrow(rand)

################################################################################
#bootstrap bluerib proportion

bluerib <- replicate(100, {
  rand <- dat[sample(nrow(dat), 100), ]
  rand_prop <- sum(rand$nmzs == 1)/sum(rand$bluerib ==1)
})

hist(bluerib)

as.data.frame(bluerib)

library(Rmisc)
CI(bluerib, 0.95)

################################################################################

#bootstrap non_bluerib proportion

non_bluerib <- replicate(100, {
  rand <- dat[sample(nrow(dat), 100), ]
  rand_prop <- sum(rand$nmzs == 1)/sum(rand$bluerib ==0)
})

hist(non_bluerib)

as.data.frame(non_bluerib)

library(Rmisc)
CI(non_bluerib, 0.95)


################################################################################
#plot the means with the upper and low CI

means <- c(0.1464189, 0.01728066)

barCenters <- barplot(means, width = 5, ylim = c(0,0.2), names.arg = c("Recommended Waters", "Other Waters"),  ylab = "Proportion of HUC12s with NZMS Occurances")

segments(barCenters, c(0.1707102, 0.02000475), barCenters, c(0.1221276, 0.01455657), lwd = 1.5)
arrows(barCenters, c(0.1707102, 0.02000475), barCenters, c(0.1221276, 0.01455657), lwd = 2.5, angle = 90, code = 3, length = 0.05)
abline(h= 0.01688, col = "red", lty = "dashed")

