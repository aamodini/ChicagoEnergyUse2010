library(ggplot2)
library(randomForest)
library(tree)

######################
# Load the data set #
####################
dat.old <- read.csv("~/Documents/Hogwarts/Graduate UIUC/First Year/STAT 432/Project Assignment/energy-usage-2010.csv")

dat <- dat.old[complete.cases(dat.old),] #omit the NA values in the dataset
#The percentage of data that is removed because of the NA values
(1 - nrow(dat)/nrow(dat.old))*100 #31.57% of the data was removed - makes sense because of the NA values present in the categorical variables
#randomly assigning values to the categorical variables could introduce a lot of bias 
#when categorising between the months

#############################
# Final dataset processing #
###########################

#each obervation is the information of 1 person. 
#Need to split up the data that excludes all the numerical factors of the variables 
#theres a lot of dependency of variables

#These 15 variables are the ones that are specifically
#Look at kWh specifically first 
#Therms can be converted into kWh - so we should expect similar results from Therms and kWh
dat$BUILDING.TYPE <- droplevels(dat$BUILDING.TYPE)
dat$BUILDING.TYPE <- as.factor(dat$BUILDING.TYPE)

dat$BUILDING_SUBTYPE <- droplevels(dat$BUILDING_SUBTYPE)
dat$BUILDING_SUBTYPE <- as.factor(dat$BUILDING_SUBTYPE)

kwh.dat <- dat[, which(colnames(dat) %in% c("COMMUNITY.AREA.NAME",
                                            "CENSUS.BOCK",
                                            "BUILDING.TYPE",
                                            "BUILDING_SUBTYPE",
                                            "ELECTRICITY.ACCOUNTS",
                                            "ZERO.KWH.ACCOUNTS",
                                            "KWH.SQFT.MEAN.2010",
                                            "TOTAL.POPULATION",
                                            "TOTAL.UNITS",
                                            "AVERAGE.STORIES",
                                            "AVERAGE.BUILDING.AGE",
                                            "AVERAGE.HOUSESIZE",
                                            "OCCUPIED.UNITS",
                                            "RENTER.OCCUPIED.HOUSING.UNITS",
                                            "OCCUPIED.HOUSING.UNITS",
                                            "KWH.MEAN.2010"))]

# therm.dat <- dat[, which(colnames(dat) %in% c("COMMUNITY.AREA.NAME",
#                                             "CENSUS.BOCK",
#                                             "BUILDING.TYPE",
#                                             "BUILDING_SUBTYPE",
#                                             "ELECTRICITY.ACCOUNTS",
#                                             "ZERO.THERMS.ACCOUNTS",
#                                             "THERMS.TOTAL.SQFT",
#                                             "THERMS.SQFT.MEAN.2010",
#                                             "TOTAL.POPULATION",
#                                             "TOTAL.UNITS",
#                                             "AVERAGE.STORIES",
#                                             "AVERAGE.BUILDING.AGE",
#                                             "AVERAGE.HOUSESIZE",
#                                             "OCCUPIED.UNITS",
#                                             "RENTER.OCCUPIED.HOUSING.UNITS",
#                                             "OCCUPIED.HOUSING.UNITS",
#                                             "TOTAL.THERMS",
#                                             "THERMS.MEAN.2010",
#                                             "THERMS.TOTAL.SQFT",
#                                             "THERM.MEAN.2010",
#                                             "THERM.SQFT.MEAN.2010",
#                                             "GAS.ACCOUNTS"))]

#These additional variiables for Therms instead of kWh.


#Categorize the dataset - label the months

lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
labs <- sapply(1:12, function(x) rep(lab[x], nrow(kwh.dat)))

months <- c('JANUARY',
            'FEBRUARY',
            'MARCH',
            'APRIL',
            'MAY',
            'JUNE',
            'JULY',
            'AUGUST',
            'SEPTEMBER',
            'OCTOBER',
            'NOVEMBER',
            'DECEMBER')

colnames <- sapply(1:length(months), function(x) paste0('KWH', '.', months[x], '.2010'))
months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset

final.ds <- data.frame(kwh.dat, "KWH.USE" = months.dat[,1], "months" = labs[,1])

for (mon in 2:ncol(months.dat)){
  ds <- data.frame(kwh.dat, "KWH.USE" = months.dat[,mon], "months" = labs[,mon])
  final.ds <- rbind(final.ds, ds)
}

final.ds$KWH.USE <- log(1+final.ds$KWH.USE)

# KWH USE is a column with the usage per month - months are labeled
# Use this dataset only when classifying/sorting data by month

###############################
# Data Visualization for kWh #
#############################

# The trend in kWh consumption per month

ggplot(data=final.ds, aes(x=months, y=KWH.USE, group=1)) +
  xlab("Months") + ylab("Average kWh Consumption") + ggtitle("Average kWh Consumption per Month") +
  geom_line(stat='summary', fun.y='mean') +
  geom_point(stat='summary', fun.y='mean')

# The trend in building subtype

ggplot(data=dat, aes(x=BUILDING_SUBTYPE, y=TOTAL.KWH, group=1)) +
  xlab("Building Sub-Type") + ylab("kWh Consumption") + ggtitle("Total kWh Consumption by Building Subtype") +
  geom_line(stat='summary', fun.y='mean') +
  geom_point(stat='summary', fun.y='mean')

# The building subtype grouped by month

ggplot(data=final.ds, aes(x=months, y=KWH.USE, group=BUILDING_SUBTYPE, color = BUILDING_SUBTYPE)) +
  xlab("Month") + ylab("kWh Consumption") + ggtitle("kWh Consumption by Building Subtype per Month") +
  geom_line(stat='summary', fun.y='mean') +
  geom_point(stat='summary', fun.y='mean')

# Average house size with mean

ggplot(data=dat, aes(x=AVERAGE.HOUSESIZE, y=KWH.MEAN.2010, group=1)) +
  xlab("Average House Size") + ylab("kWh Mean") +
  geom_line(stat='summary', fun.y='mean') +
  geom_point(stat='summary', fun.y='mean')

# Average stories with total kWh

ggplot(data=dat, aes(x=AVERAGE.STORIES, y=log(1+KWH.MEAN.2010), group=1)) +
  geom_bar(colour="black", stat="identity", size=.5) + 
  xlab("Average Stories") + ylab("Total kWh")

hist()



##########
# Goals #
########

# 1. Predict energy expenditure using factors.
#   there are about 50 different factors we can possibly choose from
# Y_1: KWH Total 2010 (dat$TOTAL.KWH)
# Y_2: Therm Total 2010 (dat$TOTAL.THERMS) 
# Using averages become quite tricky
# Factors include
# X_1: Building Type (dat$BUILDING.TYPE) - can take into account
# X_1i: Building subtype
# X_2: dat$OCCUPIED.UNITS
# X_3: dat$TOTAL.UNITS
# X_4: dat$AVERAGE.STORIES
# X_5: dat$AVERAGE.BUILDING.AGE
# X_6: dat$AVERAGE.HOUSESIZE
# X_7: dat$COMMUNITY.AREA.NAME

# 2. Classify the building types:
# X_1: KWH Total 2010 (dat$TOTAL.KWH)
# X_2: Therm Total 2010 (dat$TOTAL.THERMS) 
# X_2i: dat$OCCUPIED.UNITS
# X_3: dat$TOTAL.UNITS
# X_4: dat$AVERAGE.STORIES
# X_5: dat$AVERAGE.BUILDING.AGE
# X_6: dat$AVERAGE.HOUSESIZE
# X_7: dat$COMMUNITY.AREA.NAME

# What types of buildings and specificities in those buildings 
# cause consumption to be lower and we can further understand 
# what factors lead to more efficient energy usage.


##################
# Stat Learning #
################

############
# K means #
##########

# According to Nat Geo, a season lasts 4 months and so we define
# Dec - Feb = Winter (1), March - May = Spring(2), June - August = Summer(3), Sept - Nov = Fall(4)
# Create another label column for the seasons.

##########################################################
final.ds <- data.frame(final.ds, 'season' = 1)

for (labels in lab) {
  if (labels == 'Mar'){
    final.ds[which(final.ds$months == "Mar"), ]$season = 2
  }
  if (labels == 'Apr'){
    final.ds[which(final.ds$months == "Apr"), ]$season = 2
  }
  if (labels == 'May'){
    final.ds[which(final.ds$months == "May"), ]$season = 2
  }
  if (labels == 'Jun'){
    final.ds[which(final.ds$months == "Jun"), ]$season = 3
  }
  if (labels == 'Jul'){
    final.ds[which(final.ds$months == "Jul"), ]$season = 3
  }
  if (labels == 'Aug'){
    final.ds[which(final.ds$months == "Aug"), ]$season = 3
  }
  if (labels == 'Sep'){
    final.ds[which(final.ds$months == "Sep"), ]$season = 4
  }
  if (labels == 'Oct'){
    final.ds[which(final.ds$months == "Oct"), ]$season = 4
  }
  if (labels == 'Nov'){
    final.ds[which(final.ds$months == "Nov"), ]$season = 4
  }
}

final.ds$season <- as.factor(final.ds$season)
levels(final.ds$months) <- c('1', '2', '3', '4','5', '6', '7', '8', '9', '10', '11', '12')

##########################################################

# try PCA on kwh.dat
nums <- unlist(lapply(kwh.dat, is.numeric))
dat.pca <- kwh.dat[ , nums]
dat.pca <- as.matrix(dat.pca)
numeric.pca <- princomp(dat.pca)
pr.numeric.pca <- prcomp(dat.pca)
plot(numeric.pca, type = "l", pch = 19, main = "Engergy Use PCA Variance")
#shows high correlation.

#head(dat.pca %*% numeric.pca$loadings)

# ggplot(data = data.frame(pr.numeric.pca$x), aes(x=PC1, y=PC2)) + 
#   geom_point(color=c("chartreuse4", "darkorange", "deepskyblue", "purple")[kwh.dat$BUILDING.TYPE], size = 1)

pairs(pr.numeric.pca$x[,1:5], col=c("chartreuse4", "darkorange", "deepskyblue", "purple")[kwh.dat$BUILDING.TYPE], cex = .3,pch = 19)

hist(numeric.pca$scores[,1])

#PCA on the repeated dataset
nums <- unlist(lapply(kwh.fin, is.numeric))

dat.pca <- kwh.fin[ , nums]
dat.pca <- as.matrix(dat.pca)
numeric.pca <- princomp(dat.pca)
plot(numeric.pca, type = "l", pch = 19, main = "Engergy Use PCA Variance")
hist(log(1+numeric.pca$scores[,1]))
for (col in 1:ncol(dat.pca)){
  dat.pca[,col] <- (dat.pca[,col] - mean(dat.pca[,col]))/sd(dat.pca[,col])
}
numeric.pca.norm <- princomp(dat.pca)
plot(numeric.pca.norm, type = "l", pch = 19, main = "Engergy Use PCA Variance")
hist(log(1+numeric.pca.norm$scores[,1]))
plot.pca <- prcomp(dat.pca)
ggplot(data = data.frame(plot.pca$x), aes(x=PC1, y=PC2)) +
  geom_point(color=c("chartreuse4", "darkorange")[kwh.fin$BUILDING.TYPE], size = 1)

#### Recursive Partitioning

rf.fit = randomForest(cbind(final.ds[,16], final.ds[,5]), final.ds$season, ntree = 1000, mtry = 1, nodesize = 20, sampsize = 500)

rf.bt.fit <- randomForest(cbind(kwh.dat$ZERO.KWH.ACCOUNT, kwh.dat$KWH.SQFT.MEAN.2010, kwh.dat$TOTAL.POPULATION, kwh.dat$TOTAL.UNITS,
                                  kwh.dat$AVERAGE.STORIES, kwh.dat$AVERAGE.BUILDING.AGE, kwh.dat$AVERAGE.HOUSESIZE,
                                  kwh.dat$OCCUPIED.UNITS, kwh.dat$RENTER.OCCUPIED.HOUSING.UNITS,
                                  kwh.dat$OCCUPIED.HOUSING.UNITS, kwh.dat$KWH.MEAN.2010), kwh.dat$BUILDING_SUBTYPE, ntree = 1000, mtry = 1, nodesize = 20, sampsize = 500)

# uneven classes! 
# when looking at classification error 
tree.fit = tree(kwh.dat$BUILDING.TYPE ~ kwh.dat$ZERO.KWH.ACCOUNTS +
                  kwh.dat$KWH.SQFT.MEAN.2010 + kwh.dat$TOTAL.POPULATION + kwh.dat$TOTAL.UNITS +
                  kwh.dat$AVERAGE.STORIES + kwh.dat$AVERAGE.BUILDING.AGE + kwh.dat$AVERAGE.HOUSESIZE +
                  kwh.dat$OCCUPIED.UNITS + kwh.dat$RENTER.OCCUPIED.HOUSING.UNITS +
                  kwh.dat$OCCUPIED.HOUSING.UNITS + kwh.dat$KWH.MEAN.2010)
plot(tree.fit)
text(tree.fit)

 



