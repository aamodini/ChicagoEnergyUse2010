# Libraries ---------------------------------------------------------

library(ggplot2)
library(randomForest)
library(tree)
library(glmnet)
library(MASS)
library(leaps)
library(captioner)
library(knitr)
library(xtable)
library(splines)
library(Metrics)


# Data Exploration --------------------------------------------------------

# Read Data ---------------------------------------------------------------

dat.old <- read.csv("~/Documents/Hogwarts/Graduate UIUC/First Year/STAT 432/energy-usage-2010.csv")
dat <- dat.old[complete.cases(dat.old),]

# Redefine levels ---------------------------------------------------------

dat$BUILDING.TYPE <- droplevels(dat$BUILDING.TYPE)
dat$BUILDING.TYPE <- as.factor(dat$BUILDING.TYPE)

dat$BUILDING_SUBTYPE <- droplevels(dat$BUILDING_SUBTYPE)
dat$BUILDING_SUBTYPE <- as.factor(dat$BUILDING_SUBTYPE)

dat$ELECTRICITY.ACCOUNTS <- droplevels(dat$ELECTRICITY.ACCOUNTS)
dat$ELECTRICITY.ACCOUNTS <- as.factor(dat$ELECTRICITY.ACCOUNTS)

dat$GAS.ACCOUNTS <- droplevels(dat$GAS.ACCOUNTS)
dat$GAS.ACCOUNTS <- as.factor(dat$GAS.ACCOUNTS)

# For the whole dataset

# BUILDING.TYPE ---------------------------------------------------
dat <- dat[dat$BUILDING.TYPE != "Industrial",] # remove 'Industrial'
dat$BUILDING.TYPE <- factor(dat$BUILDING.TYPE)

dat$BUILDING.TYPE <- droplevels(dat$BUILDING.TYPE) # redefine the levels 
dat$BUILDING.TYPE <- as.factor(dat$BUILDING.TYPE)

# BUILDING SUBTYPE without Industry 
dat$BUILDING_SUBTYPE <- droplevels(dat$BUILDING_SUBTYPE)
dat$BUILDING_SUBTYPE <- as.factor(dat$BUILDING_SUBTYPE)

# BUILDING_SUBTYPE ---------------------------------------------------
dat <- dat[dat$BUILDING_SUBTYPE != "Municipal",] # remove 'Municipal'
dat$BUILDING_SUBTYPE <- factor(dat$BUILDING_SUBTYPE)

dat$BUILDING_SUBTYPE <- droplevels(dat$BUILDING_SUBTYPE) # redefine the levels
dat$BUILDING_SUBTYPE <- as.factor(dat$BUILDING_SUBTYPE)

# Recategorize the ELECTRICITY.ACCOUNTS ---------------------------------------------------

values <- levels(dat$ELECTRICITY.ACCOUNTS)
values <- values[-length(values)]
values <- as.numeric(values)
values <- sort(values) 

cat.1 <- as.character(values[1:3])
cat.2 <- as.character(values[4:6])
cat.3 <- as.character(values[7:10])
cat.4 <- as.character(values[11:18])
cat.5 <- as.character(values[19:34])
cat.6 <- as.character(values[35:272])

for (i in 1:20){
  for (lev in 1:length(levels(dat$ELECTRICITY.ACCOUNTS))){
    if (levels(dat$ELECTRICITY.ACCOUNTS)[lev] %in% cat.1 == T){
      levels(dat$ELECTRICITY.ACCOUNTS)[lev] = "4 to 6"
    } else if (levels(dat$ELECTRICITY.ACCOUNTS)[lev] %in% cat.2 == T){
      levels(dat$ELECTRICITY.ACCOUNTS)[lev] = "7 to 9"
    } else if (levels(dat$ELECTRICITY.ACCOUNTS)[lev] %in% cat.3 == T){
      levels(dat$ELECTRICITY.ACCOUNTS)[lev] = "10 to 13"
    } else if (levels(dat$ELECTRICITY.ACCOUNTS)[lev] %in% cat.4 == T){
      levels(dat$ELECTRICITY.ACCOUNTS)[lev] = "14 to 21"
    } else if (levels(dat$ELECTRICITY.ACCOUNTS)[lev] %in% cat.5 == T){
      levels(dat$ELECTRICITY.ACCOUNTS)[lev] = "22 to 37"
    } else if (levels(dat$ELECTRICITY.ACCOUNTS)[lev] %in% cat.6 == T){
      levels(dat$ELECTRICITY.ACCOUNTS)[lev] = "38 and Greater"
    } 
  } 
}

# Recategorize the GAS.ACCOUNTS ---------------------------------------------------

values <- levels(dat$GAS.ACCOUNTS)
values <- values[-length(values)]
values <- as.numeric(values)
values <- sort(values) 

cat.1 <- as.character(values[1:3])
cat.2 <- as.character(values[4:6])
cat.3 <- as.character(values[7:10])
cat.4 <- as.character(values[11:18])
cat.5 <- as.character(values[19:34])
cat.6 <- as.character(values[35:149])

for (i in 1:20){
  for (lev in 1:length(levels(dat$GAS.ACCOUNTS))){
    if (levels(dat$GAS.ACCOUNTS)[lev] %in% cat.1 == T){
      levels(dat$GAS.ACCOUNTS)[lev] = "4 to 6"
    } else if (levels(dat$GAS.ACCOUNTS)[lev] %in% cat.2 == T){
      levels(dat$GAS.ACCOUNTS)[lev] = "7 to 9"
    } else if (levels(dat$GAS.ACCOUNTS)[lev] %in% cat.3 == T){
      levels(dat$GAS.ACCOUNTS)[lev] = "10 to 13"
    } else if (levels(dat$GAS.ACCOUNTS)[lev] %in% cat.4 == T){
      levels(dat$GAS.ACCOUNTS)[lev] = "14 to 21"
    } else if (levels(dat$GAS.ACCOUNTS)[lev] %in% cat.5 == T){
      levels(dat$GAS.ACCOUNTS)[lev] = "22 to 37"
    } else if (levels(dat$GAS.ACCOUNTS)[lev] %in% cat.6 == T){
      levels(dat$GAS.ACCOUNTS)[lev] = "38 and Greater"
    } 
  } 
}

# Recategorize the COMMUNITY.AREA.NAMES ---------------------------------------------------

Central <- c("Near North Side", "Loop", "Near South Side")
North.Side <- c("North Center", "Lakeview", "Lincoln Park", "Avondale", "Logan Square")
Far.North.Side <- c("Rogers Park", "West Ridge", "Uptown", "Lincoln Square", "Edison Park", 
                    "Norwood Park", "Jefferson Park", "Forest Glen","North Park", "Albany Park",
                    "O'Hare","Edgewater")
Northwest.Side <- c("Portage Park", "Irving Park", "Dunning", "Montclare", "Belmont Cragin","Hermosa")
West.Side <- c("Humboldt Park", "West Town", "Austin", "West Garfield Park", "East Garfield Park",
               "Near West Side", "North Lawndale", "South Lawndale", "Lower West Side")
South.Side <- c("Armour Square", "Douglas", "Oakland", "Fuller Park", "Grand Boulevard", "Kenwood", 
                "Washington Park", "Hyde Park", "Woodlawn", "South Shore", "Bridgeport", "Greater Grand Crossing")
Southwest.Side <- c("Garfield Ridge","Archer Heights", "Brighton Park", "McKinley Park", "New City", 
                    "West Elsdon", "Gage Park", "Clearing", "West Lawn", "Chicago Lawn", "West Englewood", "Englewood")
Far.Southeast.Side <- c("Chatham", "Avalon Park", "South Chicago", "Burnside", "Calumet Heights", 
                        "Roseland", "Pullman", "South Deering", "East Side", "West Pullman", "Riverdale", "Hegewisch")
Far.Southwest.Side <- c("Ashburn", "Auburn Gresham", "Beverly", "Washington Heights", "Mount Greenwood", "Morgan Park")

dat <- data.frame(dat, "Community.Area" = 0)
dat$Community.Area[1] = "Far.North.Side"
for (row in 1:nrow(dat)){
  if (dat$COMMUNITY.AREA.NAME[row] %in% Central == T){
    dat$Community.Area[row] = "Central"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% North.Side == T){
    dat$Community.Area[row] = "North.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% Far.North.Side == T){
    dat$Community.Area[row] = "Far.North.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% Northwest.Side == T){
    dat$Community.Area[row] = "Northwest.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% West.Side == T){
    dat$Community.Area[row] = "West.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% South.Side == T){
    dat$Community.Area[row] = "South.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% Southwest.Side == T){
    dat$Community.Area[row] = "Southwest.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% Far.Southeast.Side == T){
    dat$Community.Area[row] = "Far.Southeast.Side"
  }
  if (dat$COMMUNITY.AREA.NAME[row] %in% Far.Southwest.Side == T){
    dat$Community.Area[row] = "Far.Southwest.Side"
  }
}
dat$Community.Area <- as.factor(dat$Community.Area)


# Log Scale of Mean Values -------------------------------------------------------

dat$KWH.MEAN.2010 <- log(1+dat$KWH.MEAN.2010)
dat$THERM.MEAN.2010 <- log(1+dat$THERM.MEAN.2010)

# Split the datasets

# kWh Data ----------------------------------------------------------------

# Create 2 datasets: kwh.dat and kwh.ms

kwh.dat <- dat[, which(colnames(dat) %in% c("Community.Area",
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

# Split months

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
months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset for kWh

kwh.ms <- data.frame(kwh.dat, "KWH.USE" = months.dat[,1], "months" = labs[,1])

for (mon in 2:ncol(months.dat)){
  ds <- data.frame(kwh.dat, "KWH.USE" = months.dat[,mon], "months" = labs[,mon])
  kwh.ms <- rbind(kwh.ms, ds)
}
# Log scale 
kwh.ms$KWH.USE <- log(1+kwh.ms$KWH.USE)
kwh.ms <- kwh.ms[,-which(colnames(kwh.ms) == "KWH.MEAN.2010")]

# Therms Data -------------------------------------------------------------

# Create 2 datasets: therms.dat and therms.ms

therms.dat <- dat[, which(colnames(dat) %in% c("Community.Area",
                                               "CENSUS.BOCK",
                                               "BUILDING.TYPE",
                                               "BUILDING_SUBTYPE",
                                               "GAS.ACCOUNTS",
                                               "THERMS.SQFT.MEAN.2010",
                                               "TOTAL.POPULATION",
                                               "TOTAL.UNITS",
                                               "AVERAGE.STORIES",
                                               "AVERAGE.BUILDING.AGE",
                                               "AVERAGE.HOUSESIZE",
                                               "OCCUPIED.UNITS",
                                               "RENTER.OCCUPIED.HOUSING.UNITS",
                                               "OCCUPIED.HOUSING.UNITS",
                                               "THERM.MEAN.2010"))]

# Split months
lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
labs <- sapply(1:12, function(x) rep(lab[x], nrow(therms.dat)))

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

colnames <- sapply(1:length(months), function(x) paste0('THERM', '.', months[x], '.2010'))
months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset for kWh

therms.ms <- data.frame(therms.dat, "THERM.USE" = months.dat[,1], "months" = labs[,1])

for (mon in 2:ncol(months.dat)){
  ds <- data.frame(therms.dat, "THERM.USE" = months.dat[,mon], "months" = labs[,mon])
  therms.ms <- rbind(therms.ms, ds)
}

# Log scale 
therms.ms$KWH.USE <- log(1+therms.ms$THERM.USE)
therms.ms <- kwh.ms[,-which(colnames(therms.ms) == "THERM.MEAN.2010")]


# Split Train/Test --------------------------------------------------------

# For kwh

# train.kwhdat <- kwh.dat[1:(floor(nrow(kwh.dat)*0.75)),]
# test.kwhdat <- kwh.dat[(floor(nrow(kwh.dat)*0.75) + 1):(nrow(kwh.dat)),]
# 
# # train
# 
# lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# labs <- sapply(1:12, function(x) rep(lab[x], nrow(train.kwhdat)))
# 
# months <- c('JANUARY',
#             'FEBRUARY',
#             'MARCH',
#             'APRIL',
#             'MAY',
#             'JUNE',
#             'JULY',
#             'AUGUST',
#             'SEPTEMBER',
#             'OCTOBER',
#             'NOVEMBER',
#             'DECEMBER')
# 
# colnames <- sapply(1:length(months), function(x) paste0('KWH', '.', months[x], '.2010'))
# months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset for kWh
# months.dat <- months.dat[1:(floor(nrow(kwh.dat)*0.75)),]
# 
# train.kwhms <- data.frame(train.kwhdat, "KWH.USE" = months.dat[,1], "months" = labs[,1])
# 
# for (mon in 2:ncol(months.dat)){
#   ds <- data.frame(train.kwhdat, "KWH.USE" = months.dat[,mon], "months" = labs[,mon])
#   train.kwhms <- rbind(train.kwhms, ds)
# }
# 
# # Log scale 
# train.kwhms$KWH.USE <- log(1+train.kwhms$KWH.USE)
# train.kwhms <- train.kwhms[,-which(colnames(train.kwhms) == "KWH.MEAN.2010")]
# 
# # test
# 
# lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# labs <- sapply(1:12, function(x) rep(lab[x], nrow(test.kwhdat)))
# 
# months <- c('JANUARY',
#             'FEBRUARY',
#             'MARCH',
#             'APRIL',
#             'MAY',
#             'JUNE',
#             'JULY',
#             'AUGUST',
#             'SEPTEMBER',
#             'OCTOBER',
#             'NOVEMBER',
#             'DECEMBER')
# 
# colnames <- sapply(1:length(months), function(x) paste0('KWH', '.', months[x], '.2010'))
# months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset for kWh
# months.dat <- months.dat[(floor(nrow(kwh.dat)*0.75) + 1):(nrow(kwh.dat)),]
# 
# test.kwhms <- data.frame(test.kwhdat, "KWH.USE" = months.dat[,1], "months" = labs[,1])
# 
# for (mon in 2:ncol(months.dat)){
#   ds <- data.frame(test.kwhdat, "KWH.USE" = months.dat[,mon], "months" = labs[,mon])
#   test.kwhms <- rbind(test.kwhms, ds)
# }
# 
# # Log scale 
# test.kwhms$KWH.USE <- log(1+test.kwhms$KWH.USE)
# test.kwhms <- test.kwhms[,-which(colnames(test.kwhms) == "KWH.MEAN.2010")]
# 
# # For therms.dat
# 
# train.thermsdat <- therms.dat[1:(floor(nrow(therms.dat)*0.75)),]
# test.thermsdat <- therms.dat[(floor(nrow(therms.dat)*0.75) + 1):(nrow(therms.dat)),]
# 
# # train
# 
# lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# labs <- sapply(1:12, function(x) rep(lab[x], nrow(train.thermsdat)))
# 
# months <- c('JANUARY',
#             'FEBRUARY',
#             'MARCH',
#             'APRIL',
#             'MAY',
#             'JUNE',
#             'JULY',
#             'AUGUST',
#             'SEPTEMBER',
#             'OCTOBER',
#             'NOVEMBER',
#             'DECEMBER')
# 
# colnames <- sapply(1:length(months), function(x) paste0('THERM', '.', months[x], '.2010'))
# months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset for therms
# months.dat <- months.dat[1:(floor(nrow(therms.dat)*0.75)),]
# 
# train.thermsms <- data.frame(train.thermsdat, "THERMS.USE" = months.dat[,1], "months" = labs[,1])
# 
# for (mon in 2:ncol(months.dat)){
#   ds <- data.frame(train.thermsdat, "THERMS.USE" = months.dat[,mon], "months" = labs[,mon])
#   train.thermsms <- rbind(train.thermsms, ds)
# }
# 
# # Log scale 
# train.thermsms$THERMS.USE <- log(1+train.thermsms$THERMS.USE)
# train.thermsms <- train.thermsms[,-which(colnames(train.thermsms) == "THERM.MEAN.2010")]
# 
# # test
# 
# lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# labs <- sapply(1:12, function(x) rep(lab[x], nrow(test.thermsdat)))
# 
# months <- c('JANUARY',
#             'FEBRUARY',
#             'MARCH',
#             'APRIL',
#             'MAY',
#             'JUNE',
#             'JULY',
#             'AUGUST',
#             'SEPTEMBER',
#             'OCTOBER',
#             'NOVEMBER',
#             'DECEMBER')
# 
# colnames <- sapply(1:length(months), function(x) paste0('THERM', '.', months[x], '.2010'))
# months.dat <- dat[, which(colnames(dat) %in% colnames)] #months dataset for kWh
# months.dat <- months.dat[(floor(nrow(therms.dat)*0.75) + 1):(nrow(therms.dat)),]
# 
# test.thermsms <- data.frame(test.thermsdat, "THERMS.USE" = months.dat[,1], "months" = labs[,1])
# 
# for (mon in 2:ncol(months.dat)){
#   ds <- data.frame(test.thermsdat, "THERMS.USE" = months.dat[,mon], "months" = labs[,mon])
#   test.thermsms <- rbind(test.thermsms, ds)
# }
# 
# # Log scale 
# test.thermsms$THERMS.USE <- log(1+test.thermsms$THERMS.USE)
# test.thermsms <- test.thermsms[,-which(colnames(test.thermsms) == "THERM.MEAN.2010")]
# PCA kWh -----------------------------------------------------------------

## train dataset 
# Get the numeric dataset
nums <- unlist(lapply(kwh.dat, is.numeric))
num.kwh <- kwh.dat[ , nums]

# standardize the datase
sd.num.kwh <- num.kwh
for (col in 1:ncol(sd.num.kwh)){
  sd.num.kwh[,col] <- (sd.num.kwh[,col] - mean(sd.num.kwh[,col]))/sd(sd.num.kwh[,col])
}

princomp.kwh <- princomp(sd.num.kwh) # Get the variance plot
prcomp.kwh <- prcomp(sd.num.kwh)
princomp.kwh$loadings 
# Shows that about 8 components explain 80% of the data
# Get PC's from this set

## test dataset
# Get the numeric dataset
# nums <- unlist(lapply(test.kwhdat, is.numeric))
# num.kwh.test <- test.kwhdat[ , nums]
# 
# # standardize the datase
# sd.num.kwh.test <- num.kwh.test
# for (col in 1:ncol(sd.num.kwh.test)){
#   sd.num.kwh.test[,col] <- (sd.num.kwh.test[,col] - mean(sd.num.kwh.test[,col]))/sd(sd.num.kwh.test[,col])
# }
# 
# princomp.kwh.test <- princomp(sd.num.kwh.test) # Get the variance plot
# prcomp.kwh.test <- prcomp(sd.num.kwh.test)
# princomp.kwh.test$loadings 
# Shows that about 8 components explain 80% of the data
# Get PC's from this set

# PCA Therms  -----------------------------------------------------------------

## train dataset
# Get the numeric dataset
nums <- unlist(lapply(therms.dat, is.numeric))
num.therms <- therms.dat[ , nums]

# standardize the datase
sd.num.therms <- num.therms
for (col in 1:ncol(sd.num.therms)){
  sd.num.therms[,col] <- (sd.num.therms[,col] - mean(sd.num.therms[,col]))/sd(sd.num.therms[,col])
}

princomp.therms <- princomp(sd.num.therms)# Get the variance plot
prcomp.therms <- prcomp(sd.num.therms)
princomp.therms$loadings 
# Shows that about 8 components explain 80% of the data
# Get PC's from this set

# ## test dataset
# # Get the numeric dataset
# nums <- unlist(lapply(test.thermsdat, is.numeric))
# num.therms.test <- test.thermsdat[ , nums]
# 
# # standardize the datase
# sd.num.therms.test <- num.therms.test
# for (col in 1:ncol(sd.num.therms.test)){
#   sd.num.therms.test[,col] <- (sd.num.therms.test[,col] - mean(sd.num.therms.test[,col]))/sd(sd.num.therms.test[,col])
# }
# 
# princomp.therms.test <- princomp(sd.num.therms.test)# Get the variance plot
# prcomp.therms.test <- prcomp(sd.num.therms.test)
# princomp.therms.test$loadings 
# Shows that about 8 components explain 80% of the data
# Get PC's from this set

# Analysis ----------------------------------------------------------------

# PCA/Linear kWh ---------------------------------------------------------------------

# Linear on train

nums <- unlist(lapply(kwh.dat, is.numeric))

pca.linearfit.kwh <- data.frame("PC1" = prcomp.kwh$x[,1], 
                                "PC2" = prcomp.kwh$x[,2], 
                                "PC3" = prcomp.kwh$x[,3], 
                                kwh.dat[, (nums == F)], 
                                "KWH.MEAN.2010" = kwh.dat$KWH.MEAN.2010)

pca.fit.kwh <- lm(KWH.MEAN.2010 ~., dat = pca.linearfit.kwh)
summary(pca.fit.kwh)

# Plots to check linearity
res.kwh <- resid(pca.fit.kwh)
means.kwn <- kwh.dat$KWH.MEAN.2010[-which.min(res.kwh)]
res.kwh <- res.kwh[-which.min(res.kwh)]
plot(means.kwn, res.kwh,
     ylab = "Residuals", xlab = "kWh Mean")
abline(0,0)
qqnorm(res.kwh) # shows a heavy tailed distribution
qqline(res.kwh) 
plot(density(resid(pca.fit.kwh)))

mean(pca.fit.kwh$residuals^2) # the mse so maybe i didn't need to split into test and train -.-
# mean((pca.linearfit.kwh$KWH.MEAN.2010 - predict(pca.fit.kwh))^2)
# nums <- unlist(lapply(test.kwhdat, is.numeric))
# 
# pca.linearfit.kwh.test <- data.frame("PC1" = prcomp.kwh.test$x[,1], 
#                                 "PC2" = prcomp.kwh.test$x[,2], 
#                                 "PC3" = prcomp.kwh.test$x[,3], 
#                                 test.kwhdat[, (nums == F)], 
#                                 "KWH.MEAN.2010" = test.kwhdat$KWH.MEAN.2010)
# 
# pred.kwh <- predict(pca.fit.kwh, pca.linearfit.kwh.test)
# mse(test.kwhdat$KWH.MEAN.2010, pred.kwh)

# PCA/Linear Therms -----------------------------------------------------------

# Linear on train

nums <- unlist(lapply(therms.dat, is.numeric))

pca.linearfit.therms <- data.frame("PC1" = prcomp.therms$x[,1], 
                                   "PC2" = prcomp.therms$x[,2], 
                                   "PC3" = prcomp.therms$x[,3], 
                                   therms.dat[, (nums == F)], 
                                   "THERM.MEAN.2010" = therms.dat$THERM.MEAN.2010)

pca.fit.therms <- lm(THERM.MEAN.2010 ~., dat = pca.linearfit.therms)
summary(pca.fit.therms)

# Plots to check linearity
res.therms <- resid(pca.fit.therms)
means.therms <- therms.dat$THERM.MEAN.2010[-which.min(res.therms)]
res.therms <- res.therms[-which.min(res.therms)]
plot(means.therms, res.therms,
     ylab = "Residuals", xlab = "Therms Mean")
abline(0,0)
qqnorm(res.therms) # shows a heavy tailed distribution
qqline(res.therms)
plot(density(resid(pca.fit.therms)))

# Predict on test
mean(pca.fit.therms$residuals^2) # the mse so maybe i didn't need to split into test and train -.-

# nums <- unlist(lapply(test.kwhdat, is.numeric))
# 
# pca.linearfit.therms.test <- data.frame("PC1" = prcomp.therms.test$x[,1], 
#                                    "PC2" = prcomp.therms.test$x[,2], 
#                                    "PC3" = prcomp.therms.test$x[,3], 
#                                    test.thermsdat[, (nums == F)], 
#                                    "THERM.MEAN.2010" = test.thermsdat$THERM.MEAN.2010)
# 
# predict(pca.fit.therms, test.thermsdat)


# Splines kWh -----------------------------------------------------------------

levels(kwh.ms$months) <- c('1', '2', '3', '4','5', '6', '7', '8', '9', '10', '11', '12')
kwh.ms$months <- as.numeric(kwh.ms$months)
bs.kwh <- lm(KWH.USE ~ AVERAGE.BUILDING.AGE + BUILDING_SUBTYPE +
              OCCUPIED.HOUSING.UNITS + KWH.SQFT.MEAN.2010 +
              ELECTRICITY.ACCOUNTS*bs(months, df=3, knots = 2), data=kwh.ms) 
summary(bs.kwh)
plot(kwh.ms$months, bs.kwh$fitted.values) # mean per month

mean(bs.kwh$residuals^2)
mean((kwh.ms$KWH.USE - predict(bs.kwh))^2)

pred.kwh <- predict(bs.kwh,interval="prediction",se.fit=TRUE)
fitted.kwh <- pred.kwh$fit

kwh.mean.hat <- data.frame("KWH.MEAN.2010" = kwh.dat$KWH.MEAN.2010, "FITTED.MEAN" = 0)
for (obs in 1:nrow(kwh.dat)){
  kwh.mean.hat[obs,2] <- sum(sapply(seq(0,11), function(x) fitted.kwh[(obs + x*nrow(kwh.dat))]))/12 
}

mean((kwh.mean.hat$KWH.MEAN.2010 - kwh.mean.hat$FITTED.MEAN)^2)

# Splines Therms -----------------------------------------------------------------

levels(therms.ms$months) <- c('1', '2', '3', '4','5', '6', '7', '8', '9', '10', '11', '12')
therms.ms$months <- as.numeric(therms.ms$months)
# bs.therms <- lm(THERMS.USE ~ AVERAGE.BUILDING.AGE + BUILDING_SUBTYPE +
#                OCCUPIED.HOUSING.UNITS + THERMS.SQFT.MEAN.2010 +
#                GAS.ACCOUNTS*bs(months, df=3, knots = 5), data=therms.ms) 
# summary(bs.therms)

bs.therms <- lm(THERMS.USE ~ . +
                  BUILDING_SUBTYPE*bs(months, df=3, knots = 2), data=therms.ms) 
summary(bs.therms)
mean(bs.therms$residuals^2)

set.seed(3)
elasticnet_therms <- cv.glmnet(data.matrix(therms.ms[,-which(colnames(therms.ms) == "THERMS.USE")]), 
                          therms.ms$THERMS.USE, nfolds = 10, alpha = .5)
coef(elasticnet_therms, s = "lambda.1se") # lambda 1se works better 
par(mfrow = c(1, 2))
plot(elasticnet_therms)
plot(elasticnet_therms$glmnet.fit, "lambda")
#lambda is being tuned.
min(elasticnet_therms$lambda)

bs.therms <- lm(THERMS.USE~.-OCCUPIED.UNITS - OCCUPIED.HOUSING.UNITS - BUILDING_SUBTYPE - TOTAL.UNITS - AVERAGE.HOUSESIZE
                + BUILDING.TYPE*bs(months, df=3, knots = 5), data=therms.ms) 
summary(bs.therms)
mean(bs.therms$residuals^2) # this doesn't really improve the mean but makes the model less complex


pred.therms <- predict(bs.therms,interval="prediction",se.fit=TRUE)
fitted.therms <- pred.therms$fit

therms.mean.hat <- data.frame("THERM.MEAN.2010" = therms.dat$THERM.MEAN.2010, "FITTED.MEAN" = 0)
for (obs in 1:nrow(therms.dat)){
  therms.mean.hat[obs,2] <- sum(sapply(seq(0,11), function(x) fitted.therms[(obs + x*nrow(therms.dat))]))/12 
}

mean((therms.mean.hat$THERM.MEAN.2010 - therms.mean.hat$FITTED.MEAN)^2)


# Random Forest kWh -------------------------------------------------------

rf.kwh <- randomForest(num.kwh, kwh.dat$BUILDING_SUBTYPE, 
                       ntree = 1000, mtry = 1, nodesize = 20, sampsize = 500, importance = TRUE)
rf.kwh

# Tuning the rf model

nodes <- c(1, 20, 40, 60) #picking some node values to test
mtrys <- c(1, 3, 6) #keeping the center value as the default 
ntrees <- c(200, 500, 1000)
best <- c(10,10,10, 10) #initating the best(error, mtry, nodesize)

for (ntree in ntrees){
  print(ntree)
  for (nodesize in nodes){
    print(nodesize)
    for (mtry in mtrys){
      print(mtry)
      rf.fit <- randomForest(dat.numeric, kwh.dat$BUILDING_SUBTYPE, 
                             ntree = ntree, mtry = mtry, nodesize = nodesize)
      OOB.err <- mean(predict(rf.fit) != kwh.dat$BUILDING_SUBTYPE)
      #retain the information of only those predictors that have the lowest prediction error
      ifelse(OOB.err < best[1], best <- c(OOB.err, mtry, nodesize, ntree), best <- best)
      print(best)
    }
  }
}

# Tuned fit 

rf.kwh.tune <- randomForest(num.kwh, kwh.dat$BUILDING_SUBTYPE, 
                            ntree = 1000, mtry = 6, nodesize = 1)
rf.kwh.tune
# plot(rf.kwh.tune)
varImpPlot(rf.kwh.tune)

# Random Forest Therms -------------------------------------------------------

rf.therms <- randomForest(num.therms, therms.dat$BUILDING_SUBTYPE, 
                          ntree = 1000, mtry = 1, nodesize = 20, sampsize = 500, importance = TRUE)
rf.therms

# Tuning the rf model

nodes <- c(1, 20, 40, 60) #picking some node values to test
mtrys <- c(1, 3, 6) #keeping the center value as the default 
ntrees <- c(200, 500, 1000)
best <- c(10,10,10, 10) #initating the best(error, mtry, nodesize)

for (ntree in ntrees){
  print(ntree)
  for (nodesize in nodes){
    print(nodesize)
    for (mtry in mtrys){
      print(mtry)
      rf.fit <- randomForest(num.therms, therms.dat$BUILDING_SUBTYPE, 
                             ntree = ntree, mtry = mtry, nodesize = nodesize)
      OOB.err <- mean(predict(rf.fit) != therms.dat$BUILDING_SUBTYPE)
      #retain the information of only those predictors that have the lowest prediction error
      ifelse(OOB.err < best[1], best <- c(OOB.err, mtry, nodesize, ntree), best <- best)
      print(best)
    }
  }
}

# 6.0000000  20.0000000 500.0000000

# Tuned fit 

rf.therms.tune <- randomForest(num.therms, therms.dat$BUILDING_SUBTYPE, 
                               ntree = 500, mtry = 6, nodesize = 20, importance = TRUE)
rf.therms.tune
# plot(rf.therms.tune)
varImpPlot(rf.therms.tune)



