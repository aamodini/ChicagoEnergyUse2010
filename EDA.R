# Libraries ---------------------------------------------------------

library(ggplot2)
library(randomForest)
library(tree)
library(glmnet)
library(MASS)

# Data Exploration --------------------------------------------------------

# Read Data ---------------------------------------------------------------

dat.old <- read.csv("energy-usage-2010.csv")

# check how much of the data is missing
(1 - nrow(dat)/nrow(dat.old))*100 

# 31.57% of the data was removed - makes sense because of the NA values present in the categorical variables
# randomly assigning values to the categorical variables could introduce a lot of bias 
# when categorising between the months
dat <- dat.old[complete.cases(dat.old),]

# 68 numeric and 5 categorical features

# Redefine levels ---------------------------------------------------------
# just making sure the correct levels are considered

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

ggplot(dat, aes(x=BUILDING.TYPE)) +
  geom_bar()

# 2 observations for Industrial buildings so remove 'Industrial'
## get rid of Industrial

dat <- dat[dat$BUILDING.TYPE != "Industrial",]
dat$BUILDING.TYPE <- factor(dat$BUILDING.TYPE)

# plot looks more reasonable

ggplot(dat, aes(x=BUILDING.TYPE)) +
  geom_bar()

# BUILDING_SUBTYPE

ggplot(dat, aes(x=BUILDING_SUBTYPE)) +
  geom_bar()

# very few observations for `municipal`
## get rid of Municipal

dat <- dat[dat$BUILDING_SUBTYPE != "Municipal",]
dat$BUILDING_SUBTYPE <- factor(dat$BUILDING_SUBTYPE)

ggplot(dat, aes(x=BUILDING_SUBTYPE)) +
  geom_bar()

# KWH.MEAN.2010 and THERM.MEAN.2010

ggplot(dat, aes(x=KWH.MEAN.2010)) + geom_histogram()
ggplot(dat, aes(x=THERM.MEAN.2010)) + geom_histogram()

# histograms looks too skinny. Might be worth rescaling the dataset

dat$KWH.MEAN.2010 <- log(1+dat$KWH.MEAN.2010)
dat$THERM.MEAN.2010 <- log(1+dat$THERM.MEAN.2010)

ggplot(dat, aes(x=KWH.MEAN.2010)) + geom_histogram(bins=30)
ggplot(dat, aes(x=THERM.MEAN.2010)) + geom_histogram(bins=30)

# looks more like a normal distribution so the assumption holds

# Recategorize the ELECTRICITY.ACCOUNTS 

ggplot(dat, aes(x=ELECTRICITY.ACCOUNTS)) +
  geom_bar()

# there are too many categories
# extract the categories
values <- levels(dat$ELECTRICITY.ACCOUNTS)
values <- values[-length(values)]
values <- as.numeric(values)
values <- sort(values) 

# and now recategorize this
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
# need to run this several time for some reason

ggplot(dat, aes(x=ELECTRICITY.ACCOUNTS)) +
  geom_bar()

# Recategorize the GAS.ACCOUNTS 
ggplot(dat, aes(x=GAS.ACCOUNTS)) +
  geom_bar()

# there are too many categories
# extract the categories

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
# need to run this several time for some reason

ggplot(dat, aes(x=GAS.ACCOUNTS)) +
  geom_bar()

# Recategorize the COMMUNITY.AREA.NAMES 

# there are too many categories
# to recategorize, try to categorize using https://en.wikipedia.org/wiki/Community_areas_in_Chicago 
# into  9 geographic regions

ggplot(dat, aes(x=COMMUNITY.AREA.NAME)) +
  geom_bar()

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

ggplot(dat, aes(x=Community.Area)) +
  geom_bar()


# Split the datasets between kWh and therms

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

# look at this plot

ggplot(data=kwh.ms, aes(x=months, y=KWH.USE, group=1)) +
  xlab("Months") + ylab("Avg kWh Consumption") + ggtitle("Average kWh Consumption per Month") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  geom_line(stat='summary', fun.y='mean', col = "cadetblue") +
  geom_point(stat='summary', fun.y='mean', col = "cadetblue")

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

# look at this plot

ggplot(data=therms.ms, aes(x=months, y=THERM.USE, group=1)) +
  xlab("Months") + ylab("Avg Therm Consumption") + 
  ggtitle("Average Therm Consumption per Month") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  geom_line(stat='summary', fun.y='mean', col = "steelblue3") +
  geom_point(stat='summary', fun.y='mean', col = "steelblue3")

# PCA kWh -----------------------------------------------------------------

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


# PCA Therms  -----------------------------------------------------------------

# Get the numeric dataset
nums <- unlist(lapply(therms.dat, is.numeric))
num.therms <- therms.dat[ , nums]

# standardize the datase
sd.num.therms <- num.therms
for (col in 1:ncol(sd.num.therms)){
  sd.num.therms[,col] <- (sd.num.therms[,col] - mean(sd.num.therms[,col]))/sd(sd.num.therms[,col])
}

princomp.therms <- princomp(sd.num.therms) # Get the variance plot
prcomp.therms <- prcomp(sd.num.therms)

# looking at the variables that contribute to the most variation in the data
plot(princomp.kwh, type = "l", pch = 19, main = "Energy Use (kWh) PCA Variance", col = "cadetblue")

# pairwise plots of the first three components
pairs(prcomp.kwh$x[, 1:3], 
      col=c("chartreuse4", "darkorange", "deepskyblue", "purple")[kwh.dat$BUILDING_SUBTYPE], 
      cex = .3, pch = 19, main = "Pairwise Plot for Principal Components")

# color seperation shows greatest separation by Building Subtype
# there may be factors that vary across building subtypes

# look at loadings for PCA kWh
prcomp.kwh$rotation[,1:3]

# look at kWh 

ggplot(data=kwh.ms, aes(x=months, y=KWH.USE, group=BUILDING_SUBTYPE, color = BUILDING_SUBTYPE)) +
  xlab("Month") + ylab("kWh Consumption") + 
  ggtitle("kWh Consumption by Building Subtype per Month") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  geom_line(stat='summary', fun.y='mean') +
  geom_point(stat='summary', fun.y='mean')

# look at Therms

ggplot(data=therms.ms, aes(x=months, y=THERM.USE, group=BUILDING_SUBTYPE, color = BUILDING_SUBTYPE)) +
  xlab("Month") + ylab("Therm Consumption") + 
  ggtitle("Therm Consumption by Building Subtype per Month") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  geom_line(stat='summary', fun.y='mean') +
  geom_point(stat='summary', fun.y='mean')

# For kWh, the Commercial buildings have the highest expenditure, 
# and households with less than 7 occupants have the lowest expenditure. 

# For Therms, households with over 7 occupants have the highest expenditure
# and single family homes have the lowest expenditure.










