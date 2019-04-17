# Data Processing

library(ggplot2)
library(randomForest)
library(tree)
library(glmnet)
library(MASS)

dat.old <- read.csv("~/Documents/Hogwarts/Graduate UIUC/First Year/STAT 432/energy-usage-2010.csv")
dat <- dat.old[complete.cases(dat.old),]

#the labels are bit weird so I've just redefined them here.
dat$BUILDING.TYPE <- droplevels(dat$BUILDING.TYPE)
dat$BUILDING.TYPE <- as.factor(dat$BUILDING.TYPE)

dat$BUILDING_SUBTYPE <- droplevels(dat$BUILDING_SUBTYPE)
dat$BUILDING_SUBTYPE <- as.factor(dat$BUILDING_SUBTYPE)

dat$ELECTRICITY.ACCOUNTS <- droplevels(dat$ELECTRICITY.ACCOUNTS)
dat$ELECTRICITY.ACCOUNTS <- as.factor(dat$ELECTRICITY.ACCOUNTS)

dat$GAS.ACCOUNTS <- droplevels(dat$GAS.ACCOUNTS)
dat$GAS.ACCOUNTS <- as.factor(dat$GAS.ACCOUNTS)

##########################
# For the whole dataset #
########################

# BUILDING.TYPE

ggplot(dat, aes(x=BUILDING.TYPE)) +
  geom_bar()

## get rid of Industrial

dat <- dat[dat$BUILDING.TYPE != "Industrial",]
dat$BUILDING.TYPE <- factor(dat$BUILDING.TYPE)

ggplot(dat, aes(x=BUILDING.TYPE)) +
  geom_bar()

# BUILDING_SUBTYPE

ggplot(dat, aes(x=BUILDING_SUBTYPE)) +
  geom_bar()

## get rid of Municipal

dat <- dat[dat$BUILDING_SUBTYPE != "Municipal",]
dat$BUILDING_SUBTYPE <- factor(dat$BUILDING_SUBTYPE)

ggplot(dat, aes(x=BUILDING_SUBTYPE)) +
  geom_bar()

# KWH.MEAN.2010

ggplot(dat, aes(x=KWH.MEAN.2010)) + geom_histogram()
ggplot(dat, aes(x=THERM.MEAN.2010)) + geom_histogram()

dat$KWH.MEAN.2010 <- log(1+dat$KWH.MEAN.2010)
dat$THERM.MEAN.2010 <- log(1+dat$THERM.MEAN.2010)

ggplot(dat, aes(x=KWH.MEAN.2010)) + geom_histogram(bins=30)
ggplot(dat, aes(x=THERM.MEAN.2010)) + geom_histogram(bins=30)

# Recategorize the ELECTRICITY.ACCOUNTS 

ggplot(dat, aes(x=ELECTRICITY.ACCOUNTS)) +
  geom_bar()

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
# need to run this several time for some reason

ggplot(dat, aes(x=ELECTRICITY.ACCOUNTS)) +
  geom_bar()

# Recategorize the GAS.ACCOUNTS 

ggplot(dat, aes(x=GAS.ACCOUNTS)) +
  geom_bar()

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

####################
# For kWh dataset #
##################

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






#######################
# For Therms dataset #
#####################

therms.dat <- dat[, which(colnames(dat) %in% c("Community.Area",
                                            "CENSUS.BOCK",
                                            "BUILDING.TYPE",
                                            "BUILDING_SUBTYPE",
                                            "GAS.ACCOUNTS",
                                            "ZERO.KWH.ACCOUNTS",
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
ggplot(therms.dat, aes(x=GAS.ACCOUNTS)) +
  geom_bar()


###################################
# For kWh month + season dataset #
#################################

# just for plotting purpose make categories characters
# months
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

# season - don't run

# kwh.ms <- data.frame(kwh.ms, 'season' = 1)
# 
# for (labels in lab) {
#   if (labels == 'Mar'){
#     kwh.ms[which(kwh.ms$months == "Mar"), ]$season = 2
#   }
#   if (labels == 'Apr'){
#     kwh.ms[which(kwh.ms$months == "Apr"), ]$season = 2
#   }
#   if (labels == 'May'){
#     kwh.ms[which(kwh.ms$months == "May"), ]$season = 2
#   }
#   if (labels == 'Jun'){
#     kwh.ms[which(kwh.ms$months == "Jun"), ]$season = 3
#   }
#   if (labels == 'Jul'){
#     kwh.ms[which(kwh.ms$months == "Jul"), ]$season = 3
#   }
#   if (labels == 'Aug'){
#     kwh.ms[which(kwh.ms$months == "Aug"), ]$season = 3
#   }
#   if (labels == 'Sep'){
#     kwh.ms[which(kwh.ms$months == "Sep"), ]$season = 4
#   }
#   if (labels == 'Oct'){
#     kwh.ms[which(kwh.ms$months == "Oct"), ]$season = 4
#   }
#   if (labels == 'Nov'){
#     kwh.ms[which(kwh.ms$months == "Nov"), ]$season = 4
#   }
# }
# 
# kwh.ms$season <- as.factor(kwh.ms$season)
# 
# # Process the seasonal dataset
# 
# kwh.ms$KWH.USE <- log(1+kwh.ms$KWH.USE)

######################################
# For Therms month + season dataset #
####################################

# just for plotting purpose make categories characters
# months
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

# season

# therms.ms <- data.frame(therms.ms, 'season' = 1)
# 
# for (labels in lab) {
#   if (labels == 'Mar'){
#     therms.ms[which(therms.ms$months == "Mar"), ]$season = 2
#   }
#   if (labels == 'Apr'){
#     therms.ms[which(therms.ms$months == "Apr"), ]$season = 2
#   }
#   if (labels == 'May'){
#     therms.ms[which(therms.ms$months == "May"), ]$season = 2
#   }
#   if (labels == 'Jun'){
#     therms.ms[which(therms.ms$months == "Jun"), ]$season = 3
#   }
#   if (labels == 'Jul'){
#     therms.ms[which(therms.ms$months == "Jul"), ]$season = 3
#   }
#   if (labels == 'Aug'){
#     therms.ms[which(therms.ms$months == "Aug"), ]$season = 3
#   }
#   if (labels == 'Sep'){
#     therms.ms[which(therms.ms$months == "Sep"), ]$season = 4
#   }
#   if (labels == 'Oct'){
#     therms.ms[which(therms.ms$months == "Oct"), ]$season = 4
#   }
#   if (labels == 'Nov'){
#     therms.ms[which(therms.ms$months == "Nov"), ]$season = 4
#   }
# }
# 
# therms.ms$season <- as.factor(therms.ms$season)
# 
# # Process the seasonal dataset
# 
# therms.ms$THERM.USE <- log(1+therms.ms$THERM.USE)

###################################
# Visualization for both datasets #
###################################

# ggplot(data=kwh.ms, aes(x=months, y=KWH.USE, group=1)) +
#   xlab("Months") + ylab("Average kWh Consumption") + ggtitle("Average kWh Consumption per Month") +
#   geom_line(stat='summary', fun.y='mean') +
#   geom_point(stat='summary', fun.y='mean')
# ggplot(data=therms.ms, aes(x=months, y=THERM.USE, group=1)) +
#   xlab("Months") + ylab("Average Therm Consumption") + ggtitle("Average Therm Consumption per Month") +
#   geom_line(stat='summary', fun.y='mean') +
#   geom_point(stat='summary', fun.y='mean')
# 
# 
# ggplot(data=kwh.ms, aes(x=months, y=KWH.USE, group=BUILDING_SUBTYPE, color = BUILDING_SUBTYPE)) +
#   xlab("Month") + ylab("kWh Consumption") + ggtitle("kWh Consumption by Building Subtype per Month") +
#   geom_line(stat='summary', fun.y='mean') +
#   geom_point(stat='summary', fun.y='mean')
# ggplot(data=therms.ms, aes(x=months, y=THERM.USE, group=BUILDING_SUBTYPE, color = BUILDING_SUBTYPE)) +
#   xlab("Month") + ylab("Therm Consumption") + ggtitle("Therm Consumption by Building Subtype per Month") +
#   geom_line(stat='summary', fun.y='mean') +
#   geom_point(stat='summary', fun.y='mean')
# 
# 
# ggplot(data=kwh.dat, aes(x=AVERAGE.HOUSESIZE, y=KWH.MEAN.2010, group=1)) +
#   xlab("Average House Size") + ylab("kWh Mean") +
#   geom_line(stat='summary', fun.y='mean') +
#   geom_point(stat='summary', fun.y='mean')
# ggplot(data=therms.dat, aes(x=AVERAGE.HOUSESIZE, y=THERM.MEAN.2010, group=1)) +
#   xlab("Average House Size") + ylab("Therm Mean") +
#   geom_line(stat='summary', fun.y='mean') +
#   geom_point(stat='summary', fun.y='mean')


##########################
# Train/Test kWh dataset #
##########################

x.train.kwhdat <- kwh.dat[1:ceiling(nrow(kwh.dat)*.75), -5]
y.train.kwhdat <- kwh.dat[1:ceiling(nrow(kwh.dat)*.75), 5]

x.test.kwhdat <- kwh.dat[1:ceiling(nrow(kwh.dat)*.25), -5]
y.test.kwhdat <- kwh.dat[1:floor(nrow(kwh.dat)*.25), 5]

#############################
# Train/Test Therms dataset #
#############################


















