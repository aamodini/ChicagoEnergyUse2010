#########################
# Process the data set # example
#######################

small.dat <- dat[dat == "Albany Park",]
#each obervation is the information of 1 person. 
#Need to split up the data that excludes all the numerical factors of the variables 
#theres a lot of dependency of variables

#These 15 variables are the ones that are specifically
#Look at kWh specifically first 
#Therms can be converted into kWh - so we should expect similar results from Therms and kWh
new.small.dat <- small.dat[, which(colnames(small.dat) %in% c("COMMUNITY.AREA.NAME",
                                                              "CENSUS.BOCK",
                                                              "BUILDING.TYPE",
                                                              "BUILDING.SUBTYPE",
                                                              "ELECTRICITY.ACCOUNTS",
                                                              "ZERO.KWH.ACCOUNTS",
                                                              "KWH.TOTAL.SQFT",
                                                              "KWH.SQFT.MEAN.2010",
                                                              "TOTAL.POPULATION",
                                                              "TOTAL.UNITS",
                                                              "AVERAGE.STORIES",
                                                              "AVERAGE.BUILDING.AGE",
                                                              "AVERAGE.HOUSE.SIZE",
                                                              "OCCUPIED.UNITS",
                                                              "RENTER.OCCUPIED.HOUSING.UNITS",
                                                              "OCCUPIED.HOUSING.UNITS",
                                                              "TOTAL.KWH"))]

#These additional variiables for Therms instead of kWh.
# "THERMS.TOTAL.SQFT",
# "THERM.MEAN.2010",
# "THERM.SQFT.MEAN.2010",
# "GAS.ACCOUNTS",

#Categorize the dataset - label 

lab <- c('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
labs <- sapply(1:12, function(x) rep(lab[x], nrow(new.small.dat)))

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
months.small.dat <- small.dat[, which(colnames(small.dat) %in% colnames)] #months dataset

final.ds <- data.frame(new.small.dat, "KWH.USE" = months.small.dat[,1], "months" = labs[,1])

for (mon in 2:ncol(months.small.dat)){
  ds <- data.frame(new.small.dat, "KWH.USE" = months.small.dat[,mon], "months" = labs[,mon])
  final.ds <- rbind(final.ds, ds)
}

# y values are KWH.USE
# Look at its relation with other prediction values
plot(final.ds$OCCUPIED.UNITS, final.ds$KWH.USE, pch = 19,
     col = c("indianred","deepskyblue", "darkorange", "purple", "darkred", "yellow", 
             "darkblue", "blue", "pink", "orange", "green", "violet")[final.ds$months])

plot(final.ds$KWH.TOTAL.SQFT, final.ds$KWH.USE, pch = 19,
     col = c("indianred","deepskyblue", "darkorange", "purple", "darkred", "yellow", 
             "darkblue", "blue", "pink", "orange", "green", "violet")[final.ds$months])

plot(new.small.dat$TOTAL.POPULATION, new.small.dat$TOTAL.KWH, pch = 19,
     col = c("indianred","deepskyblue", "darkorange", "purple")[new.small.dat$BUILDING.TYPE])

plot(new.small.dat$TOTAL.POPULATION, new.small.dat$TOTAL.KWH, pch = 19,
     col = c("indianred","deepskyblue", "darkorange", "purple")[new.small.dat$BUILDING.TYPE])

boxplot(final.ds$KWH.USE ~ final.ds$months, col=c("indianred","deepskyblue", "darkorange", "purple", "darkred", "yellow", 
                                                  "darkblue", "blue", "pink", "orange", "green", "violet"))

#there are too many points that are out of range just for Albany Park

boxplot(new.small.dat$TOTAL.KWH ~ new.small.dat$BUILDING.TYPE, col=c("deepskyblue", "darkorange", "purple"))

#this is a little better

hist(new.small.dat$AVERAGE.STORIES, xlab = "Average Stories")

##########################################################
# K Means isn't really working
##########################################################
# by month
kwh.kmeans.mon <- kmeans(final.ds[, 16], centers = 12, nstart = 1000)
table(kwh.kmeans.mon$cluster,final.ds$months)

# by season
kwh.kmeans.sea <- kmeans(final.ds[, 16], centers = 4, nstart = 500)
table(kwh.kmeans.sea$cluster,final.ds$season)

# by building type - there are 3 levels
kwh.kmeans.bt <- kmeans(dat$KWH.MEAN.2010, centers = 3, nstart = )
table(kwh.kmeans.bt$cluster,dat$BUILDING.TYPE)

# by building subtype - 6 subtypes
kwh.kmeans.bst <- kmeans(dat$KWH.MEAN.2010, centers = 6, nstart = 20)
table(kwh.kmeans.bst$cluster,dat$BUILDING_SUBTYPE)


##########################################################
# Lasso doesn't work either cause of too much colinearity
##########################################################
set.seed(3)
lasso_all <- cv.glmnet(data.matrix(kwh.dat[,-(which(colnames(kwh.dat) == "KWH.MEAN.2010"))]), kwh.dat$KWH.MEAN.2010, nfolds = 10)
coef(lasso_all, s = "lambda.min")
par(mfrow = c(1, 2))
plot(lasso_all)
plot(lasso_all$glmnet.fit, "lambda")
#lambda is being tuned.
min(lasso_all$lambda)
