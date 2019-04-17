# Analysis after data processing

library(ggplot2)
library(randomForest)
library(tree)
library(glmnet)
library(MASS)
library(leaps)


# List of datasets:

# 1. dat.old <- this is the unprocessed dataset
# 2. dat <- this is the complete dataset (without NAN values)

# 3. kwh.dat <- kWh dataset
# 4. therms.dat <- Therms dataset
# 5. kwh.ms <- kwh dataset with month and season 
# 6. therms.ms <- kwh dataset with month and season 

# 7. num.kwh <- numeric dataset for kwh
# 8. sd.num.kwh <- standardized numeric dataset for kwh
# 9. pca.linearfit.kwh <- principal components and cat for kwh (used in linear model)
# 9. num.therms <- numeric dataset for therms
# 10. sd.num.therms <- standardized numeric dataset for therms
# 11. pca.linearfit.therms <- principal components and cat for therms (used in linear model)

####################################
# PCA and Linear Regression on kWh #
####################################

nums <- unlist(lapply(kwh.dat, is.numeric))
num.kwh <- kwh.dat[ , nums]
princomp.kwh <- princomp(num.kwh)
plot(princomp.kwh, type = "l", pch = 19, main = "Engergy Use (kWh) PCA Variance")
hist(log(1+princomp.kwh$scores[,1]))

# standardize the datase
sd.num.kwh <- num.kwh
for (col in 1:ncol(sd.num.kwh)){
  sd.num.kwh[,col] <- (sd.num.kwh[,col] - mean(sd.num.kwh[,col]))/sd(sd.num.kwh[,col])
}
princomp.kwh <- princomp(sd.num.kwh)
plot(princomp.kwh, type = "l", pch = 19, main = "Engergy Use (kWh) PCA Variance")
hist(log(1+princomp.kwh$scores[,1]))

prcomp.kwh <- prcomp(sd.num.kwh)
princomp.kwh$loadings # Shows that about 8 components explain 80% of the data
prcomp.kwh$rotation # Get PC's from this set

# the linear model
pca.linearfit.kwh <- data.frame("PC1" = prcomp.kwh$x[,1], 
                            "PC2" = prcomp.kwh$x[,2], 
                            "PC3" = prcomp.kwh$x[,3], 
                            kwh.dat[, (nums == F)], "KWH.MEAN.2010" = kwh.dat$KWH.MEAN.2010)
# pca.linearfit.kwh <- pca.linearfit.kwh[,-which(colnames(pca.linearfit.kwh) == "COMMUNITY.AREA.NAME")]
# pca.linearfit.kwh <- pca.linearfit.kwh[,-which(colnames(pca.linearfit.kwh) == "ELECTRICITY.ACCOUNTS")]

pca.fit.kwh <- lm(KWH.MEAN.2010 ~., dat = pca.linearfit.kwh)
summary(pca.fit.kwh)

#######################################
# PCA and Linear Regression on Therms #
#######################################

therms.dat <- therms.dat[,-which(colnames(therms.dat) == "ZERO.KWH.ACCOUNTS")]
nums <- unlist(lapply(therms.dat, is.numeric))
num.therms <- therms.dat[ , nums]
princomp.therms <- princomp(num.therms)
plot(princomp.therms, type = "l", pch = 19, main = "Engergy Use (Therms) PCA Variance")
hist(log(1+princomp.therms$scores[,1]))

# standardize the datase
sd.num.therms <- num.therms
for (col in 1:ncol(sd.num.therms)){
  sd.num.therms[,col] <- (sd.num.therms[,col] - mean(sd.num.therms[,col]))/sd(sd.num.therms[,col])
}
princomp.therms <- princomp(sd.num.therms)
plot(princomp.therms, type = "l", pch = 19, main = "Engergy Use (Therms) PCA Variance")
hist(log(1+princomp.therms$scores[,1]))

prcomp.therms <- prcomp(sd.num.therms)
princomp.therms$loadings # Shows that about 8 components explain 80% of the data
prcomp.therms$rotation # Get PC's from this set

# the linear model
pca.linearfit.therms <- data.frame("PC1" = prcomp.therms$x[,1], 
                                "PC2" = prcomp.therms$x[,2], 
                                "PC3" = prcomp.therms$x[,3], 
                                therms.dat[, (nums == F)], "THERM.MEAN.2010" = therms.dat$THERM.MEAN.2010)
# pca.linearfit.kwh <- pca.linearfit.kwh[,-which(colnames(pca.linearfit.kwh) == "COMMUNITY.AREA.NAME")]
# pca.linearfit.kwh <- pca.linearfit.kwh[,-which(colnames(pca.linearfit.kwh) == "ELECTRICITY.ACCOUNTS")]

pca.fit.therms <- lm(THERM.MEAN.2010 ~., dat = pca.linearfit.therms)
summary(pca.fit.therms)

################################
# Lasso on kWh - All Variables #
################################

kwh.ms <- kwh.ms[,-which(colnames(kwh.ms) == "KWH.MEAN.2010")]
set.seed(3)
lasso_kwh <- cv.glmnet(data.matrix(kwh.ms[,-which(colnames(kwh.ms) == "KWH.USE")]), kwh.ms$KWH.USE, nfolds = 10)
coef(lasso_kwh, s = "lambda.min")
par(mfrow = c(1, 2))
plot(lasso_kwh)
plot(lasso_kwh$glmnet.fit, "lambda")
#lambda is being tuned.
min(lasso_kwh$lambda)

###################################
# Lasso on Therms - All Variables #
###################################

therms.ms <- therms.ms[,-which(colnames(therms.ms) == "THERM.MEAN.2010")]
set.seed(3)
lasso_therms <- cv.glmnet(data.matrix(therms.ms[,-which(colnames(therms.ms) == "THERM.USE")]), 
                          therms.ms$THERM.USE, nfolds = 10)
coef(lasso_therms, s = "lambda.1se") # lambda 1se works better 
par(mfrow = c(1, 2))
plot(lasso_therms)
plot(lasso_therms$glmnet.fit, "lambda")
#lambda is being tuned.
min(lasso_therms$lambda)

########################################
# Linear Regression and AIC/BIC on kWh #
########################################

#kwh.ms <- kwh.ms[,-which(colnames(kwh.ms) == "season")]
kwh.ms <- kwh.ms[,-which(colnames(kwh.ms) == "OCCUPIED.HOUSING.UNITS")]
kwh.lmfit <- lm(KWH.MEAN.2010 ~., data = kwh.dat)
summary(kwh.lmfit)
n <- nrow(kwh.dat)

step(kwh.lmfit, direction="both", k=log(n), trace = 0)

# RSSleaps <- regsubsets(as.matrix(kwh.dat[,-5]), kwh.dat[,5])
# summary(RSSleaps, matrix=T)

###########################################
# Linear Regression and AIC/BIC on Therms #
###########################################

therms.ms <- therms.ms[,-which(colnames(therms.ms) == "OCCUPIED.HOUSING.UNITS")]
kwh.lmfit <- lm(THERM.MEAN.2010 ~., data = therms.dat)
summary(therms.lmfit)
n <- nrow(therms.dat)

step(kwh.lmfit, direction="both", k=log(n), trace = 0)

#####################
# Splines on kWh #
#####################

levels(kwh.ms$months) <- c('1', '2', '3', '4','5', '6', '7', '8', '9', '10', '11', '12')
kwh.ms$months = as.numeric(kwh.ms$months)
splineDesign(4, kwh.ms$months, outer.ok = TRUE)
bs.kwh = lm(KWH.USE ~ bs(months, df=3, knots = 3), data=kwh.ms) # 4-6 different knots
plot(kwh.ms$months, bs.kwh$fitted.values) # mean per month

#####################
# Random Forest kWh #
#####################

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
                                ntree = 1000, mtry = 6, nodesize = 1, importance = TRUE)
rf.kwh.tune
# plot(rf.kwh.tune)
varImpPlot(rf.kwh.tune)

########################
# Random Forest therms #
########################

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












