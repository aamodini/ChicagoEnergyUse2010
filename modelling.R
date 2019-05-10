
# PCA Linear Regression -------------------------------------------------------

# kWh

nums <- unlist(lapply(kwh.dat, is.numeric))
pca.linearfit.kwh <- data.frame("PC1" = prcomp.kwh$x[,1],"PC2" = prcomp.kwh$x[,2], 
                                "PC3" = prcomp.kwh$x[,3], kwh.dat[, (nums == F)], 
                                "KWH.MEAN.2010" = kwh.dat$KWH.MEAN.2010)
pca.fit.kwh <- lm(KWH.MEAN.2010 ~., dat = pca.linearfit.kwh)

res.kwh <- resid(pca.fit.kwh)
means.kwn <- kwh.dat$KWH.MEAN.2010[-which.min(res.kwh)]
res.kwh <- res.kwh[-which.min(res.kwh)]
summary.kwh <- summary(pca.fit.kwh)

par(mfrow=c(1,2))
plot(means.kwn, res.kwh, main = "Residual Plot",
     ylab = "Residuals", xlab = "kWh Mean", cex = 1)
abline(0,0)
qqnorm(res.kwh, cex = 1) # shows a heavy tailed distribution
qqline(res.kwh) 

summary.kwh$coefficients[1:5,]
summary.kwh$r.squared
mean(pca.fit.kwh$residuals^2)

# therm

nums <- unlist(lapply(therms.dat, is.numeric))
pca.linearfit.therms <- data.frame("PC1" = prcomp.therms$x[,1], 
                                   "PC2" = prcomp.therms$x[,2], 
                                   "PC3" = prcomp.therms$x[,3], 
                                   therms.dat[, (nums == F)], 
                                   "THERM.MEAN.2010" = therms.dat$THERM.MEAN.2010)

pca.fit.therms <- lm(THERM.MEAN.2010 ~., dat = pca.linearfit.therms)

# Plots to check linearity
res.therms <- resid(pca.fit.therms)
means.therms <- therms.dat$THERM.MEAN.2010[-which.min(res.therms)]
res.therms <- res.therms[-which.min(res.therms)]
summary.therms <- summary(pca.fit.therms)

# save space
par(mfrow=c(1,2))
plot(means.therms, res.therms, main = "Residual Plot",
     ylab = "Residuals", xlab = "Therms Mean", cex = 1)
abline(0,0)
qqnorm(res.therms, cex = 1) # shows a heavy tailed distribution
qqline(res.therms)

summary.therms$coefficients[1:5,]
summary.therms$r.squared
mean(pca.fit.therms$residuals^2)


# Cubic Splines -----------------------------------------------------------

# kwh

levels(kwh.ms$months) <- c('1', '2', '3', '4','5', '6', '7', '8', '9', '10', '11', '12')
kwh.ms$months <- as.numeric(kwh.ms$months)

bs.kwh <- lm(KWH.USE ~ .*bs(months, df=3, knots = 3), data=kwh.ms) 
summary.splines.kwh <- summary(bs.kwh)

summary.splines.kwh$coefficients[1:4,]
summary.splines.kwh$r.squared
mean(bs.kwh$residuals^2)

fitted.kwh <- predict(bs.kwh)

kwh.mean.hat <- data.frame("KWH.MEAN.2010" = kwh.dat$KWH.MEAN.2010, "FITTED.MEAN" = 0)
for (obs in 1:nrow(therms.dat)){
  kwh.mean.hat[obs,2] <- sum(sapply(seq(0,11), 
                                    function(x) fitted.kwh[(obs + x*nrow(kwh.dat))]))/12 
}

mean((kwh.mean.hat$KWH.MEAN.2010 - kwh.mean.hat$FITTED.MEAN)^2)
set.seed(3)
elasticnet_kwh <- cv.glmnet(data.matrix(kwh.ms[,-which(colnames(kwh.ms) == "KWH.USE")]), 
                            kwh.ms$KWH.USE, nfolds = 10, alpha = .5)

coef <- as.matrix(coef(elasticnet_kwh, s = "lambda.1se"))
par(mfrow = c(1, 2))
plot(elasticnet_kwh)
plot(elasticnet_kwh$glmnet.fit, "lambda")
min(elasticnet_kwh$lambda)

bs.kwh <- lm(KWH.USE~(.- TOTAL.UNITS)*bs(months, df=3, knots = 5), data=kwh.ms) 
summary.splines.kwh <- summary(bs.kwh)

summary.splines.kwh$coefficients[1:3,]
summary.splines.kwh$r.squared
mean(bs.kwh$residuals^2)

fitted.kwh <- predict(bs.kwh)

kwh.mean.hat <- data.frame("KWH.MEAN.2010" = kwh.dat$KWH.MEAN.2010, "FITTED.MEAN" = 0)
for (obs in 1:nrow(therms.dat)){
  kwh.mean.hat[obs,2] <- sum(sapply(seq(0,11), function(x) fitted.kwh[(obs + x*nrow(kwh.dat))]))/12 
}

mean((kwh.mean.hat$KWH.MEAN.2010 - kwh.mean.hat$FITTED.MEAN)^2)

# therm

levels(therms.ms$months) <- c('1', '2', '3', '4','5', '6', '7', '8', '9', '10', '11', '12')
therms.ms$months <- as.numeric(therms.ms$months) 

bs.therms <- lm(THERM.USE ~ .*bs(months, df=3, knots = 3), data=therms.ms)

summary.splines.therms <- summary(bs.therms)

summary.splines.therms$r.squared
mean(bs.therms$residuals^2)

fitted.therms <- predict(bs.therms)

therms.mean.hat <- data.frame("THERM.MEAN.2010" = therms.dat$THERM.MEAN.2010, "FITTED.MEAN" = 0)
for (obs in 1:nrow(therms.dat)){
  therms.mean.hat[obs,2] <- sum(sapply(seq(0,11), function(x) fitted.therms[(obs + x*nrow(therms.dat))]))/12 
}

mean((therms.mean.hat$THERM.MEAN.2010 - therms.mean.hat$FITTED.MEAN)^2)

set.seed(3)
elasticnet_therms <- cv.glmnet(data.matrix(therms.ms[,-which(colnames(therms.ms) == "THERM.USE")]), 
                               therms.ms$THERM.USE, nfolds = 10, alpha = .5)
coef <- as.matrix(coef(elasticnet_therms, s = "lambda.1se"))

min(elasticnet_therms$lambda)

bs.therms <- lm(THERM.USE~(.-OCCUPIED.UNITS - OCCUPIED.HOUSING.UNITS 
                           - BUILDING_SUBTYPE - TOTAL.UNITS - AVERAGE.HOUSESIZE 
                           - RENTER.OCCUPIED.HOUSING.UNITS)*bs(months, df=3, knots = 5), 
                data=therms.ms)

summary.splines.therms <- summary(bs.therms)

summary.splines.therms$r.squared
mean(bs.therms$residuals^2)

fitted.therms <- predict(bs.therms)

therms.mean.hat <- data.frame("THERM.MEAN.2010" = therms.dat$THERM.MEAN.2010, "FITTED.MEAN" = 0)
for (obs in 1:nrow(therms.dat)){
  therms.mean.hat[obs,2] <- sum(sapply(seq(0,11), function(x) fitted.therms[(obs + x*nrow(therms.dat))]))/12 
}

mean((therms.mean.hat$THERM.MEAN.2010 - therms.mean.hat$FITTED.MEAN)^2)


# Random Forest -----------------------------------------------------------

# Random Forest kWh -------------------------------------------------------

# Note that eval is set to false because it takes a lot of time to run
# and is not needed for specific results - this model is tuned
# OOB error is about 14%
rf.kwh <- randomForest(num.kwh, kwh.dat$BUILDING_SUBTYPE, 
                       ntree = 1000, mtry = 1, nodesize = 20, sampsize = 500)

nodes <- c(1, 20, 40, 60) # picking some node values to test
mtrys <- c(1, 3, 6) # keeping the center value as the default 
ntrees <- c(200, 500, 1000)
best <- c(10,10,10, 10) # initating the best(error, mtry, nodesize)

for (ntree in ntrees){
  for (nodesize in nodes){
    for (mtry in mtrys){
      rf.fit <- randomForest(dat.numeric, kwh.dat$BUILDING_SUBTYPE, 
                             ntree = ntree, mtry = mtry, nodesize = nodesize)
      OOB.err <- mean(predict(rf.fit) != kwh.dat$BUILDING_SUBTYPE)
      # retain the information of only those predictors 
      # that have the lowest prediction error
      ifelse(OOB.err < best[1], best <- c(OOB.err, mtry, nodesize, ntree), best <- best)
      print(best)
    }
  }
}

rf.kwh.tune <- randomForest(num.kwh, kwh.dat$BUILDING_SUBTYPE, 
                            ntree = 1000, mtry = 6, nodesize = 1, importance = TRUE)

rf.kwh.tune$confusion

par(mfrow = c(1, 2))
dotchart(sort(rf.kwh.tune$importance[,5]), main = "Mean Decrease Accuracy", cex = .5)
dotchart(sort(rf.kwh.tune$importance[,6]), main = "Mean Decrease Gini", cex = .5)

# Random Forest Therms -------------------------------------------------------

rf.therms <- randomForest(num.therms, therms.dat$BUILDING_SUBTYPE, 
                          ntree = 1000, mtry = 1, nodesize = 20, sampsize = 500)

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

rf.therms.tune <- randomForest(num.therms, therms.dat$BUILDING_SUBTYPE, 
                               ntree = 500, mtry = 6, nodesize = 20, importance = TRUE)

rf.therms.tune$confusion

par(mfrow = c(1, 2))
dotchart(sort(rf.therms.tune$importance[,5]), main = "Mean Decrease Accuracy", cex = .5)
dotchart(sort(rf.therms.tune$importance[,6]), main = "Mean Decrease Gini", cex = .5)










