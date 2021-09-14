# Sample script for experiment 2
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load necessary packages and functions
library(ROCR)
library(hmeasure)
library(Ckmeans.1d.dp)
library(mgcv)
library(mltools)
library(data.table)
library(xgboost)
library(ggplot2)
library(rworldmap)

source("WOE2_code.R")

# load the data
load("creditCardTransactions.rdata")

data <- rbind(dataTrain, dataTest)
trainInds <- 1:nrow(dataTrain)
testInds  <- nrow(dataTrain) + 1:nrow(dataTest)

# We work in 2 steps:
# 1. fit gam and use this to optimize number of clusters for 
# categorical vairables
# 2. discretize the gam effects


############
## Step 1 ##
############

# fit a gam model, and use this to optimize the number of 
# clusters for the categorical variables
# Note that we only include the time variable as a spline here,
# as the other two continuous variables display a (close to) linear relation
# The functin fac2WOE converts the categorical variables into WOE values,
# shrunk WOE values and clustered WOE values. The clustering is determinde
# based on the tuning parameter lambda

set.seed(123)
lambdaseq  <- c(0, exp(-c(10:0)))
result     <- array(0, dim = c(length(lambdaseq), 3))
result_oos <- array(0, dim = c(length(lambdaseq), 6))

for (i in 1:length(lambdaseq)) {
  fac2woe.out <- fac2WOE(data = data,
                         yind = 1,
                         trainInds = trainInds,
                         testInds = testInds,
                         weightType = "varprop", 
                         offSet = 0.01,
                         lambda = lambdaseq[i])
  
  datat_woe  <- fac2woe.out$data_woe
  datat_swoe <- fac2woe.out$data_swoe
  datat_cwoe <- fac2woe.out$data_cwoe
  
  gam.out_woe <- mgcv::gam(isFraud ~  category + country +
                         age +
                         amount+
                         s(time, k = 8, bs="cc"),
                       data = datat_woe[trainInds, ],
                       family = "binomial")
  gam.out_swoe <- mgcv::gam(isFraud ~  category + country +
                             age +
                             amount+
                             s(time, k = 8, bs="cc"),
                           data = datat_swoe[trainInds, ],
                           family = "binomial")
  gam.out_cwoe <- mgcv::gam(isFraud ~  category +
                              country +
                             age +
                             amount+
                             s(time, k = 8, bs="cc"),
                           data = datat_cwoe[trainInds, ],
                           family = "binomial")
  
  result[i, ] <- c(gam.out_woe$aic, 
                   gam.out_swoe$aic,
                   gam.out_cwoe$aic)
  print(i)
}

# Now extract the optimal tuning parameter for the categorical variables:
which.min(result[, 3])# 5
lambda_cat <- lambdaseq[which.min(result[, 3])]; lambda_cat # 0.000911882




############
## Step 2 ##
############

# Now that we have the lambda for the categorical variables, we can start
# discretizing the splines.


# first get the data with the optimal value of lambda_cat:
fac2woe.out <- fac2WOE(data = data,
                       yind = 1,
                       trainInds = trainInds,
                       testInds = testInds,
                       weightType = "varprop", 
                       offSet = 0.01,
                       lambda = lambda_cat)

datat_woe  <- fac2woe.out$data_woe
datat_swoe <- fac2woe.out$data_swoe
datat_cwoe <- fac2woe.out$data_cwoe

# Now fit the gam models using this data.
# We obtain three datasets (WOE, shrunk WOE and clustered WOE):
gam.out_woe <- mgcv::gam(isFraud ~  category + country + age + amount +
                           s(time, k = 8, bs="cc"),
                         data = datat_woe[trainInds, ],
                         family = "binomial")
gam.out_swoe <- mgcv::gam(isFraud ~  category + country + age + amount +
                            s(time, k = 8, bs="cc"),
                          data = datat_swoe[trainInds, ],
                          family = "binomial")
gam.out_cwoe <- mgcv::gam(isFraud ~  category + country +
                            age +
                            amount +
                            s(time, k = 8, bs="cc"),
                          data = datat_cwoe[trainInds, ],
                          family = "binomial")

# Plot the spline function of the time variable for each of these:
plot(gam.out_woe)
plot(gam.out_swoe)
plot(gam.out_cwoe)


# Let's fit the data using the shrunk WOE values.
# The other results can be obtained by replacing "swoe" by "cwoe" or "woe"
gam.out <- gam.out_swoe
dataSel <- datat_swoe

# Now specify which variables need to be discretized in spline functions.
# "resp": response
# lin: linear effect --> no discretization
# cat: categorical variable
# "UC": unconstrained binning of spline function
# "C" constrained binning of spline function


varTypes    <- c("resp", "lin", "lin", "cat", "cat", "UC")

# maximum number of bins:
kmax <- 10


# Now bin the variables into k = 2, ..., kmax clusters
binVars.out <- binVars(dataSel, gam.out, trainInds, testInds, varTypes, kmax)


# The output above contains kmax-1 partitions of the spline effect.
# in order to select the best one, we evaluate the criterion
# WCSS + lambda * k for a range of lambda values

# Note that here, since we have only one variable which is binned
# we could also tune directly on k, instead of using the criterion above


lambdaseq_UC  <- exp(seq(0, 4, by = 0.5))
lambdaseq_C   <- 1

nbUCvar <- length(binVars.out$UCvar)
nbCvar  <- 0
result <- array(Inf, dim = c(length(lambdaseq_UC), length(lambdaseq_C)))

for (i in 1:length(lambdaseq_C)) {
  for (j in 1:length(lambdaseq_UC)) {

    newData       <- dataSel[trainInds, ]
    
    if (nbUCvar > 0) {
      for (k in 1:nbUCvar) {
        splitnb <- which.min(binVars.out$UC_WCSS[k, ] + binVars.out$kseq * lambdaseq_UC[j])
        varNb   <-  binVars.out$UCvar[k]
        newData[, varNb] <- binVars.out$UC_cl[k, ,splitnb]
      }
    }
    if (nbCvar > 0) {
      for (k in 1:nbCvar) {
        splitnb <- which.min(binVars.out$C_WCSS[k, ] + binVars.out$kseq * lambdaseq_C[j])
        varNb   <-  binVars.out$Cvar[k]
        newData[, varNb] <- binVars.out$C_cl[k, ,splitnb]
      }
    }
    glm.out   <- glm(isFraud~., data = newData, family = "binomial")
    result[j, i] <- glm.out$aic
  }
}
best.params <- which(result == min(result), arr.ind = TRUE); best.params # 9

# the optimal lambda is: 
lambda_opt_UC <- lambdaseq_UC[9]; lambda_opt_UC # 54.59
lambda_opt_C <- 0

# this corresponds with 3 bins:
1 + which.min(drop(binVars.out$UC_WCSS[1, ]) + binVars.out$kseq * lambda_opt_UC) # 3



# Now construct the final dataset with the optimally binned effects:
newData      <- dataSel[trainInds, ]
newData_test <- dataSel[testInds, ]

if (nbUCvar > 0) {
  for (k in 1:nbUCvar) {
    splitnb <- which.min(binVars.out$UC_WCSS[k, ] + binVars.out$kseq * lambda_opt_UC)
    varNb   <-  binVars.out$UCvar[k]
    newData[, varNb] <- binVars.out$UC_cl[k, ,splitnb]
    newData_test[, varNb] <- binVars.out$UC_cl_test[k, ,splitnb]
  }
}

if (nbCvar > 0) {
  for (k in 1:nbCvar) {
    splitnb <- which.min(binVars.out$C_WCSS[k, ] + binVars.out$kseq * lambda_opt_C)
    varNb   <-  binVars.out$Cvar[k]
    newData[, varNb] <- binVars.out$C_cl[k, ,splitnb]
    newData_test[, varNb] <- binVars.out$C_cl_test[k, ,splitnb]
  }
}



# Now we can fit the final glm:
glm.out      <- glm(isFraud~., data = newData, family = "binomial")

# coefficients:
tab.out <- summary(glm.out)$coefficients[, c(1, 4)];xtable::xtable(tab.out)

# performance evaluation:
evalglm(glm.out, newData_test, newData_test$isFraud)
#     AIC       AUC    wbrier H-measure 
# 212.184     0.943     0.336     0.652 

# Note that this AIC is overoptimistic. We should add the degrees of freedom
# resulting from the spline fits.
# Note that now, each of these count for 1 DF. We consider them as
# categorical, in which case they would together have 3 df.
# so need another 2:
2*2 + 212.185 # 216.185


# as a first benchmark, we can compute the glm without splines:
glm.out      <- glm(isFraud~., data = dataSel[trainInds, ], family = "binomial")
glm.out$aic
evalglm(glm.out, dataSel[testInds, ], dataSel$isFraud[testInds])
# AIC       AUC    wbrier H-measure 
# 226.179     0.928     0.354     0.615 




# as a second benchmark, we consider xgboost:

# one hot encoding:
newdata_train <- as.matrix(one_hot(as.data.table(dataTrain[, -1])))
newdata_test  <-  as.matrix(one_hot(as.data.table(dataTest[, -1])))
trainData <-  xgb.DMatrix(as.matrix(newdata_train), label = as.numeric(dataTrain$isFraud) - 1 )
testData  <- xgb.DMatrix(as.matrix(newdata_test), label = as.numeric(dataTest$isFraud) - 1 )
xgb.out   <- xgboost(data = trainData,objective = "reg:logistic", nrounds = 1000)

preds.out <- predict(xgb.out, newdata_test)

result <- rep(0, 3)
names(result) <- c("auc", "wbrier", "H-measure")
y_test <- as.numeric(dataTest$isFraud) - 1
pred <- ROCR::prediction(as.vector(preds.out), as.factor(y_test));
result[1] <- as.numeric(ROCR::performance(pred,"auc")@y.values)
wts       <- (1 / (table(y_test) / sum(y_test)))[y_test + 1]
result[2] <- weighted.mean((preds.out - y_test)^2,w = wts)
result[3] <- hmeasure::HMeasure(true.class = y_test, scores = as.vector(preds.out))$metrics$H
round(result, 3)
# auc    wbrier H-measure 
# 0.905     0.364     0.611





################################################################################
################################################################################

#############
## Figures ##
#############

cx = 2
cx.lab =3
cx.axis = 3
lwd = 4

gam.out <- mgcv::gam(isFraud ~  category + country + age + s(amount) +
                            s(time, k = 8, bs="cc"),
                          data = datat_swoe[trainInds, ],
                          family = "binomial")

mgptemp <- c(4.5, 2, 0)


pdf("Fig6.pdf",width = 20, height = 10) # Figure 7 in paper
par(mar = c(8, 10, 1, 2), mfrow = c(1, 2))
plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     ylab = "f(amount)", cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "amount",
     cex.axis = cx.axis, select = 1, mgp = mgptemp)
plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     ylab = "f(time)", cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "time",
     cex.axis = cx.axis, select = 2, mgp = mgptemp)
dev.off()

par(mar = c(5.1, 4.1, 4.1, 2.1))

## Plots of the discretization phase.

# Time effect
kmeans.out <- (binVars.out$UC_kms[[1]])[[2]]
prec <- 0.01
timevals   <- seq(0, 24, by = prec)
clockdf    <- data.frame(time = timevals)
clockdf    <- cbind(matrix(1, length(timevals), 4), clockdf)
colnames(clockdf) <- colnames(dataSel)[-1]
splineVals <- predict(gam.out, newdata = clockdf, type= "terms")
plot(timevals, splineVals[, 5])
clockdf$ftime <- splineVals[, 5]
binCols <- sapply(splineVals[, 5],
                  function(y) which.min(abs(y - kmeans.out$centers)))
binVals <- kmeans.out$centers[binCols]
clockdf$binCols <- binCols
clockdf$binVals <- binVals
clockdf <- clockdf[, -c(1:4)]
cols <-  colorRampPalette(c("steelblue", "firebrick"))(3)


ggclock <- ggplot(clockdf) +
  geom_bar(aes(x = time, y = binVals + 2, fill = as.factor(binCols)),
           stat = "identity", width = prec) +

  scale_fill_manual("risk level",
                    values = cols) +
  geom_line(aes(x = time, y = ftime + 2), size = 1) +
  coord_polar() +
  theme_minimal() +
  scale_x_continuous("", breaks = 0:24, limits = c(0, 24)) + ylim(0, 3.8) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+ theme(
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 25)
        );ggclock

ggsave("Fig7.pdf") # Figure 8 in paper

## Country 


levelstemp <- sort(unique(datat_cwoe$country))
clustID    <- sapply(datat_cwoe$country, function(z) which(levelstemp == z))
clusteredCountries <- clustID
names(clusteredCountries) <- as.character(data$country)
clusteredCountries[sapply(levels(data$country), function(y)
  which(names(clusteredCountries) == y)[1])]

tempc <- list()
tempc$cluster <- clusteredCountries

cols <- colorRamp(c("blue", "red"))((levelstemp - min(levelstemp)) / diff(range(levelstemp)))
cols <- apply(cols, 1, function(z) rgb(z[1]/255, z[2]/255, z[3]/255))
cnames <- tempc$cluster
cnames <- as.factor(tempc$cluster)
levels(cnames) <- 1:12
df.worldmap <- data.frame(names(clusteredCountries))
df.worldmap$risk <- cnames
colnames(df.worldmap) <- c("country", "risk")

wMap <- joinCountryData2Map(df.worldmap, joinCode = "ISO3",
                            nameJoinColumn = "country")

mapCountryData(wMap, catMethod = "categorical", nameColumnToPlot = "risk",
               missingCountryCol = gray(.8), colourPalette = cols,mapTitle = "",
               addLegend = FALSE)

pdf(file = "Fig5.pdf", height = 6, width = 12) # Figure 6 in paper
par(mar  = c(0, 1, 0, 0))
mapCountryData(wMap, catMethod = "categorical", nameColumnToPlot = "risk",
               missingCountryCol = gray(.8), colourPalette = cols,mapTitle = "",
               addLegend = FALSE)
dev.off()


