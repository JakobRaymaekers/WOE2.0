# experiment 1: all continuous variables on the credit card data

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load necessary packages and functions
library(ROCR)
library(hmeasure)
library(Ckmeans.1d.dp)
library(mgcv)
library(xgboost)
library(mltools)
library(data.table)
library(ggplot2)
library(grid)
library(gridBase)

source("WOE2_code.R")

# load the data
load("creditCardTransactions.rdata")

data <- rbind(dataTrain, dataTest)
trainInds <- 1:nrow(dataTrain)
testInds  <- nrow(dataTrain) + 1:nrow(dataTest)

# select only the continuous variables
data      <- data[, c(1, 2, 3, 6)]
dataTrain <- dataTrain[, c(1, 2, 3, 6)]
dataTest  <- dataTest[, c(1, 2, 3, 6)]


# Explore the data and visualize
cx = 2
cx.lab = 5
cx.axis = 5
lwd = 4
mars = c(5.1, 5.1, 1.1, 2.1)

mgptemp <- c(1, 3, 1)
# Marginal distributions (Figure 1 in paper)

pdf("Fig1.pdf", width = 30, height =10)
par(mar = c(8, 10, 2,3), mfrow = c(1, 3))
hist(data$age, main="", cex.lab = cx.lab,
     cex.axis = cx.axis,
     xlab = "", xaxt = "n",
     xlim = c(20, 70), ylim = c(0, 2000),
     ylab = "")
axis(side = 1, at = seq(20, 70, by = 10),
     labels = seq(20, 70, by = 10),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "age", cex.lab = cx.lab, line = 5)

hist(exp(data$amount), main="",
     cex.lab = cx.lab, cex.axis = cx.axis,
     xlab = "", xaxt = "n", ylim = c(0, 7000),
     ylab = "")
axis(side = 1, at = seq(0, 3000, by = 500),
     labels = seq(0, 3000, by = 500),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "amount", cex.lab = cx.lab, line = 5)

hist(data$time, main="", cex.lab = cx.lab,
     cex.axis = cx.axis, xlim = c(0, 24),
     xlab = "", xaxt = "n", yaxt = "n", ylim = c(0, 1000),
     ylab = "")
axis(side = 1, at = seq(0, 24, by = 6),
     labels = seq(0, 24, by= 6),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "time", cex.lab = cx.lab, line = 5)

axis(side = 2, at = seq(0, 1000, by = 200),
     labels =seq(0, 1000, by = 200),
     cex.axis = cx.axis, cex.lab = cx.lab)
dev.off()


# Now we fit a classical GAM and plot the resulting splines (Fig 3 in paper)
gam.out <- mgcv::gam(isFraud ~  s(age, k = 10) +
                       s(amount, k = 10) +
                       s(time, k = 10, bs="cc"),
                     data = data,
                     family = "binomial")


mgptemp <- c(6, 2.5, 0)

pdf("Fig2.pdf",width = 30, height = 10)
par(mar = c(8, 12, 2, 3), mfrow = c(1, 3))
plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "age", ylab = "",
     cex.axis =cx.axis, select = 1, ylim = c(-14, 7),
     mgp = mgptemp)
title(ylab = "f(age)", cex.lab = cx.lab, line = 7)

plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     ylab = "f(amount)", cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "amount",
     cex.axis = cx.axis, select = 2, ylim = c(-8, 6),
     mgp = mgptemp)

plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     ylab = "f(time)", cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "time",
     cex.axis = cx.axis,
     select =3, ylim = c(-2.5, 2.5),
     mgp = mgptemp)
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))


# Now we fit a GAM in which the age variable enters as a linear effect
gam.out <- mgcv::gam(isFraud ~  
                       age +
                       s(amount, k = 20)+
                       s(time, k = 8, bs="cc"),
                     data = dataTrain,
                     family = "binomial")

evalglm(gam.out, dataTest, y_test = dataTest$isFraud)
#     AIC       AUC    wbrier H-measure 
# 284.495     0.919     0.407     0.604 

cx.lab <- 3
cx.axis <- 3

mgptemp <- c(4.5, 2, 0)


pdf("Fig3.pdf",width = 20, height = 10)
par(mar = c(8, 10, 1, 3), mfrow = c(1, 2))
plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     ylab = "f(amount)", cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "amount",
     cex.axis = cx.axis, select = 1, ylim = c(-2, 5),
     mgp = mgptemp)
plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, scheme=1, shade=T,shade.col='lightblue',
     ylab = "f(time)", cex.lab = cx.lab, col = "darkblue", lwd = lwd,
     xlab = "time",
     cex.axis = cx.axis, select =2, ylim = c(-2.5, 3),
     mgp = mgptemp)
dev.off()


# Now specify which variables need to be discretized in spline functions.
# "resp": response
# lin: linear effect --> no discretization
# cat: categorical variable
# "UC": unconstrained binning of spline function
# "C" constrained binning of spline function

varTypes <- c("resp", "C", "lin", "UC")

# set maximum number of bins, and execute the binVars function:
kmax        <- 10
binVars.out <- binVars(data, gam.out, trainInds, testInds, varTypes, kmax)




# Since we have only 1 constrained and 1 unconstrained variable here
# we can tune with k directly as it is equivalent to using the AIC criterion

result <- array(Inf, dim =  c(kmax, kmax))
for (i in 2:kmax) {
  bestSplits_C <- i
  for (j in 2:kmax) {
    bestSplits_UC <- j
    newData <- dataTrain
    for (k in 1:length(binVars.out$UCvar)) {
      varNb <- binVars.out$UCvar[k]
      newData[, varNb] <- binVars.out$UC_cl[k, ,bestSplits_UC[k]-1]
    }
    for (k in 1:length(binVars.out$Cvar)) {
      varNb <- binVars.out$Cvar[k]
      newData[, varNb] <- binVars.out$C_cl[k, ,bestSplits_C[k]-1]
    }
    
    glm.out      <- glm(isFraud~., data = newData, family = "binomial")
    result[i, j] <- glm.out$aic
  }
}

best.params <- which(result == min(result), arr.ind = TRUE); best.params
bestSplits_C <- best.params[1] - 1
bestSplits_UC <- best.params[2] - 1


# Build the new data in which the continuous effects have been
# converted into binned splines:

newData <- dataTrain
newData_test <- dataTest
for (k in 1:length(binVars.out$UCvar)) {
  varNb <- binVars.out$UCvar[k]
  newData[, varNb] <- binVars.out$UC_cl[k, ,bestSplits_UC[k]]
  newData_test[, varNb] <- binVars.out$UC_cl_test[k, ,bestSplits_UC[k]]
}
for (k in 1:length(binVars.out$Cvar)) {
  varNb <- binVars.out$Cvar[k]
  newData[, varNb] <- binVars.out$C_cl[k, ,bestSplits_C[k]]
  newData_test[, varNb] <- binVars.out$C_cl_test[k, ,bestSplits_C[k]]
}

# Finally, evaluate the GLM with the binned splines

glm.out      <- glm(isFraud~., data = newData, family = "binomial")
glm.out$aic
evalglm(glm.out, newData_test, newData_test$isFraud)
#     AIC       AUC    wbrier H-measure 
# 268.862     0.925     0.397     0.626

# Note that this AIC is overoptimistic. We should add the degrees of freedom
# resulting from the spline fits.
# Note that now, each of these count for 1 DF. We consider them as
# categorical, in which case they would together have 7+6 = 11 df.
# so need another 9:
# 2*9 + 268.862 = 286.862



################################################################################
# Now compare the performance to the benchmarks


# Classical GLM
glm.out      <- glm(isFraud~., data = dataTrain, family = "binomial")
evalglm(glm.out, dataTest, dataTest$isFraud)
# AIC       AUC    wbrier H-measure 
# 293.656     0.896     0.438     0.549 

# Classical GAM
evalglm(gam.out, dataTest, dataTest$isFraud)
#     AIC       AUC    wbrier H-measure 
# 284.495     0.919     0.407     0.604 


# XGBoost

trainData <-  xgb.DMatrix(as.matrix(dataTrain[, -1]), label = as.numeric(dataTrain$isFraud) - 1 )
testData  <- xgb.DMatrix(as.matrix(dataTest[, -1]), label = as.numeric(dataTest$isFraud) - 1 )
xgb.out   <- xgboost(data = trainData,objective = "reg:logistic", nrounds = 1000)
 
preds.out <- predict(xgb.out, as.matrix(dataTest[, -1]))

result <- rep(0, 3)
names(result) <- c("auc", "wbrier", "H-measure")
y_test <- as.numeric(dataTest$isFraud) - 1
pred <- ROCR::prediction(as.vector(preds.out), as.factor(y_test));
result[1] <- as.numeric(ROCR::performance(pred,"auc")@y.values)
wts       <- (1 / (table(y_test) / sum(y_test)))[y_test + 1]
result[2] <- weighted.mean((preds.out - y_test)^2,w = wts)
result[3] <- hmeasure::HMeasure(true.class = y_test, scores = as.vector(preds.out))$metrics$H
round(result, 3)
#   auc    wbrier H-measure 
# 0.891     0.363     0.567 
# 


################################################################################
# Now draw the plots of the discretization phase (Fig 5 in paper)


# Time effect
kmeans.out <- (binVars.out$UC_kms[[1]])[[bestSplits_UC]]
prec <- 0.01
timevals   <- seq(0, 24, by = prec)
clockdf    <- data.frame(time = timevals)
clockdf    <- cbind(1, cbind(1, clockdf))
colnames(clockdf) <- colnames(data)[-1]
splineVals <- predict(gam.out, newdata = clockdf, type= "terms")
plot(timevals, splineVals[, 3])
clockdf$ftime <- splineVals[, 3]
binCols <- sapply(splineVals[, 3],
                  function(y) which.min(abs(y - kmeans.out$centers)))
binVals <- kmeans.out$centers[binCols]
clockdf$binCols <- binCols
clockdf$binVals <- binVals
clockdf <- clockdf[, -c(1,2)]
cols <-  colorRampPalette(c("steelblue", "firebrick"))(6)


ggclock <- ggplot(clockdf) +
  geom_bar(aes(x = time, y = binVals + 2, fill = as.factor(binCols)),
           stat = "identity", width = prec) +
  
  scale_fill_manual("risk level",
                    values = cols) +
  geom_line(aes(x = time, y = ftime + 2), size = 1) +
  coord_polar() +
  theme_minimal() +
  scale_x_continuous("", breaks = 0:24, limits = c(0, 24)) + ylim(0, 3.8) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())+ theme(
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 25)
        )


# amount effect

kmeans.out <- (binVars.out$C_kms[[1]])[[bestSplits_C]]
mgptemp <- c(6, 2, 0)
cx.lab <- 2.5


pdf("Fig4.pdf",width = 20, height = 10)
plot.new()
par(mar = c(8, 5, 1, 3), mfrow = c(1, 2))
pushViewport(viewport(layout = grid.layout(1, 2)))
#Draw base plot
pushViewport(viewport(layout.pos.col = 1))
par(fig = gridFIG(), new = TRUE)
plot(gam.out, residuals=F, se=TRUE,pch=19,
     cex=cx, col = "black", rug = FALSE,
     ylab = "f(amount)", cex.lab = cx.lab,
     lwd = lwd,
     xlab = "amount",
     cex.axis =cx.axis, select = 1, mgp = mgptemp)
segments(x0 = c(0, kmeans.out$breakpoints), x1 = c(kmeans.out$breakpoints, 8),
         y0 = kmeans.out$origcenters_f, y1 = kmeans.out$origcenters_f,
         col = 1:7, lwd = 4)
popViewport()

#Draw ggplot
pushViewport(viewport(layout.pos.col = 2))
print(ggclock, newpage = FALSE)
popViewport()
dev.off()