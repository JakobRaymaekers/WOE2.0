# Sample script for PAKDD 2009 data
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load necessary packages and functions
library(mgcv)
library(xgboost)
library(ROCR)
library(hmeasure)
library(caret)
library(Ckmeans.1d.dp)
library(mltools)
library(data.table)
require(parallel)  
library(gridExtra)
library(grid)


source("WOE2_code.R")

# download and read the data
dataurl <- url("https://github.com/albahnsen/CostSensitiveClassification/raw/master/costcla/datasets/data/creditscoring2.csv.gz")
con <- gzcon(dataurl)
txt <- readLines(con)
data <- read.csv(textConnection(txt), sep = "\t")

# peprocess the data, leaving out constant variables and converting
# categorical ones to factors:

plot(apply(data, 2, function(y) length(unique(y))))

temp <- which(data$TARGET_LABEL_BAD.1 == "N")
datatemp <- data[temp, -21] 
data <- data[, -28]
colnames(datatemp) = colnames(data)
datatemp$QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION <- as.numeric(datatemp$QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION)
data[temp, ] <- datatemp

dataorig <- data

data$ID_SHOP <- as.factor(data$ID_SHOP)
data$SEX <- as.factor(data$SEX)
data$MARITAL_STATUS <- as.factor(data$MARITAL_STATUS)
data$FLAG_RESIDENCIAL_PHONE <- as.factor(data$FLAG_RESIDENCIAL_PHONE)
data$AREA_CODE_RESIDENCIAL_PHONE <- as.factor(data$AREA_CODE_RESIDENCIAL_PHONE)
data$SHOP_RANK <- as.factor(data$SHOP_RANK)
data$RESIDENCE_TYPE <- as.factor(data$RESIDENCE_TYPE)
data$FLAG_MOTHERS_NAME <- as.factor(data$FLAG_MOTHERS_NAME)
data$FLAG_FATHERS_NAME <- as.factor(data$FLAG_FATHERS_NAME)
data$FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN <- as.factor(data$FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN)
data$FLAG_RESIDENCE_STATE_eq_WORKING_STATE <- as.factor(data$FLAG_RESIDENCE_STATE_eq_WORKING_STATE)
data$PROFESSION_CODE <- as.factor(data$PROFESSION_CODE)
data$FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS <- as.factor(data$FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS )
data$FLAG_OTHER_CARD <- as.factor(data$FLAG_OTHER_CARD )
data$FLAG_MOBILE_PHONE <- as.factor(data$FLAG_MOBILE_PHONE)
data$FLAG_CONTACT_PHONE <- as.factor(data$FLAG_CONTACT_PHONE)
data$COD_APPLICATION_BOOTH <- as.factor(data$COD_APPLICATION_BOOTH)
data$FLAG_CARD_INSURANCE_OPTION <- as.factor(data$FLAG_CARD_INSURANCE_OPTION)
data$TARGET_LABEL_BAD.1 <- as.factor(data$TARGET_LABEL_BAD.1)

data$FLAG_OTHER_CARD <- NULL
data$FLAG_CONTACT_PHONE <- NULL
data$FLAG_MOBILE_PHONE <- NULL
data$QUANT_BANKING_ACCOUNTS <- NULL
data$PERSONAL_NET_INCOME <- as.numeric(data$PERSONAL_NET_INCOME)
data$FLAG_CARD_INSURANCE_OPTION <- NULL
data$COD_APPLICATION_BOOTH <- NULL

data$PERSONAL_NET_INCOME <-  cellWise::transfo(data$PERSONAL_NET_INCOME)$Xt
data$MATE_INCOME <- log(1+data$MATE_INCOME)

dim(data)

###############################################################################

# First explore and visualize the data

cx = 2
cx.lab = 5
cx.axis = 5
lwd = 4
mars = c(5.1, 5.1, 1.1, 2.1)
mgptemp <- c(1, 3, 1)
# Marginal distributions, Fig 2 in paper
pdf("Fig1_credit.pdf", width = 40, height =80/3)
par(mar = c(8, 10, 6,3), mfrow = c(2, 3))
hist(data$AGE, main="", cex.lab = cx.lab,
     cex.axis = cx.axis,
     xlab = "", xaxt = "n",
     ylab = "", probability = TRUE)
axis(side = 1, at = seq(0, 100, by = 10),
     labels =  seq(0, 100, by = 10),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "age", cex.lab = cx.lab, line = 5)

hist(data$PAYMENT_DAY, main="",
     cex.lab = cx.lab, cex.axis = cx.axis,
     xlab = "", xaxt = "n",
     ylab = "", probability = TRUE)
axis(side = 1, at = seq(1, 30, by = 1),
     labels = seq(1, 30, by = 1),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "payment day", cex.lab = cx.lab, line = 5)

hist(data$MONTHS_IN_RESIDENCE, main="", cex.lab = cx.lab,
     cex.axis = cx.axis, xlim = c(0, 1200),
     xlab = "", xaxt = "n", 
     ylab = "", probability = TRUE)
axis(side = 1, at = seq(0, 1200, by = 50),
     labels =  seq(0, 1200, by = 50),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "months in residence", cex.lab = cx.lab, line = 5)


hist(data$MONTHS_IN_THE_JOB, main="", cex.lab = cx.lab,
     cex.axis = cx.axis, xlim = c(0, 1200),
     xlab = "", xaxt = "n", 
     ylab = "", probability = TRUE)
axis(side = 1, at = seq(0, 1200, by = 50),
     labels =  seq(0, 1200, by = 50),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "months in job", cex.lab = cx.lab, line = 5)

hist(data$PERSONAL_NET_INCOME,
     main="", cex.lab = cx.lab,
     cex.axis = cx.axis, xlim = c(-4.5, 4.5),
     xlab = "", xaxt = "n", 
     ylab = "", probability = TRUE)
axis(side = 1, at = seq(-4,4 , by = 1),
     labels =   seq(-4,4 , by = 1),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "personal net income", cex.lab = cx.lab, line = 5)

hist(data$QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION,
     main="", cex.lab = cx.lab,
     cex.axis = cx.axis, xlim = c(0, 3),
     xlab = "", xaxt = "n", 
     ylab = "", probability = TRUE, breaks = c(0:3))

axis(side = 1, at = c(0, 0.5, 1.5, 2.5, 3),
     labels =   c("", 1:3, ""),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "quant additional cards", cex.lab = cx.lab, line = 5)

dev.off()



hist(data$MATE_INCOME, main="", cex.lab = cx.lab,
     cex.axis = cx.axis, xlim = c(0, 150000),
     xlab = "", xaxt = "n", 
     ylab = "")
axis(side = 1, at = seq(0, 150000, by = 10000),
     labels =  seq(0, 150000, by = 10000),
     cex.axis = cx.axis, cex.lab = cx.lab,
     mgp = mgptemp, line = -1)
title(xlab = "mate income", cex.lab = cx.lab, line = 5)





##############################################################################

# Set number of available threads. Higher means faster CV

cl <- makeCluster(10) 
Foldresults <- array(0, dim  = c(10, 10,4))
folds <- caret::createFolds(1:nrow(data), k = 10, list = TRUE, returnTrain = FALSE)



for (foldnb in 1:10) {
  trainInds <- (1:nrow(data))[-folds[[foldnb]]]
  testInds  <- folds[[foldnb]]
  
  # We work in 2 steps:
  # 1. fit gam and use this to optimize number of clusters for 
  # categorical vairables
  # 2. discretize the gam effects
  
  
  ############
  ## Step 1 ##
  ############
  
  # fit a gam model, and use this to optimize the number of 
  # clusters for the categorical variables
  # The function fac2WOE converts the categorical variables into WOE values,
  # shrunk WOE values and clustered WOE values. The clustering is determine
  # based on the tuning parameter lambda
  
  set.seed(123)
  lambdaseq  <- c(0, exp(-c(10:0)))
  result     <- array(0, dim = c(length(lambdaseq), 3))
  
  for (i in 1:length(lambdaseq)) {
    
    
    
    fac2woe.out <- fac2WOE(data = data,
                           yind = 21,
                           trainInds = trainInds,
                           testInds = testInds,
                           weightType = "varprop", 
                           offSet = 0.01,
                           lambda = lambdaseq[i])
    
    datat_woe  <- fac2woe.out$data_woe[trainInds, ]
    datat_swoe <- fac2woe.out$data_swoe[trainInds, ]
    datat_cwoe <- fac2woe.out$data_cwoe[trainInds, ]
    
    
    
    gam.out_woe <- mgcv::bam(TARGET_LABEL_BAD.1 ~ 
                               ID_SHOP +
                               SEX +
                               MARITAL_STATUS + 
                               s(AGE, k = 10) +
                               FLAG_RESIDENCIAL_PHONE +
                               AREA_CODE_RESIDENCIAL_PHONE + 
                               s(PAYMENT_DAY, k=8, bs = "cc") +
                               SHOP_RANK +
                               RESIDENCE_TYPE + 
                               s(MONTHS_IN_RESIDENCE, k = 10) +
                               FLAG_MOTHERS_NAME + 
                               FLAG_FATHERS_NAME + 
                               FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN +
                               FLAG_RESIDENCE_STATE_eq_WORKING_STATE + 
                               s(MONTHS_IN_THE_JOB, k = 10) +
                               PROFESSION_CODE+
                               s(MATE_INCOME ,k=10) + 
                               FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS  +
                               s(PERSONAL_NET_INCOME ,k=10) + 
                               QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION   ,
                             data = datat_woe,
                             family = "binomial", cluster = cl)
    
    gam.out_swoe <- mgcv::bam(TARGET_LABEL_BAD.1 ~ 
                                ID_SHOP +
                                SEX +
                                MARITAL_STATUS + 
                                s(AGE, k = 10) +
                                FLAG_RESIDENCIAL_PHONE +
                                AREA_CODE_RESIDENCIAL_PHONE + 
                                s(PAYMENT_DAY, k=8, bs = "cc") +
                                SHOP_RANK +
                                RESIDENCE_TYPE + 
                                s(MONTHS_IN_RESIDENCE, k = 10) +
                                FLAG_MOTHERS_NAME + 
                                FLAG_FATHERS_NAME + 
                                FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN +
                                FLAG_RESIDENCE_STATE_eq_WORKING_STATE + 
                                s(MONTHS_IN_THE_JOB, k = 10) +
                                PROFESSION_CODE+
                                s(MATE_INCOME ,k=10) + 
                                FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS  +
                                s(PERSONAL_NET_INCOME ,k=10) + 
                                QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION  ,
                              data = datat_swoe,
                              family = "binomial", cluster = cl)
    
    gam.out_cwoe <- mgcv::bam(TARGET_LABEL_BAD.1 ~ 
                                ID_SHOP +
                                SEX +
                                MARITAL_STATUS + 
                                s(AGE, k = 10) +
                                FLAG_RESIDENCIAL_PHONE +
                                AREA_CODE_RESIDENCIAL_PHONE + 
                                s(PAYMENT_DAY, k=8, bs = "cc") +
                                SHOP_RANK +
                                RESIDENCE_TYPE + 
                                s(MONTHS_IN_RESIDENCE, k = 10) +
                                FLAG_MOTHERS_NAME + 
                                FLAG_FATHERS_NAME + 
                                FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN +
                                FLAG_RESIDENCE_STATE_eq_WORKING_STATE + 
                                s(MONTHS_IN_THE_JOB, k = 10) +
                                PROFESSION_CODE+
                                s(MATE_INCOME ,k=10) + 
                                FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS  +
                                s(PERSONAL_NET_INCOME ,k=10) + 
                                QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION  ,
                              data = datat_cwoe,
                              family = "binomial", cluster = cl)
    
    result[i, ] <- c(gam.out_woe$aic, 
                     gam.out_swoe$aic,
                     gam.out_cwoe$aic)
    print(i)
  }
  
  # Now extract the optimal tuning parameter for the categorical variables:
  which.min(result[, 3])
  lambda_cat <- lambdaseq[which.min(result[, 3])]; lambda_cat 
  
  
  
  
  ############
  ## Step 2 ##
  ############
  
  # Now that we have the lambda for the categorical variables, we can start
  # discretizing the splines.
  
  
  # first get the data with the optimal value of lambda_cat:
  fac2woe.out <- fac2WOE(data = data,
                         yind = 21,
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
  
  gam.out_woe <- mgcv::bam(TARGET_LABEL_BAD.1 ~ 
                             ID_SHOP +
                             SEX +
                             MARITAL_STATUS + 
                             s(AGE, k = 10) +
                             FLAG_RESIDENCIAL_PHONE +
                             AREA_CODE_RESIDENCIAL_PHONE + 
                             s(PAYMENT_DAY, k=8, bs = "cc") +
                             SHOP_RANK +
                             RESIDENCE_TYPE + 
                             s(MONTHS_IN_RESIDENCE, k = 10) +
                             FLAG_MOTHERS_NAME + 
                             FLAG_FATHERS_NAME + 
                             FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN +
                             FLAG_RESIDENCE_STATE_eq_WORKING_STATE + 
                             s(MONTHS_IN_THE_JOB, k = 10) +
                             PROFESSION_CODE+
                             s(MATE_INCOME ,k=10) + 
                             FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS  +
                             s(PERSONAL_NET_INCOME ,k=10) + 
                             QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION  ,
                           data = datat_woe[trainInds, ],
                           family = "binomial", cluster = cl)
  
  gam.out_swoe <- mgcv::bam(TARGET_LABEL_BAD.1 ~ 
                              ID_SHOP +
                              SEX +
                              MARITAL_STATUS + 
                              s(AGE, k = 10) +
                              FLAG_RESIDENCIAL_PHONE +
                              AREA_CODE_RESIDENCIAL_PHONE + 
                              s(PAYMENT_DAY, k=8, bs = "cc") +
                              SHOP_RANK +
                              RESIDENCE_TYPE + 
                              s(MONTHS_IN_RESIDENCE, k = 10) +
                              FLAG_MOTHERS_NAME + 
                              FLAG_FATHERS_NAME + 
                              FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN +
                              FLAG_RESIDENCE_STATE_eq_WORKING_STATE + 
                              s(MONTHS_IN_THE_JOB, k = 10) +
                              PROFESSION_CODE+
                              s(MATE_INCOME ,k=10) + 
                              FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS  +
                              s(PERSONAL_NET_INCOME ,k=10) + 
                              QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION  ,
                            data = datat_swoe[trainInds, ],
                            family = "binomial", cluster = cl)
  
  gam.out_cwoe <- mgcv::bam(TARGET_LABEL_BAD.1 ~ 
                              ID_SHOP +
                              SEX +
                              MARITAL_STATUS + 
                              s(AGE, k = 10) +
                              FLAG_RESIDENCIAL_PHONE +
                              AREA_CODE_RESIDENCIAL_PHONE + 
                              s(PAYMENT_DAY, k=8, bs = "cc") +
                              SHOP_RANK +
                              RESIDENCE_TYPE + 
                              s(MONTHS_IN_RESIDENCE, k = 10) +
                              FLAG_MOTHERS_NAME + 
                              FLAG_FATHERS_NAME + 
                              FLAG_RESIDENCE_TOWN_eq_WORKING_TOWN +
                              FLAG_RESIDENCE_STATE_eq_WORKING_STATE + 
                              s(MONTHS_IN_THE_JOB, k = 10) +
                              PROFESSION_CODE+
                              s(MATE_INCOME ,k=10) + 
                              FLAG_RESIDENCIAL_ADDRESS_eq_POSTAL_ADDRESS  +
                              s(PERSONAL_NET_INCOME ,k=10) + 
                              QUANT_ADDITIONAL_CARDS_IN_THE_APPLICATION  ,
                            data = datat_cwoe[trainInds, ],
                            family = "binomial", cluster = cl)
  
  
  # Plot the spline function of the time variable for each of these:
  # plot(gam.out_woe)
  # plot(gam.out_swoe)
  # 
  # plot(gam.out_cwoe)
  
  
  # Let's fit the data using the shrunk WOE values.
  # The other results can be obtained by replacing "swoe" by "cwoe" or "woe"
  
  for (dataType in 1:3) {
    gam.out <- list(gam.out_woe, gam.out_swoe, gam.out_cwoe)[[dataType]]
    dataSel <- list(datat_woe, datat_swoe, datat_cwoe)[[dataType]]
    
    # Now specify which variables need to be discretized in spline functions.
    # "resp": response
    # lin: linear effect --> no discretization
    # cat: categorical variable
    # "UC": unconstrained binning of spline function
    # "C" constrained binning of spline function
    
    
    varTypes    <- c(rep("lin", 3), "UC", "lin", "lin", "UC", "lin", "lin","UC",
                     rep("lin", 4), "UC", "lin", "UC", "lin",
                     "UC", "lin", "resp")
    
    # maximum number of bins:
    kmax <- 10
    
    
    # Now bin the variables into k = 2, ..., kmax clusters
    binVars.out <- binVars(dataSel, gam.out, trainInds, testInds, varTypes, kmax)
    
    
    # The output above contains kmax-1 partitions of the spline effect.
    # in order to select the best one, we evaluate the criterion
    # WCSS + lambda * k for a range of lambda values
    
    lambdaseq_UC  <- exp(seq(-10, 5, by = 0.5))
    lambdaseq_C   <- 1 #exp(seq(0, 4, by = 0.5))
    
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
        glm.out   <- glm(TARGET_LABEL_BAD.1~., data = newData, family = "binomial")
        result[j, i] <- glm.out$aic
      }
    }
    best.params <- max(which(result == min(result))); best.params 
    
    # the optimal lambda is: 
    lambda_opt_UC <- lambdaseq_UC[best.params]; lambda_opt_UC 
    lambda_opt_C <- 0
    
    # this corresponds with different numbers of bins:
    bins1 <- 1 + which.min(drop(binVars.out$UC_WCSS[1, ]) + binVars.out$kseq * lambda_opt_UC) 
    bins2 <- 1 + which.min(drop(binVars.out$UC_WCSS[2, ]) + binVars.out$kseq * lambda_opt_UC) 
    bins3 <- 1 + which.min(drop(binVars.out$UC_WCSS[3, ]) + binVars.out$kseq * lambda_opt_UC) 
    bins4 <- 1 + which.min(drop(binVars.out$UC_WCSS[4, ]) + binVars.out$kseq * lambda_opt_UC) 
    bins5 <- 1 + which.min(drop(binVars.out$UC_WCSS[5, ]) + binVars.out$kseq * lambda_opt_UC) 
    bins6 <- 1 + which.min(drop(binVars.out$UC_WCSS[6, ]) + binVars.out$kseq * lambda_opt_UC) 
    
    totNbBins <- bins1 + bins2 + bins3 + bins4 + bins5 + bins6
    
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
    glm.out      <- glm(TARGET_LABEL_BAD.1~., data = newData, family = "binomial")
    Foldresults[foldnb, dataType, ] <- evalglm(glm.out, newData_test,
                                               newData_test$TARGET_LABEL_BAD.1)
    Foldresults[foldnb, dataType, 1] <- Foldresults[foldnb, dataType, 1] + (totNbBins - 3) 
    
    
    # as a first benchmark, we can compute the glm without splines:
    glm.out      <- glm(TARGET_LABEL_BAD.1~., data = dataSel[trainInds, ], family = "binomial")
    Foldresults[foldnb, 3 + dataType, ] <- evalglm(glm.out, dataSel[testInds, ],
                                                   dataSel$TARGET_LABEL_BAD.1[testInds])
    
    # second benchmark: gam without binning
    Foldresults[foldnb, 6 + dataType, ] <- evalglm(gam.out, dataSel[testInds, ],
                                                   dataSel$TARGET_LABEL_BAD.1[testInds])
  }
  
  
  # as a third benchmark, we consider xgboost:
  
  # one hot encoding:
  newdata_train <- as.matrix(one_hot(as.data.table(data[trainInds, -21])))
  newdata_test  <- as.matrix(one_hot(as.data.table(data[testInds, -21])))
  trainData <-  xgb.DMatrix(as.matrix(newdata_train),
                            label = as.numeric(data$TARGET_LABEL_BAD.1)[trainInds] - 1)
  testData  <- xgb.DMatrix(as.matrix(newdata_test),
                           label = as.numeric(data$TARGET_LABEL_BAD.1)[testInds] - 1)
  xgb.out   <- xgboost(data = trainData, objective = "reg:logistic", nrounds = 500)
  
  preds.out <- predict(xgb.out, newdata_test)
  
  result <- rep(0, 3)
  names(result) <- c("auc", "wbrier", "H-measure")
  y_test <-as.numeric(data$TARGET_LABEL_BAD.1)[testInds] - 1
  pred <- ROCR::prediction(as.vector(preds.out), as.factor(y_test));
  result[1] <- as.numeric(ROCR::performance(pred,"auc")@y.values)
  wts       <- (1 / (table(y_test) / sum(y_test)))[y_test + 1]
  result[2] <- weighted.mean((preds.out - y_test)^2,w = wts)
  result[3] <- hmeasure::HMeasure(true.class = y_test, scores = as.vector(preds.out))$metrics$H
  Foldresults[foldnb, 10, -c(1)]  <- round(result, 3)
  
}
stopCluster(cl)


################################################################################
################################################################################

table1 <- apply(Foldresults, c(2, 3), mean)
rownames(table1) <- c("woe + eglm", "swoe + eglm", "cwoe + eglm",
                      "woe + glm", "swoe + glm", "cwoe + glm",
                      "woe + gam", "swoe + gam", "cwoe + gam",
                      "xgb")
colnames(table1) <- c("AIC", "auc", "wbrier", "H-measure") 
table1 # we have also added the gam fits as a reference: 
#                  AIC    auc wbrier H-measure
# woe + eglm  33184.43 0.6732 0.3087    0.1093
# swoe + eglm 33291.17 0.6746 0.3083    0.1112
# cwoe + eglm 33185.57 0.6733 0.3086    0.1096
# woe + glm   33304.29 0.6693 0.3105    0.1043
# swoe + glm  33413.02 0.6701 0.3104    0.1056
# cwoe + glm  33305.30 0.6692 0.3105    0.1045
# woe + gam   33157.03 0.6737 0.3087    0.1102
# swoe + gam  33263.67 0.6747 0.3083    0.1112
# cwoe + gam  33158.19 0.6737 0.3087    0.1103
# xgb             0.00 0.6546 0.3168    0.0886


# Hypothesis testing for comparison of methods.


# AUC
# WOE vs SWOE:
wilcox.test(Foldresults[, 2, 2], Foldresults[, 1, 2], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 2, 2] and Foldresults[, 1, 2]
# V = 36, p-value = 0.005988
# alternative hypothesis: true location shift is greater than 0
wilcox.test(Foldresults[, 5, 2], Foldresults[, 4, 2], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 5, 2] and Foldresults[, 4, 2]
# V = 18.5, p-value = 0.05289
# alternative hypothesis: true location shift is greater than 0
wilcox.test(Foldresults[, 8, 2], Foldresults[, 7, 2], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 8, 2] and Foldresults[, 7, 2]
# V = 25.5, p-value = 0.02892
# alternative hypothesis: true location shift is greater than 0

# SWOE + eglm is better than SWOE+glm:
wilcox.test(Foldresults[, 2, 2], Foldresults[, 5, 2], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 2, 2] and Foldresults[, 5, 2]
# V = 44, p-value = 0.006386
# alternative hypothesis: true location shift is greater than 0


# wbrier
# WOE vs SWOE:
wilcox.test(Foldresults[, 2, 3], Foldresults[, 1, 3], paired = TRUE, alternative = "less")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 2, 3] and Foldresults[, 1, 3]
# V = 0, p-value = 0.03593
# alternative hypothesis: true location shift is less than 0
wilcox.test(Foldresults[, 5, 3], Foldresults[, 4, 3], paired = TRUE, alternative = "less")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 5, 3] and Foldresults[, 4, 3]
# V = 2, p-value = 0.3864
# alternative hypothesis: true location shift is less than 0
wilcox.test(Foldresults[, 8, 3], Foldresults[, 7, 3], paired = TRUE, alternative = "less")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 8, 3] and Foldresults[, 7, 3]
# V = 0, p-value = 0.03593
# alternative hypothesis: true location shift is less than 0


# SWOE + eglm is better than SWOE+glm:
wilcox.test(Foldresults[, 2, 3], Foldresults[, 5, 3], paired = TRUE, alternative = "less")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 2, 3] and Foldresults[, 5, 3]
# V = 0, p-value = 0.002118
# alternative hypothesis: true location shift is less than 0

# H-measure
# WOE vs SWOE:
wilcox.test(Foldresults[, 2, 4], Foldresults[, 1, 4], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 2, 4] and Foldresults[, 1, 4]
# V = 45, p-value = 0.004217
# alternative hypothesis: true location shift is greater than 0
wilcox.test(Foldresults[, 5, 4], Foldresults[, 4, 4], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 5, 4] and Foldresults[, 4, 4]
# V = 28, p-value = 0.009953
# alternative hypothesis: true location shift is greater than 0
wilcox.test(Foldresults[, 8, 4], Foldresults[, 7, 4], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 8, 4] and Foldresults[, 7, 4]
# V = 15, p-value = 0.02838
# alternative hypothesis: true location shift is greater than 0

# SWOE + eglm is better than SWOE + gam and SWOE+glm:
wilcox.test(Foldresults[, 2, 4], Foldresults[, 5, 4], paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test with continuity correction
# 
# data:  Foldresults[, 2, 4] and Foldresults[, 5, 4]
# V = 55, p-value = 0.002929
# alternative hypothesis: true location shift is greater than 0
