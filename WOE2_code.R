

fac2WOE <- function(data, yind, trainInds = NULL,
                    testInds = NULL, offSet = 0.1,
                    weightType = NULL, lambda = 0) {
  # Converts the factor variables in a dataset to WOE values.
  # Args:
  #   data: an n x p data matrix
  #   yind: the index of the column corresponding to the response variable
  #   trainInds: the indices of the observations used as training data
  #   testInds: the indices of the observations used as test data
  #   offSet: small offSet value applied to treat proportions in
  #   lambda: tuning parameter to select the number of clusters in clustered WOE
  # 
  # Returns:
  #   data_woe: the data matrix in which the factor variables have been replaced 
  #             by WOE values
  #   data_swoe: the data matrix in which the factor variables have been replaced 
  #             by WOE values based on shrinkage estimation of the proportion
  #   data_cwoe:the data matrix in which the factor variables have been replaced 
  #             by clustered WOE values 
  #   weights: weights used in the computation of clustered WOE values
  #
  facInds <- which(unlist(lapply(data, is.factor)))
  if (yind %in% facInds) {
    facInds <- facInds[-which(facInds == yind)]
  }
  data_woe <- data_cwoe <- data_swoe <- data
  
  if (is.null(trainInds)) {
    trainInds <- 1:dim(data)[1]
  }
  
  data_swoe[, facInds] <- data_woe[, facInds] <- data_cwoe[, facInds] <-
    matrix(0, dim(data)[1], length(facInds))
  wts <- matrix(0, dim(data)[1], length(facInds))
  colnames(wts) = facInds
  for (i in 1:length(facInds)) {
    facInd <- facInds[i]
    woe.out <- shrinkWOE(x_train = data[trainInds, facInd],
                         y_train  = data[trainInds, yind],
                         x_test = data[testInds, facInd],
                         offSet = offSet, weightType = weightType,
                         lambda = lambda)
    
    
    
    data_woe[trainInds, facInd]  <- woe.out$WOE_train
    data_swoe[trainInds, facInd] <- woe.out$WOEs_train
    data_cwoe[trainInds, facInd] <- woe.out$WOEc_train
    
    if (!is.null(testInds)) {
      data_woe[testInds, facInd] <- woe.out$WOE_test
      data_swoe[testInds, facInd] <- woe.out$WOEs_test
      data_cwoe[testInds, facInd] <- woe.out$WOEc_test
    }
    wts[trainInds, i] <- woe.out$weights
  }
  
  
  
  
  return(list(data_woe = data_woe, data_swoe = data_swoe,
              data_cwoe = data_cwoe, weights = wts))
}



evalglm <- function(glm.out, newdata, y_test) {
  # Evaluate a glm fit on newdata using the 
  # AUC, weighted brier sore and H-measure
  # The AIC on the training data is also returned.
  # 
  
  if (is.factor(y_test)) {y_test <- as.numeric(y_test) - 1}
  result <- rep(0, 4)
  names(result) <- c("AIC", "AUC", "wbrier", "H-measure")
  preds.out <- predict(glm.out, newdata = newdata,
                       type = "response")
  pred <- ROCR::prediction(as.vector(preds.out), as.factor(y_test));
  result[1] <-  glm.out$aic
  result[2] <- as.numeric(ROCR::performance(pred,"auc")@y.values) 
  wts       <- (1 / (table(y_test) / sum(y_test)))[y_test + 1]
  result[3] <- weighted.mean((preds.out - y_test)^2,w = wts)
  result[4] <- hmeasure::HMeasure(true.class = y_test, scores = as.vector(preds.out))$metrics$H
  
  return(round(result, 3))
}

shrinkWOE <- function(x_train, y_train, x_test = NULL,
                      offSet = 0,
                      weightType = NULL, lambda = lambda) {
  # convert a single factor variable into WOE, SWOE and CWOE values
  # 
  # args: 
  #   x_train: the training data (must be a factor variable)
  #   y_train: the response of the training data
  #   x_test: the test data (must be a factor with the same levels as x_train)
  #   offSet: the offset value used to treat proportions in {0,1}
  #   lambda: a tuning parameter used to select the number of clusters for 
  #           computing the CWOE values.
  #
  # returns:
  #   WOE_train: the WOE values of the data in x_train
  #   WOE_test: the WOE values of the data in x_test
  #   WOEs_train: the shrinkage-based WOE values of the data in x_train
  #   WOEs_test: the shrinkage-based WOE values of the data in x_test
  #   WOEC_train: the clustered WOE values of the data in x_train
  #   WOEC_test: the clustered WOE values of the data in x_test
  #   weights: the weights used in the computation of the clustered WOE values.
  #
  
  # offset can be chosen between 0 and 1
  if (!is.factor(x_train)) {stop("x_train must be a factor")}
  if (is.factor(y_train)) {y_train <- as.numeric(y_train) - 1}
  n    <- length(y_train)
  cats <- levels(x_train)
  
  
  # local estimates
  nj    <- table(x_train)
  phatj <- table(x_train, y_train); phatj <- phatj[, 2] / rowSums(phatj)
  qj    <- nj / n
  
  if (!is.null(offSet)) { # local offset
    phatj <- pmin(pmax(phatj, offSet / nj), (nj - offSet) / nj)
  }
  phatj[which(nj == 0)] <- sum(y_train) / n
  

  
  # global estimates
  phat       <-  sum(y_train) / n # global phat
  var_global <- phat * (1 - phat) / n
  var_between <- var(phatj) # variance between different categories
  
  vj  <-  bj <- phatsj <- rep(1, length(cats)) # sample variances of each category
  WOE_train <- WOEs_train <- phatsj_train <-
    phatj_train <- catSizes <- rep(0, n) # as a vector of length n
  WOE_test <- WOEs_test <- phatsj_test <- phatj_test <-
    rep(0, length(x_test))
  
  for (j in 1:length(cats)) {
    catInds   <- which(x_train == cats[j])
    catSizes[catInds] <- length(catInds)
    # estimate variances
    
    if (nj[j] < 50 | (phatj[j] %in% c(0, 1))) {
      vj[j] <- var_global / qj[j] # use global proportions to estimate variance
    } else {
      vj[j] <-  phatj[j] * (1 - phatj[j]) / nj[j]
    }
    # compute shrinkage coefficients
    
    bj[j] <- (vj[j] * (1 - qj[j])) / 
      (vj[j] * (1 - 2 * qj[j]) + var_global + var_between)
    
    if (!is.finite(bj[j])) {
      bj[j] <- 1
    }
    # compute shrunk proportions
    
    phatsj[j] <- (1 - bj[j]) * phatj[j] + bj[j] * phat # shrinkage
    
    # fill output containers
    phatj_train[catInds]  <- phatj[j]
    phatsj_train[catInds]  <- phatsj[j]
    
    WOE_train[catInds] <- log(phatj[j] / (1 - phatj[j]))
    WOEs_train[catInds] <- log(phatsj[j] / (1 - phatsj[j]))
    
    if (!is.null(x_test) & (length(x_test) > 0)) {
      catInds_test <- which(x_test == cats[j]) 
      phatj_test[catInds_test]  <- phatj[j]
      phatsj_test[catInds_test]  <- phatsj[j]
      
      WOE_test[catInds_test] <- log(phatj[j] / (1 - phatj[j]))
      WOEs_test[catInds_test] <- log(phatsj[j] / (1 - phatsj[j]))
    }
  }
  
  
  # Now perform univariate clustering On WOE
  
  if (is.null(weightType)) {
    wts <- rep(1, length(y_train))
  } else if (weightType == "sdprop") {
    wts <- 1 / sqrt(phatsj_train * (1 - phatsj_train) / catSizes)
    wts <- wts / sum(wts) * length(y_train)
  } else if (weightType == "varprop") {
    wts <- 1 / (phatsj_train * (1 - phatsj_train) / catSizes)
    wts <- wts / sum(wts) * length(y_train)
  } else if (weightType == "sizeprop") {
    wts <-  catSizes
    wts <- wts / sum(wts) * length(y_train)
  } else if (weightType == "sqrtsizeprop") {
    wts <- sqrt( catSizes)
    wts <- wts / sum(wts) * length(y_train)
  }
  
  ##
  kmax       <-  min(length(unique(WOE_train)), 50)
  # 
  WCSS <- rep(0, kmax - 1)
  for (k in 2:kmax) {
    tempc <- Ckmeans.1d.dp::Ckmeans.1d.dp(WOE_train,
                                          k =  k, y = wts)
    
    WCSS[k - 1] <- tempc$tot.withinss + k * lambda
  }
  km.out <- Ckmeans.1d.dp::Ckmeans.1d.dp(WOE_train,
                                         k =  1 + which.min(WCSS),
                                         y = wts)
  #
  WOEc_train <- km.out$centers[km.out$cluster]
  WOEc_test <- NULL
  if (!is.null(x_test)& (length(x_test) > 0)) {
    testIDs   <- sapply(WOE_test, function(y) which.min((y - km.out$centers)^2))
    WOEc_test <- km.out$centers[testIDs]
  }
  
  return(list(phatj_train = phatj_train, phatj_test = phatj_test,
              phatsj_train = phatsj_train, phatsj_test = phatsj_test,
              WOE_train = WOE_train, WOE_test = WOE_test,
              WOEs_train = WOEs_train, WOEs_test = WOEs_test,
              WOEc_train = WOEc_train, WOEc_test = WOEc_test,
              fullWOE = c(WOE_train, WOE_test),
              fullsWOE = c(WOEs_train, WOEs_test),
              fullcWOE = c(WOEc_train, WOEc_test),
              weights = wts))
}






binCvar <- function(x_train, x_test, sx_train, sdsx_train, kseq) {
  # Bin a variable using constrained binning
  # args: 
  #   x_train: the training data
  #   x_test: the test data
  #   sx_train: the spline fitted to x_train
  #   sdsx_train: the standard deviations of the spline at each point of x_train
  #   kseq: a sequence of possible numbers of clusters
  #
  # returns:
  #   result_train: binned variable x_train
  #   result_test: binned variable x_test
  #   WCSS: within-cluster sums of squares 
  #   km.out: the output of the k-segments clusterings
  # 
  
  xt.order           <- order(x_train)
  x_train_ordered    <- x_train[xt.order]
  time_test          <- x_test
  sx_train_ordered   <- sx_train[xt.order]
  sdsx_train_ordered <- sdsx_train[xt.order]
  
  wts.orig <- 1 / sdsx_train^2
  wts      <- 1 / sdsx_train_ordered^2
  wts      <- wts / sum(wts) * length(sx_train)
  wts      <- pmax(wts, 0.25)
  nbtimes  <- round(wts / min(wts))
  
  # cluster the spline 
  result_train <- array(0, dim = c(length(sx_train), length(kseq)))
  WCSS         <- rep(NA, length(kseq))
  result_test  <- array(0, dim = c(length(x_test), length(kseq)))
  km.out       <- list()
  
  for (k in 1:length(kseq)) {
    # add observations to include weights
    kmeans.out <- Ckmeans.1d.dp::Cksegs.1d.dp(y = rep(sx_train_ordered,
                                                      times = nbtimes),
                                              x = rep(x_train_ordered,
                                                      times = nbtimes),
                                              k = kseq[k])
    
    # take the added observations back out
    origcenters   <- kmeans.out$centers
    origcenters_f <- sapply(1:kseq[k], function(z) mean(rep(sx_train_ordered,
                                                            times = nbtimes)[which(kmeans.out$cluster == z)]))
    kmeans.out$cluster <- kmeans.out$cluster[cumsum(nbtimes)]
    breakpoints        <- sapply(which(diff(kmeans.out$cluster) != 0),
                                 function(z) (x_train_ordered[z] + x_train_ordered[z + 1])/2)
    
    kmeans.out$breakpoints <- breakpoints
    kmeans.out$origcenters <- origcenters
    kmeans.out$origcenters_f <- origcenters_f
    
    # Now translate the cluster centers in x to cluster centers in f(x) 
    kmeans.out$cluster[xt.order] <- kmeans.out$cluster
    
    kmeans.out$centers <- sapply(1:kseq[k], function(z) sum(sx_train[which(kmeans.out$cluster == z)] *
                                                        wts.orig[which(kmeans.out$cluster == z)]) /
                                   sum(wts.orig[which(kmeans.out$cluster == z)]))
    kmeans.out$tot.withinss <- sum(wts.orig *
                                     (sx_train - kmeans.out$centers[kmeans.out$cluster])^2)
    
    result_train[, k]   <- kmeans.out$center[kmeans.out$cluster]
    WCSS[k]             <- kmeans.out$tot.withinss
    testIDs             <- suppressWarnings(sapply(time_test,
                                                   function(z) min(which(z < breakpoints))))
    testIDs[which(testIDs == Inf)] <- kseq[k]
    result_test[, k]   <- kmeans.out$center[testIDs]
    km.out[[k]]        <- kmeans.out
  }
  return(list(result_train = result_train, WCSS = WCSS, 
              result_test = result_test, km.out = km.out))
}


binUCvar <- function(sx_train, sx_test, sdsx_train, kseq) {
  # Bin a variable using unconstrained binning
  # args: 
  #   x_train: the training data
  #   x_test: the test data
  #   sx_train: the spline fitted to x_train
  #   sdsx_train: the standard deviations of the spline at each point of x_train
  #   kseq: a sequence of possible numbers of clusters
  #
  # returns:
  #   result_train: binned variable x_train
  #   result_test: binned variable x_test
  #   WCSS: within-cluster sums of squares 
  #   km.out: the output of the k-means clusterings
  # 
  
  # get weights
  sds <- sdsx_train
  wts <- 1 / sds^2
  wts <- wts / sum(wts) * length(sx_train)
  
  # Cluster the spline 
  result_train <- array(0, dim = c(length(sx_train), length(kseq)))
  WCSS         <- rep(NA, length(kseq))
  result_test  <- array(0, dim = c(length(sx_test), length(kseq)))
  km.out       <- list()
  
  for (k in 1:length(kseq)) {
    kmeans.out <- Ckmeans.1d.dp::Ckmeans.1d.dp(sx_train,
                                               k = kseq[k], y = wts)
    result_train[, k] <- kmeans.out$center[kmeans.out$cluster]
    WCSS[k]           <- kmeans.out$tot.withinss
    testIDs           <- sapply(sx_test,
                                function(z) which.min((z - kmeans.out$center)^2))
    result_test[, k]  <- kmeans.out$center[testIDs]
    km.out[[k]]       <- kmeans.out
  }
  return(list(result_train = result_train, WCSS = WCSS, 
              result_test = result_test, km.out = km.out))
}



binVars <- function(data, gam.out, trainInds, testInds, varTypes, kmax = 10) {
  # Bin the variables in a dataset using constrained or unconstrained binning
  # args: 
  #   data: the dataset
  #   gam.out: the fitted gam model
  #   trainInds: the row indices corresponding with the observations in the training data
  #   testInds: the row indices corresponding with the osbervations in the test data
  #   varTypes: a vector of length dim(data)[2] indicating the variable types:
  #     "resp": response
  #     "lin": linear effect --> no discretization
  #     "cat": categorical variable--> no discretization
  #     "UC": unconstrained binning of spline function
  #     "C" constrained binning of spline function
  #   kmax: the maximum number of bins to consider
  #
  # returns:
  #   UC_cl: the variables of the training data binned using unconstrained clustering
  #   UC_WCSS: the WCSS values of the unconstrained clusterings
  #   UC_cl_test: the variables of the test data binned using unconstrained clustering
  #   UC_kms: the full k-means outputs
  #   C_cl: the variables of the training data binned using constrained clustering
  #   C_WCSS: the WCSS values of the constrained clusterings
  #   C_cl_test: the variables of the test data binned using constrained clustering
  #   C_kms: the full k-segments outputs
  #   Cvar: the indices of the variables which were binned using constrained clustering
  #   UCvar: the indices of the variables which were binned using unconstrained clustering
  #   kseq: the sequence of k-values considered
  #
  
  Cvar        <- which(varTypes == "C")
  UCvar       <- which(varTypes == "UC")
  UCvar_names <- colnames(data)[UCvar]
  Cvar_names  <- colnames(data)[Cvar]
  nbUCvar     <- length(UCvar)  # number of variables with nonlinear effects
  nbCvar      <- length(Cvar)  # number of variables with nonlinear effects
  
  
  kseq <- 2:kmax
  
  # Extract the splines
  internalpred.out <- predict(gam.out, newdata = data[trainInds, ],
                              type = "terms", se.fit = TRUE)
  fittedTerms      <- internalpred.out$fit
  fittedSes        <- internalpred.out$se.fit
  testpred.out     <- predict(gam.out, newdata = data[testInds, ],
                              type = "terms", se.fit = TRUE)
  fittedTerms_test <- testpred.out$fit
  
  
  # clustering of unconstrained effects
  UC_cl      <- array(0, dim = c(nbUCvar, length(trainInds), length(kseq)))
  UC_cl_test <- array(0, dim = c(nbUCvar, length(testInds), length(kseq)))
  UC_WCSS    <- array(NA, dim = c(nbUCvar, length(kseq)))
  UC_kms     <- list()
  
  if (nbUCvar > 0) {
    for (j in 1:nbUCvar) {
      varNb      <- UCvar[j]
      varName    <- UCvar_names[j]
      varNB2     <- which(colnames(fittedTerms) == paste("s(", varName, ")", sep = ""))
      x_train    <- data[trainInds, varNb]
      sx_train   <- fittedTerms[, varNB2]
      sx_test    <- fittedTerms_test[, varNB2]
      sdsx_train <- fittedSes[, varNB2]
      
      binUCvar.out <- binUCvar(sx_train = sx_train, sx_test = sx_test,
                               sdsx_train = sdsx_train, kseq = kseq)
      
      UC_cl[j, , ]      <- binUCvar.out$result_train
      UC_WCSS[j, ]      <- binUCvar.out$WCSS
      UC_cl_test[j, , ] <- binUCvar.out$result_test
      UC_kms[[j]]       <- binUCvar.out$km.out
    }
  }
  
  
  # clustering of constrained effects
  C_cl      <- array(0, dim = c(nbCvar, length(trainInds), length(kseq)))
  C_cl_test <- array(0, dim = c(nbCvar, length(testInds), length(kseq)))
  C_WCSS    <- array(NA, dim = c(nbCvar, length(kseq)))
  C_kms     <- list()
  
  if (nbCvar > 0) {
    for (j in 1:nbCvar) {
      varNb      <- Cvar[j]
      varName    <- Cvar_names[j]
      varNB2     <- which(colnames(fittedTerms) == paste("s(", varName, ")", sep = ""))
      x_train    <- data[trainInds, varNb]
      x_test     <- data[testInds, varNb]
      sx_train   <- fittedTerms[, varNB2]
      sx_test    <- fittedTerms_test[, varNB2]
      sdsx_train <- fittedSes[, varNB2]
      
      binCvar.out <- binCvar(x_train = x_train, x_test = x_test, 
                             sx_train = sx_train, sdsx_train = sdsx_train,
                             kseq = kseq)
      
      C_cl[j, , ]      <- binCvar.out$result_train
      C_WCSS[j, ]      <- binCvar.out$WCSS
      C_cl_test[j, , ] <- binCvar.out$result_test
      C_kms[[j]]       <- binCvar.out$km.out
    }
  }
  return(list(UC_cl = UC_cl,
              UC_WCSS = UC_WCSS,
              UC_cl_test = UC_cl_test,
              UC_kms = UC_kms,
              C_cl = C_cl,
              C_WCSS = C_WCSS,
              C_cl_test = C_cl_test,
              C_kms = C_kms,
              Cvar = Cvar,
              UCvar = UCvar,
              kseq = kseq))
}
