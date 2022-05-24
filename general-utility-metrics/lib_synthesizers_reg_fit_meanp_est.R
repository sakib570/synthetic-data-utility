## Author Niklas Reje (nreje@kth.se)
library("synthpop")
library("spatialEco")
source("synthesizers.R")
source("library_cio_and_variance.R")

##Parametric and Decision

# Method for the standard parametric methods synthetsizer.
# ods is the original dataset.
# visitSeq is the imputation order.
# m is the number of released synthetic datasets.
# proper is a boolean for using proper or non-proper imputation.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
TestPar <- function(ods,visitSeq,m = 1,proper = FALSE,predMat = NULL){
  sds <- SynthMeth(ods,"parametric",visitSeq,predMat,m,proper)
  return(sds)
}

# Method for the Decision Trees synthetsizer.
# ods is the original dataset.
# visitSeq is the imputation order.
# m is the number of released synthetic datasets.
# proper is a boolean for using proper or non-proper imputation.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
TestDec <- function(ods,visitSeq,m = 1,proper = FALSE,predMat = NULL){
  sds <- SynthMeth(ods,"cart",visitSeq,predMat,m,proper)
  return(sds)
}

# Method for Decision Trees that ignore crashes which is needed because of a bug in implentation. Bit more info in Synthesizers.R.
# ods is the original dataset.
# visitSeq is the imputation order.
# m is the number of released synthetic datasets.
# proper is a boolean for using proper or non-proper imputation.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
TestDecPF <- function(ods,visitSeq,m = 10,proper = TRUE,predMat = NULL){
  sds <- forceDec(ods,"cart",visitSeq,predMat,m,proper)
  return(sds)
}

# Method for the catall with pmm synthetsizer.
# ods is the original dataset.
# visitSeq is the imputation order.
# m is the number of released synthetic datasets.
# proper is a boolean for using proper or non-proper imputation.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
TestCAP <- function(ods,method,visitSeq,m = 1,proper = FALSE,predMat = NULL){
  sds <- SynthMeth(ods,method,visitSeq,predMat,m,proper)
  return(sds)
}

# Method for the catall with cart synthetsizer. Uses forceDec because of crash issues with Decision Trees implmentation. Bit more info in Synthesizers.R.
# ods is the original dataset.
# visitSeq is the imputation order.
# m is the number of released synthetic datasets.
# proper is a boolean for using proper or non-proper imputation.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
TestCAC <- function(ods,method,visitSeq,m = 10,proper = FALSE,predMat = NULL){
  sds <- forceDec(ods,method,visitSeq,predMat,m,proper)
  return(sds)
}



##Test glm

# Method for testing regression fits on the synthetic dataset(s) and the original dataset.
# sds is an object that contatins the synthetic dataset(s).
# ods is the original dataset.
# mSet is an integer vector and is used for how many datasets should be combined as released datasets.
# k is the number of different samples made of the regression fit results for each number of released datasets in mSet.
# formulas is a formula vector that has the formulas for each regression fit.
# families is a character vector for each family function used for the formula of the matching index.
# return is a list of lists of lists that breaks down over formulas, mSet, k and finally the results for the different variance estimators.
TestGLM <- function(sds,ods,mSet,k,formulas,families){
  n.obs <- sds$n # Number of records in the original dataset.
  n.syn <- sds$k # Number of records in the synthetic datasets.
  m.og <- sds$m  # Number of synthetic datasets.
  glmList <- list()
  
  preList <- getCleanSds(sds,formulas,families) # To find if any synthetic dataset does not work with any regression fit. More info at getCleanSds.
  
  fitList <- preList[[1]] # A list with all coefficients and variables for each syntehtic dataset and regression fit.
  clean <- c(1:m.og)[!preList[[2]]] # An integer vector of those synthetic dataset that had no poor fits.
  
  rm(preList)
  
  sampleList <- list()
  # Create k number of samples to use for all regression fits for each m 
  for(m in mSet){
    sample <- matrix(NA,nrow = m,ncol = k)
    for(i in c(1:k)){
      if(m > 1){
        sample[,i] <- sample(clean,m)
      }
      else{
        sample[,i] <- c(i)
      }
    }
    sampleList <- c(sampleList,list(sample))
  }
  
  for(i in c(1:length(formulas))){
    message(paste0("fit ",i,": "),appendLF = FALSE)
    formula <- formulas[[i]]
    family <- families[[i]]
    
    model.ods <- glm({{formula}}, {{family}}, ods,maxit = 100)
    message("(MODEL), ",appendLF = FALSE)
    obser <- cbind(model.ods$coefficients,summary(model.ods)$coefficients[,2])
    synth.coef <- fitList[[i]][[1]]
    synth.var <- fitList[[i]][[2]]
    
    # Clean up so that the synth and obser has the same coefficients.
    obserTmp <- matrix(NA,nrow = ncol(synth.coef),ncol = 2,
                       dimnames = list(colnames(synth.coef), c("obs.coef","obs.se")))
    insO <- match(rownames(obser),rownames(obserTmp))
    for(i in c(1:length(insO))){
      idx <- insO[i]
      if(!is.na(idx)){
        obserTmp[idx,] <- obser[i,]
      }
    }
    
    obser <- obserTmp
    rm(obserTmp)
    
    proper <- sds$proper # If using proper imputation or not.
    rm(model.ods)
    
    topList <- list()
    j <- 0
    for(m in mSet){
      j <- j + 1
      message(paste0(m,", "),appendLF = FALSE)
      mList <- list()
      for(i in c(1:k)){
        synset <- sampleList[[j]][,i] # The indexes of the sampled synthetic datasets.
        
        db.mcoef <- synth.coef[synset,]
        db.mvar <- synth.var[synset,]
        if(m > 1){
          db.mcoefavg <- apply(db.mcoef,2,mean)
          db.mvaravg <- apply(db.mvar,2,mean)
          db.bm <- apply(db.mcoef,2,var)
        }
        else{
          db.mcoefavg <- sapply(db.mcoef,mean)
          db.mvaravg <- sapply(db.mvar,mean)
          db.bm <- NULL # Can't be calculated if there is only one synthetic dataset.
        }
        db <- list(coef = db.mcoefavg,vars = db.mvaravg,bm = db.bm,m = m,n = n.obs,k = n.syn)
        
        # c# use different variance estimations. c#CI are the CI overlaps with different variance estimations.
        # ReDa# contains the original coefficient and variance, estimated coefficient and different estimated variance and the different CI overlaps.
        
        c1 <- estimator(db,incomplete = FALSE, population.inference = FALSE,proper = proper)
        c2 <- estimator(db,incomplete = FALSE, population.inference = TRUE,proper = proper)
        c1CI <- compare.CI.T(c1,obser,ci.level = 0.95, intercept = TRUE)
        c2CI <- compare.CI.T(c2,obser,ci.level = 0.95, intercept = TRUE)
        numVar <- nrow(c1); rNames <- rownames(c1)
        ReDa1 <- matrix(NA, nrow = numVar, ncol = 5, 
                        dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
        ReDa1[,1:2] <- obser[,1:2]
        ReDa1[,3:4] <- c1[,1:2]
        ReDa1[,5] <- c1CI[,1]
        ReDa2 <- matrix(NA, nrow = numVar, ncol = 5, 
                        dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
        ReDa2[,1:2] <- obser[,1:2]
        ReDa2[,3:4] <- c2[,1:2]
        ReDa2[,5] <- c2CI[,1]
        if(m > 1){ #This variance estimator is only possible with more than one synthetic dataset.
          c3 <- estimator(db,incomplete = TRUE, population.inference = TRUE,proper = sds$proper)
          c3CI <- compare.CI.T(c3,obser,ci.level = 0.95, intercept = TRUE)
          ReDa3 <- matrix(NA, nrow = numVar, ncol = 5, 
                          dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
          ReDa3[,1:2] <- obser[,1:2]
          ReDa3[,3:4] <- c3[,1:2]
          ReDa3[,5] <- c3CI[,1]
          kList <- list(ReDa1,ReDa2,ReDa3)
        }
        else{ kList <- list(ReDa1,ReDa2) }
        
        mList <- c(mList,list(kList))
      }
      topList <- c(topList,list(mList))
    }
    message("DONE",appendLF = TRUE)
    glmList <- c(glmList,list(topList))
  }
  glmList <- c(glmList,list(list(sampleList,clean))) # Include all the combinations of synthetic datasets used.
  return(glmList)
}

# The regression fits on the synthetic datasets are done here.
# After a regression fit has been done the synthetic datasets the coefficients are checked that they are not too large nor that the variance is not zero 
#  because that would most likely indicate that the fit was poor and including it would scewer the results.
# Any synthetic dataset where at least one of the regression fits are poor are excluded.
# This is so one can use the same synthetic dataset combination for each regression fit.
# sds is an object with the synthetic datasets.
# formulas is a formula vector that has the formulas for each regression fit.
# families is a character vector for each family function used for the formula of the matching index.
# return is a list with the regression fit coefficients and variances together with a boolean vector for which datasets have poor fits.
getCleanSds <- function(sds,formulas,families){
  fitList <- list()
  clean <- matrix(NA,nrow = length(formulas),ncol = sds$m) # True if poor fit, else false.
  message("CLEAN: ",appendLF = FALSE)
  for(i in c(1:length(formulas))){
    message(paste0(i,", "),appendLF = FALSE)
    formula <- formulas[[i]]
    family <- families[[i]]
    
    model.sds <- glm.synds({{formula}}, {{family}}, sds,maxit = 100)
    
    synth.coef <- model.sds$mcoef #Regression fit coefficients
    synth.var <- model.sds$mvar   #Regression fit variances
    
    rm(model.sds)
    
    clean[i,] <- (abs(synth.coef[,1]) > 10**10 | synth.var[,1] == 0) # Set those synthetic dataset and regression fit combinations that are poor to true.
    
    synthList <- list(synth.coef,synth.var)
    
    fitList <- c(fitList,list(synthList))
  }
  
  message("DONE",appendLF = TRUE)
  return(list(fitList,apply(clean,2,any)))
}


# Calculating CI overlaps for the mean of numeric variables is done here.
# Was designed to use functions that only act on one variable at the time but can use functions that act on the entire dataset if alt is set to TRUE.
# sds is an object with the synthetic datasets.
# ods is the original dataset.
# est is the function which will be applied.
# estU is the variance calculation for function est.
# vari is a numerical vector to say which variables est should be applied on. Only used when alt is FALSE.
# mSet is an integer vector and is used for how many datasets should be combined as released datasets.
# k is the number of different samples made of the regression fit results for each number of released datasets in mSet.
# alt is used to change between applying one function to the entire dataset (TRUE) or only to one variable at a time (FALSE).
# return is a list of lists that breaks down over mSet, k and finally the results for the different variance estimators.
TestEst <- function(sds,ods,est,estU,vari,mSet,k,alt = FALSE){
  m <- sds$m # Number of synthetic dataset.
  no <- sds$n # Number of records in the syntehtic dataset.
  ns <- sds$k # Number of records in the original dataset.
  
  if(alt == TRUE){ # If TRUE apply to entire dataset.
    obser <- cbind(match.fun(est)(ods),sqrt(match.fun(estU)(ods)))
  }
  else{ # If FALSE apply to variables in vari separately.
    obser <- cbind(apply(ods[,vari],2,match.fun(est)),sqrt(apply(ods[,vari],2,match.fun(estU))))
  }
  
  message("(PRE), ",appendLF = FALSE)
  pointEstMat <- matrix(NA, nrow = m, ncol = length(vari), 
                        dimnames = list(c(1:m), colnames(ods)[vari]))
  pointVarMat <- matrix(NA, nrow = m, ncol = length(vari), 
                        dimnames = list(c(1:m), colnames(ods)[vari]))
  for(i in c(1:m)){
    if(alt == TRUE){ # If TRUE apply to entire dataset.
      pointEstMat[i,1] <- match.fun(est)(sds$syn[[i]])
      pointVarMat[i,1] <- match.fun(estU)(sds$syn[[i]])
    }
    else{ # If FALSE apply to variables in vari separately.
      pointEstMat[i,] <- apply(sds$syn[[i]][,vari],2,match.fun(est))
      pointVarMat[i,] <- apply(sds$syn[[i]][,vari],2,match.fun(estU))
    }
  }
  topList <- list()
  for(mi in mSet){
    message(paste0(mi,", "),appendLF = FALSE)
    pointEst <- matrix(NA, nrow = mi, ncol = length(vari), 
                       dimnames = list(c(1:mi), colnames(ods)[vari]))
    pointVar <- matrix(NA, nrow = mi, ncol = length(vari), 
                       dimnames = list(c(1:mi), colnames(ods)[vari]))
    mList <- list()
    for(i in c(1:k)){
      if(mi > 1){
        synset <- sample(m,mi)
      }
      else{
        synset <- c(i)
      }
      pointEst[] <- pointEstMat[synset,]
      pointVar[] <- pointVarMat[synset,]
      
      if(mi > 1){
        mcoefavg <- apply(pointEst,2,mean)
        mvaravg <- apply(pointVar,2,mean)
        bm <- apply(pointEst,2,var)
      }
      else{
        mcoefavg <- apply(rbind(pointEst,pointEst),2,mean)
        mvaravg <- apply(rbind(pointVar,pointVar),2,mean)
        bm <- NULL # Can't be calculated if there is only one synthetic dataset.
      }
      
      # c# use different variance estimations. c#CI are the CI overlaps with different variance estimations.
      # ReDa# contains the original coefficient and variance, estimated coefficient and different estimated variance and the different CI overlaps.
      
      db <- list(coef = mcoefavg,vars = mvaravg,bm = bm,m = mi,n = no,k = ns)
      c1 <- estimator(db,incomplete = FALSE, population.inference = FALSE,proper = sds$proper)
      c2 <- estimator(db,incomplete = FALSE, population.inference = TRUE,proper = sds$proper)
      c1CI <- compare.CI.T(c1,obser,ci.level = 0.95, intercept = TRUE)
      c2CI <- compare.CI.T(c2,obser,ci.level = 0.95, intercept = TRUE)
      numVar <- nrow(c1); rNames <- rownames(c1)
      ReDa1 <- matrix(NA, nrow = numVar, ncol = 5, 
                      dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
      ReDa1[,1:2] <- obser[,1:2]
      ReDa1[,3:4] <- c1[,1:2]
      ReDa1[,5] <- c1CI[,1]
      ReDa2 <- matrix(NA, nrow = numVar, ncol = 5, 
                      dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
      ReDa2[,1:2] <- obser[,1:2]
      ReDa2[,3:4] <- c2[,1:2]
      ReDa2[,5] <- c2CI[,1]
      if(mi > 1){
        c3 <- estimator(db,incomplete = TRUE, population.inference = TRUE,proper = sds$proper)
        c3CI <- compare.CI.T(c3,obser,ci.level = 0.95, intercept = TRUE)
        ReDa3 <- matrix(NA, nrow = numVar, ncol = 5, 
                        dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
        ReDa3[,1:2] <- obser[,1:2]
        ReDa3[,3:4] <- c3[,1:2]
        ReDa3[,5] <- c3CI[,1]
        kList <- list(ReDa1,ReDa2,ReDa3)
      }
      else{ kList <- list(ReDa1,ReDa2) }
      
      mList <- c(mList,list(kList))
    }
    topList <- c(topList,list(mList))
  }
  message("DONE",appendLF = TRUE)
  return(topList)
}


# Calculating KL-divergence scores for the synthetic datasets split for all the variables.
# sds is an object with the synthetic datasets.
# ods is the original dataset.
# return is a matrix with KL-divergence scores for each original and synthetic dataset combination split over the variables.
TestKLD <- function(sds,ods){
  odsT <- do.call(cbind,ods) # Transform categorical variables into numerical variables.
  
  message("(KLD), ",appendLF = FALSE)
  if(sds$m > 1){
    sdsT <- list()
    for(i in c(1:sds$m)){
      sdsT <- c(sdsT,list(do.call(cbind,sds$syn[[i]]))) # Transform categorical variables into numerical variables.
    }
  }
  else{
    sdsT <- list(do.call(cbind,sds$syn)) # Transform categorical variables into numerical variables.
  }
  
  cNames <- colnames(ods)
  
  # Used to hold the maximum and minimum value for each variable seen in the original and all the synthetic datasets.
  odsLimits <- matrix(NA, nrow = 2, ncol = ncol(ods), 
                      dimnames = list(c("low","high"),cNames)) 
  
  for(i in c(1:ncol(ods))){
    mn <- min(odsT[,i])
    mx <- max(odsT[,i])
    for(j in c(1:sds$m)){
      mn <- min(mn,min(sdsT[[j]][,i]))
      mx <- max(mx,max(sdsT[[j]][,i]))
    }
    odsLimits[1,i] <- mn
    odsLimits[2,i] <- mx
  }
  
  topList <- list()
  for(i in c(1:ncol(ods))){
    probO <- NRProbEst2(odsT[,i],odsLimits[1,i],odsLimits[2,i]) # Calculate density distribution for original dataset.
    synProbs <- matrix(NA, nrow = length(probO), ncol = sds$m)
    for(j in c(1:sds$m)){
      synT <- sdsT[[j]]
      probS1 <- NRProbEst2(synT[,i],odsLimits[1,i],odsLimits[2,i]) # Calculate density distribution for synthetic datasets.
      synProbs[,j] <- probS1[]
    }
    cList <- list(probO,synProbs)
    topList <- c(topList,list(cList))
  }
  
  # Used to hold the KL-divergence score for original and synthetic combination split over each variable.
  klCol <- matrix(NA, nrow = sds$m, ncol = ncol(ods), 
                  dimnames = list(c(1:sds$m),cNames))
  
  for(i in c(1:ncol(ods))){
    ogN <- topList[[i]][[1]]
    for(j in c(1:sds$m)){
      ngN <- topList[[i]][[2]][,j]
      rt <- cbind(ogN,ngN)
      kl <- kl.divergence(rt,eps = 10^-4,overlap = TRUE) # Calculate KL-divergence.
      klCol[[j,i]] <- (kl[[2,1]] + kl[[1,2]])/2
    }
  }
  
  message("DONE",appendLF = TRUE)
  return(klCol)
}


# Create density distribution for use with KL-divergence score calculations.
# x is a numerical vector.
# low is the lower bound.
# high is the upper bound.
# width is the step-size.
# return is a density distribution.
NRProbEst2 <- function(x,low,high,width = 1){
  x <- as.numeric(x)
  if(missing(low)){low = min(x)}
  if(missing(high)){high = max(x)}
  prob <- seq(low,high,width)
  prob[] <- 0
  d <- floor((x - low)/width) + 1 # Transform all numerical values into discrete integers.
  for(i in d){
    if(i >= 1 && i <= length(prob)){
      prob[i] <- prob[i] + 1
    }
  }
  prob <- prob / length(x)
  return(prob)
}

# Method for testing regression fits on k-anonymized datasets.
# odsK is a k-anonymized dataset.
# odsF is the original dataset.
# formulas is a formula vector that has the formulas for each regression fit.
# families is a character vector for each family function used for the formula of the matching index.
# return is a list of the results for all the formulas tested.
testGLMA <- function(odsK,odsF,formulas,families){
  n.obs <- nrow(odsF) # Number of records in the original dataset.
  n.syn <- nrow(odsK) # Number of records in k-anonymized dataset.

  glmList <- list()
  
  message("test: ",appendLF = FALSE)
  
  for(i in c(1:length(formulas))){
    message(paste0(i,", "),appendLF = FALSE)
    
    formula <- formulas[[i]]
    family <- families[[i]]
    
    model.ods <- glm({{formula}}, {{family}}, odsF,maxit = 100)
    model.sds <- glm({{formula}}, {{family}}, odsK,maxit = 100)
    obser <- cbind(model.ods$coefficients,summary(model.ods)$coefficients[,2])
    synth <- summary(model.sds)$coefficients[,c(1:2)]
    
    # Clean up so that the synth and obser has the same coefficients.
    synthTmp <- matrix(NA,nrow = nrow(obser),ncol = 2,
                       dimnames = list(rownames(obser), c("obs.coef","obs.se")))
    insO <- match(rownames(synth),rownames(synthTmp))
    for(i in c(1:length(insO))){
      idx <- insO[i]
      if(!is.na(idx)){
        synthTmp[idx,] <- synth[i,]
      }
    }
    
    synth <- synthTmp
    rm(synthTmp)
    
    rm(model.sds)
    rm(model.ods)
    gc()
    topList <- list()
    
    # c1CI is the CI overlap while ReDa1 contains the original coefficient and variance, k-anonymized coefficient and variance and the CI overlap.
    
    c1CI <- compare.CI.T(synth,obser,ci.level = 0.95, intercept = TRUE)
    numVar <- nrow(synth); rNames <- rownames(synth)
    ReDa1 <- matrix(NA, nrow = numVar, ncol = 5, 
                    dimnames = list(rNames, c("obs.coef","obs.se","syn.coef","syn.se","CI overlap")))
    ReDa1[,1:2] <- obser[,1:2]
    ReDa1[,3:4] <- synth[,1:2]
    ReDa1[,5] <- c1CI[,1]
    
    glmList <- c(glmList,list(ReDa1))
  }
  message("DONE",appendLF = TRUE)
  return(glmList)
}

# Method for making regression fits while also transforming the numerical variables into categorical ones.
# sds is an object that contatins the synthetic dataset(s).
# odsF is the original dataset with a matching transformation.
# mSet is an integer vector and is used for how many datasets should be combined as released datasets.
# k is the number of different samples made of the regression fit results for each number of released datasets in mSet.
# formulas is a formula vector that has the formulas for each regression fit.
# families is a character vector for each family function used for the formula of the matching index.
# transFM is a nx2 matrix that says how the (n number of) numerical variables should be transformed.
#  First column is the variable index while the second says how many cuts should be made.
# return is a list of the results for all the formulas tested.
testGLMK <- function(sds,odsF,mSet,k,formulas,families,transFm){
  for(i in c(1:sds$m)){
    sds$syn[[i]] <- createFac(sds$syn[[i]],odsF,transFm,colnames(odsF)) # Create extra categorical varibales that are transformations of the numerical variables.
  }
  nMethod <- c(sds$method,rep("",nrow(transFm)))
  names(nMethod) <- colnames(odsF)
  sds$method <- nMethod # Add new categorical variables to the object.
  return(TestGLM(sds,odsF,mSet,k,formulas,families)) # Call the regular regression fit function with the transformed synthetic datasets and return results.
}

# Method to create new categorical variables that are transformations of numerical ones.
# sds is an object that contatins the synthetic dataset(s).
# odsF is the original dataset with a matching transformation.
# transFM is a nx2 matrix that says how the (n number of) numerical variables should be transformed.
#  First column is the variable index while the second says how many cuts should be made.
# cname are the names that will be used for the original variables + new categorical variables.
# return is the synthetic dataset expanded with the new categorical variables.
createFac <- function(sds,odsF,transFm,cname){
  cn <- ncol(sds)
  for(j in c(1:nrow(transFm))){
    
    # Make sure that the synthetic dataset has values that are within the bound of the original dataset.
    sdsC <- sds[,transFm[[j,1]]]
    sdsC[sdsC > max(odsF[,transFm[[j,1]]])] <- max(odsF[,transFm[[j,1]]])
    sdsC[sdsC < min(odsF[,transFm[[j,1]]])] <- min(odsF[,transFm[[j,1]]])
    
    newCol <- cut(sdsC,quantile(odsF[,transFm[[j,1]]],probs = seq(0,1,1/transFm[[j,2]]),names = FALSE)
                  - c(1,rep(0,transFm[[j,2]]))) # Change so that the cut is [.] instead of (.]
    
    # If one or more levels have zero entries then the level with the largest number of members will have some of
    #  those members changed into the ones without so that each level has a minimum of one entry.
    # This is because it is possible that some of the synthetic datasets will have such a dissimilar
    #  distribution so that no numerical values fall within the range of some level. Levels with zero entries will cause issues later.
    spredCol <- summary(newCol)
    spredColZeroes <- names(spredCol)[spredCol == 0]
    spredColLarge <- names(spredCol)[[which.max(spredCol)]]
    for(n in spredColZeroes){
      idx <- match(spredColLarge,newCol)
      newCol[[idx]] <- n
    }
    sds[,cn+j] <- newCol
  }
  colnames(sds) <- cname
  return(sds)
}
