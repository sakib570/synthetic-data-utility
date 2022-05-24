# Nethods imported from other sources
###-----dfCI---------------------------------------------------------------
# extract info for plotting confidence intervals
dfCI <- function(modelsummary, names.est.se = c("Estimate","Std. Error"),
                 model.name = "observed",  ci.level = 0.95, Z = FALSE, 
                 name.Z = colnames(modelsummary$coefficients)[3]){
  
  CI <- qnorm(1 - (1 - ci.level)/2)
  if (!Z) {
    msCI <- as.data.frame(modelsummary$coefficients[,1:2]) 
    names(msCI) <- c("Value", "SE")
  } else {
    msCI <- as.data.frame(modelsummary$coefficients[,3]) 
    names(msCI) <- c("Value")
    msCI$SE <- 1
  }  
  msCI$Coefficient <- rownames(msCI)
  
  msCI$HighCI <- msCI$Value + CI*msCI$SE
  msCI$LowCI  <- msCI$Value - CI*msCI$SE
  msCI$SE     <- NULL
  msCI$Model  <- model.name
  msCI$Coefficient <- factor(msCI$Coefficient, levels = rev(msCI$Coefficient))  #!BN290416, rev added 
  
  return(msCI)
}

###-----compare.CI---------------------------------------------------------
compare.CI.T <- function(synthetic, observed, ci.level, intercept, ...){
  CI <- qnorm(1- (1 - ci.level)/2)
  ##Initiate
  if(nrow(observed) > nrow(synthetic)){
    numVar <- nrow(synthetic); rNames <- rownames(synthetic)
  } else{
    numVar <- nrow(observed); rNames <- rownames(observed)
  } 
  CIoverlap <- matrix(NA, nrow = numVar, ncol = 1, 
                      dimnames = list(rNames, "CI overlap"))
  
  ##Calculate CIoverlap
  for(i in 1:numVar) {
    
    ##Store CIs
    syn.upper <- synthetic[i, 1] + (CI * synthetic[i, 2])
    syn.lower <- synthetic[i, 1] - (CI * synthetic[i, 2])
    obs.upper <- observed[i, 1] + (CI * observed[i, 2])
    obs.lower <- observed[i, 1] - (CI * observed[i, 2])
    
    ## CI overlap
    overlap.lower <- max(obs.lower, syn.lower)       
    overlap.upper <- min(obs.upper, syn.upper)       
    CIoverlap[i, 1] <- 0.5 * 
      (((overlap.upper - overlap.lower) / (obs.upper - obs.lower)) + 
         ((overlap.upper - overlap.lower) / (syn.upper - syn.lower)))
  }

  if(intercept == FALSE) {
    CIoverlap = CIoverlap[-1, , drop = FALSE]
  }
  
  return(as.data.frame(CIoverlap))
}

###-----estimator-------
estimator <- function(db, incomplete = FALSE, population.inference = FALSE, proper = FALSE){
  m <- db$m
  n <- db$n
  k <- db$k
  ## Checks and warnings for incomplete method
  #---
  if (incomplete == TRUE) {
    if (population.inference == TRUE & m == 1) {
      stop("You have selected population inference using a method for incompletely synthesised data with\n m = 1 - standard errors cannot be calculated.\n", call. = FALSE)
    }
  }
  #--- 
  
  ## Inference to Q hat
  #---
  if (population.inference == FALSE) { 
    result <- cbind(db$coef,
                    sqrt(db$vars*k/n),
                    db$coef/sqrt(db$vars*k/n))
    colnames(result) <- c("xpct(Beta)", "xpct(se.Beta)", "xpct(z)")
    #--- 
    
    ## Population inference to Q
    #---   
  } else { 
    ## incomplete method  
    if (incomplete == TRUE) {
      result <- cbind(db$coef,
                      sqrt(db$bm/m + db$vars*k/n),
                      db$coef/sqrt(db$bm/m + db$vars*k/n))
      
      ## simple synthesis   
    } else {
      if (proper == FALSE) Tf <- db$vars*(k/n + 1/m) else Tf <- db$vars*(k/n + (1 + k/n)/m)
      result <- cbind(db$coef, 
                      sqrt(Tf), 
                      db$coef/sqrt(Tf)) 
    }
    colnames(result) <- c("Beta.syn","se.Beta.syn","z.syn")
  }
  #---
  return(result)
}