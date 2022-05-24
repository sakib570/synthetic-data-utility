## Author Niklas Reje (nreje@kth.se)
library("synthpop")

# Method from which the different synthesizers are created in testArea script file.
# ods is the original dataset.
# method is what synthesizer method(s) is going to be used. Can either be a string if only one synthesizer 
#  or a character vector if multiple synthesizers are used.
# visitSeq is the imputation order.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# m is the number of released synthetic datasets.
# proper is a boolean for using proper or non-proper imputation.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
SynthMeth <- function(ods,method = "cart",visitSeq,predMat,m = 1,proper = FALSE){
  if(missing(visitSeq)){visitSeq = c(1:ncol(ods))}
  
  sds <- syn(data = ods, visit.sequence = visitSeq, method = method,
             m = m, proper = proper, predictor.matrix = predMat)
  
  return(sds)
}

# Because of a bug in synthpop or the underlying implementation of the Decision Trees it is possible that it crashes.
# Crashes has happened when using proper, when using selective imputation, when having a large number of records (>20000) and when combined with catall.
# The crashes seem to be semi-random however and it is therefor possible to generate datasets one at a time
#  until you have as many as you want while ignoring crashes.
# ods is the original dataset.
# method is what synthesizer method(s) is going to be used. Can either be a string if only one synthesizer 
#  or a character vector if multiple synthesizers are used.
# visitSeq is the imputation order.
# predMat is the predictor matrix which decides which variables are used for the imputation of each variable.
# mW is the number of released synthetic datasets wanted.
# proper is a boolean for using proper or non-proper imputation.
# sds is an object that contains the synthetic datasets together with some other information on the imputation process and the original dataset.
forceDec <- function(ods,method = "cart",visitSeq,predMat = NULL,mW = 10,proper = TRUE){
  message("Force Dec: ",appendLF = FALSE)
  if(missing(visitSeq)){visitSeq = c(1:ncol(ods))}
  mD <- 0 # Number of complete synthetic datasets
  sds <- syn(ods,method = method,m = 0,visit.sequence = visitSeq,predictor.matrix = predMat,
             print.flag = FALSE,proper = proper) #Used to create a synthetic dataset object.
  sds$m <- mW
  
  while(mD < mW){
    sdsD <- NULL
    tryCatch({
      sdsD <- syn(ods,method = method,visit.sequence = visitSeq,predictor.matrix = predMat,
                  print.flag = FALSE,proper = proper)
    }, error=function(e){sdsD <- NULL})
    if(!is.null(sdsD)){
      sds$syn[[mD+1]] <- sdsD$syn
      mD <- mD + 1
      message(paste0(mD,", "),appendLF = FALSE)
    }
  }
  message("DONE",appendLF = TRUE)
  return(sds)
}