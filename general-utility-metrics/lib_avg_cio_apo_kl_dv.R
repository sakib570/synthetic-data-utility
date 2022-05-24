## Author Niklas Reje (nreje@kth.se)

# Method to compare regression fit and mean CI overlaps for different synthesizers.
# vari is to choose between the different variance estimator, 1 is mean variance, 2 is T_p and 3 is T_s.
# glm is a character vector with the names of the regression fit or mean results objects.
# te is a character vector with the names of the synthesizers.
# CI is set to a number [0.1) to calculate the average percentage of CI overlap above that number.
# est is set to FALSE if this is to compare results for regression fits or TRUE if it is mean results.
# return is a list with list with different transformations of the results.
#  First is a list of list of average result with first split over synthesizer and then m.
#  Second is a list of different transforms of the k results (Max, average, 95 and 5 percentile) over m.
#  Third is a list of different transforms of the k results (Max, average, 95 and 5 percentile) over the synthesizers.
#  Fourth is a list of different transforms of the k results (Max, max index, average, 95 and 5 percentile).
avgModelsCIPerSamp <- function(vari,glm,te,CI = NULL,est = FALSE){
  if(est == FALSE){
    mMax <- sapply(glm,function(x) length(eval(as.name(x))[[1]])) # Get the number of m since it could vary for different synthesizers.
  }
  else{
    mMax <- sapply(glm,function(x) length(eval(as.name(x)))) # Get the number of m since it could vary for different synthesizers.
  }
  
  message("START: ",appendLF = FALSE)
  #mSet <- c(1:max(mMax))
  #sM <- 1
  #if(vari == 3){ # The third variance estimator can't work with only one synthetic dataset.
  #  mSet <- c(2:max(mMax))
  #  sM <- 2
  #}
  
  if(est == FALSE){
    nm <- length(eval(as.name(glm[[1]]))) - 1 # To exclude the extra info on which synthetic dataset was used in each combination.
    cn <- c(1:nm)
  }
  else{
    nm <- nrow(eval(as.name(glm[[1]]))[[1]][[1]][[1]])
    cn <- rownames(eval(as.name(glm[[1]]))[[1]][[1]][[1]])
  }
  
  # List of list of average result with first split over synthesizer and then m.
  topList <- list()
  for(i in c(1:length(te))){
    message(paste0(i,", "),appendLF = FALSE)
    if(est == FALSE) k <- length(eval(as.name(glm[[i]]))[[1]][[1]])
    else k <- length(eval(as.name(glm[[i]]))[[1]])
    
    mList <- list()
    for(m in c(1:mMax[[i]])){
      if(est == FALSE) col <- lapply(eval(as.name(glm[[i]]))[1:nm],function(x) x[[m]])
      else{
        col <- NULL
        for(n in c(1:nm)){ # Separate the mean results to better match the structure of the regression fit results.
          colT <- lapply(eval(as.name(glm[[i]]))[[m]],function(x) lapply(x,function(y) matrix(y[n,],1,5,dimnames = list(cn[n],colnames(y))) ))
          col <- c(col,list(colT))
        }
      }
      
      if(length(col[[1]][[1]]) < vari) next # Skip if variance estimator has not been used here.
      
      
      # Holds results for all the k combinations and nm regression fits or variables mean tested.
      glmBoxCI <- matrix(NA, nrow = k, ncol = nm, 
                         dimnames = list(c(1:k), cn))
      
      # Used to remove outliers. Check that the variance is not zero and that the coefficients are not larger than 10^10 for synthetic.
      #  Also changes any negative CI overlaps into zeros.
      rInc <- function(base){
        nrm <- ( base[,4] == 0 | abs(base[,3]) > 10**10 )
        base[nrm,5] <- NA
        zrm <- base[,5] < 0
        base[zrm,5] <- 0
        return(base)
      }
      
      for(j in c(1:length(col))){
        if(is.null(CI)){ # Calculate average CI overlaps.
          if(est == FALSE) glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) mean(rInc(x[[vari]])[,5],na.rm = TRUE)))
          else glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) rInc(x[[vari]])[,5]))
        }
        else{ # Calculate average percentage of CI overlaps above CI
          if(est == FALSE) glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) sum(rInc(x[[vari]])[,5] > CI,na.rm = TRUE)/sum(!is.na(rInc(x[[vari]])[,5])) ))
          else glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) rInc(x[[vari]])[,5] > CI ))
        }
        
      }
      mList <- c(mList,list(glmBoxCI))
    }
    topList <- c(topList,list(mList))
  }
  message("DONE",appendLF = TRUE)
  
  mMaxS <- unlist(lapply(topList,function(x) length(x)))
  mSet <- c(1:max(mMaxS))
  
  # List of different transforms of the k results (Max, average, 95 and 5 percentile) over m.
  topList2 <- list()
  
  for(m in mSet){
    # Holds results for all the synthesizers and nm regression fits or variables mean tested.
    glmBoxCI <- matrix(NA, nrow = length(te), ncol = nm, 
                       dimnames = list(te, c(1:nm)))
    glmBoxSe <- matrix(NA, nrow = length(te), ncol = nm, 
                       dimnames = list(te, c(1:nm)))
    glmBoxCI2 <- matrix(NA, nrow = length(te), ncol = nm, 
                        dimnames = list(te, c(1:nm)))
    glmBoxSe2 <- matrix(NA, nrow = length(te), ncol = nm, 
                        dimnames = list(te, c(1:nm)))
    for(i in c(1:length(te))){
      #if(m <= mMax[[i]] + 1 - sM){
      if(m <= length(topList[[i]]) ){
        glmBoxCI[i,] <- apply(topList[[i]][[m]],2,max,na.rm = TRUE) # Max
        glmBoxSe[i,] <- apply(topList[[i]][[m]],2,mean,na.rm = TRUE) # Average
        glmBoxCI2[i,] <- apply(topList[[i]][[m]],2,function(x) UDCI(x)$UP) # 95 percentile
        glmBoxSe2[i,] <- apply(topList[[i]][[m]],2,function(x) UDCI(x)$DW) # 5 percentile
      }
      
    }
    mList <- list(glmBoxCI,glmBoxSe,glmBoxCI2,glmBoxSe2)
    topList2 <- c(topList2,list(mList))
  }
  
  # List of different transforms of the k results (Max, average, 95 and 5 percentile) over the synthesizers.
  topList3 <- list()
  
  for(i in c(1:length(te))){
    # Holds results for all m and nm regression fits or variables mean tested.
    glmBoxCI <- matrix(NA, nrow = length(mSet), ncol = nm, 
                       dimnames = list(mSet, c(1:nm)))
    glmBoxSe <- matrix(NA, nrow = length(mSet), ncol = nm, 
                       dimnames = list(mSet, c(1:nm)))
    glmBoxCI2 <- matrix(NA, nrow = length(mSet), ncol = nm, 
                        dimnames = list(mSet, c(1:nm)))
    glmBoxSe2 <- matrix(NA, nrow = length(mSet), ncol = nm, 
                        dimnames = list(mSet, c(1:nm)))
    for(m in c(1:length(mSet))){
      glmBoxCI[m,] <- topList2[[m]][[1]][i,]
      glmBoxSe[m,] <- topList2[[m]][[2]][i,]
      glmBoxCI2[m,] <- topList2[[m]][[3]][i,]
      glmBoxSe2[m,] <- topList2[[m]][[4]][i,]
    }
    mList <- list(glmBoxCI,glmBoxSe,glmBoxCI2,glmBoxSe2)
    topList3 <- c(topList3,list(mList))
  }
  
  # Holds results for all m and synthesizers.
  glmBoxCI <- matrix(NA, nrow = length(mSet), ncol = length(te), 
                     dimnames = list(mSet, te))
  glmBoxCI2 <- glmBoxCI
  glmBoxCI3 <- glmBoxCI
  glmBoxCI4 <- glmBoxCI
  glmBoxCI5 <- glmBoxCI
  
  for(i in c(1:length(te))){
    #mSetS <- c(1:length(c(sM:mMax[[i]])))
    mSetS <- c(1:mMaxS[[i]])
    glmBoxCI[mSetS,i] <- unlist(lapply(topList[[i]],function(x) max(apply(x,1,mean,na.rm = TRUE)))) # Max
    glmBoxCI2[mSetS,i] <- unlist(lapply(topList[[i]],function(x) which.max(apply(x,1,mean,na.rm = TRUE)))) # Max index
    glmBoxCI3[mSetS,i] <- unlist(lapply(topList[[i]],function(x) mean(apply(x,1,mean,na.rm = TRUE)))) # Average
    glmBoxCI4[mSetS,i] <- unlist(lapply(topList[[i]],function(x) UDCI(apply(x,1,mean,na.rm = TRUE))$UP )) # 95 percentile
    glmBoxCI5[mSetS,i] <- unlist(lapply(topList[[i]],function(x) UDCI(apply(x,1,mean,na.rm = TRUE))$DW )) # 5 percentile
  }
  
  # List of different transforms of the k results (Max, max index, average, 95 and 5 percentile).
  topList4 <- list(glmBoxCI,glmBoxCI2,glmBoxCI3,glmBoxCI4,glmBoxCI5)
  
  return(list(topList,topList2,topList3,topList4))
}


# Method to compare regression fit and mean Coefficient difference for different synthesizers.
# glm is a character vector with the names of the regression fit or mean results objects.
# te is a character vector with the names of the synthesizers.
# CI is set to a number [0.1) to calculate the average percentage of normalized coefficient difference above that number.
# est is set to FALSE if this is to compare results for regression fits or TRUE if it is mean results.
# return is a list with list with different transformations of the results.
#  First is a list of list of average result with first split over synthesizer and then m.
#  Second is a list of different transforms of the k results (Max, average, 95 and 5 percentile) over m.
#  Third is a list of different transforms of the k results (Max, average, 95 and 5 percentile) over the synthesizers.
#  Fourth is a list of different transforms of the k results (Max, max index, average, 95 and 5 percentile).
avgModelsDiffPerSamp <- function(glm,te,CI = NULL,est = FALSE){
  if(est == FALSE){
    mMax <- sapply(glm,function(x) length(eval(as.name(x))[[1]])) # Get the number of m since it could vary for different synthesizers.
  }
  else{
    mMax <- sapply(glm,function(x) length(eval(as.name(x)))) # Get the number of m since it could vary for different synthesizers.
  }
  
  message("START: ",appendLF = FALSE)
  
  mSet <- c(1:max(mMax))
  
  if(est == FALSE){
    nm <- length(eval(as.name(glm[[1]]))) - 1 # To exclude the extra info on which synthetic dataset was used in each combination.
    cn <- c(1:nm)
  }
  else{
    nm <- nrow(eval(as.name(glm[[1]]))[[1]][[1]][[1]])
    cn <- rownames(eval(as.name(glm[[1]]))[[1]][[1]][[1]])
  }
  
  # List of list of average result with first split over synthesizer and then m.
  topList <- list()
  for(i in c(1:length(te))){
    if(est == FALSE) k <- length(eval(as.name(glm[[i]]))[[1]][[1]])
    else k <- length(eval(as.name(glm[[i]]))[[1]])
    message(paste0(i,", "),appendLF = FALSE)
    
    mList <- list()
    for(m in c(1:mMax[[i]])){
      if(est == FALSE) col <- lapply(eval(as.name(glm[[i]]))[1:nm],function(x) x[[m]])
      else{
        col <- NULL
        for(n in c(1:nm)){ # Separate the mean results to better match the structure of the regression fit results.
          colT <- lapply(eval(as.name(glm[[i]]))[[m]],function(x) lapply(x,function(y) matrix(y[n,],1,5,dimnames = list(cn[n],colnames(y))) ))
          col <- c(col,list(colT))
        }
      }
      
      # Holds results for all the k combinations and nm regression fits or variables mean tested.
      glmBoxCI <- matrix(NA, nrow = k, ncol = nm, 
                         dimnames = list(c(1:k), cn))
      
      # Used to remove outliers. Check that the variance is not zero and that the coefficients are not larger than 10^10 for synthetic.
      #  Also calculate normalized coefficient difference and return those.
      rInc <- function(base){
        nrm <- ( base[,4] == 0 | abs(base[,3]) > 10**10 )
        base[nrm,c(3,4)] <- NA
        pre <- abs((base[,3]-base[,1])/base[,1])
        pre[is.nan(pre)] <- NA
        return(pre)
      }
      
      for(j in c(1:length(col))){
        if(is.null(CI)){ # Calculate normalized coefficient difference.
          if(est == FALSE) glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) mean(rInc(x[[1]]),na.rm = TRUE)))
          else glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) rInc(x[[1]]) ))
        }
        else{ # Calculate average percentage of normalized coefficient difference below CI.
          if(est == FALSE) glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) sum(rInc(x[[1]]) < CI,na.rm = TRUE)/sum(!is.na(rInc(x[[1]]))) ))
          else glmBoxCI[,j] <- unlist(lapply(col[[j]],function(x) rInc(x[[1]]) < CI ))
        }
        
      }
      mList <- c(mList,list(glmBoxCI))
    }
    topList <- c(topList,list(mList))
  }
  message("DONE",appendLF = TRUE)
  
  # List of different transforms of the k results (Max, average, 95 and 5 percentile) over m.
  topList2 <- list()
  
  for(m in mSet){
    # Holds results for all the synthesizers and nm regression fits or variables mean tested.
    glmBoxCI <- matrix(NA, nrow = length(te), ncol = nm, 
                       dimnames = list(te, c(1:nm)))
    glmBoxSe <- matrix(NA, nrow = length(te), ncol = nm, 
                       dimnames = list(te, c(1:nm)))
    glmBoxCI2 <- matrix(NA, nrow = length(te), ncol = nm, 
                        dimnames = list(te, c(1:nm)))
    glmBoxSe2 <- matrix(NA, nrow = length(te), ncol = nm, 
                        dimnames = list(te, c(1:nm)))
    for(i in c(1:length(te))){
      if(m <= mMax[[i]]){
        if(is.null(CI)){
          glmBoxCI[i,] <- apply(topList[[i]][[m]],2,min,na.rm = TRUE) # Min because it is normalized coefficient difference.
        }
        else{
          glmBoxCI[i,] <- apply(topList[[i]][[m]],2,max,na.rm = TRUE) # Max because it is the average percentage of normalized coefficient difference below CI.
        }
        glmBoxSe[i,] <- apply(topList[[i]][[m]],2,mean,na.rm = TRUE) # Average
        glmBoxCI2[i,] <- apply(topList[[i]][[m]],2,function(x) UDCI(x)$DW) # 95 percentile
        glmBoxSe2[i,] <- apply(topList[[i]][[m]],2,function(x) UDCI(x)$UP) # 5 percentile
      }
    }
    mList <- list(glmBoxCI,glmBoxSe,glmBoxCI2,glmBoxSe2)
    topList2 <- c(topList2,list(mList))
  }
  
  # List of different transforms of the k results (Max, average, 95 and 5 percentile) over the synthesizers.
  topList3 <- list()
  
  for(i in c(1:length(te))){
    # Holds results for all m and nm regression fits or variables mean tested.
    glmBoxCI <- matrix(NA, nrow = length(mSet), ncol = nm, 
                       dimnames = list(mSet, c(1:nm)))
    glmBoxSe <- matrix(NA, nrow = length(mSet), ncol = nm, 
                       dimnames = list(mSet, c(1:nm)))
    glmBoxCI2 <- matrix(NA, nrow = length(mSet), ncol = nm, 
                        dimnames = list(mSet, c(1:nm)))
    glmBoxSe2 <- matrix(NA, nrow = length(mSet), ncol = nm, 
                        dimnames = list(mSet, c(1:nm)))
    for(m in mSet){
      glmBoxCI[m,] <- topList2[[m]][[1]][i,]
      glmBoxSe[m,] <- topList2[[m]][[2]][i,]
      glmBoxCI2[m,] <- topList2[[m]][[3]][i,]
      glmBoxSe2[m,] <- topList2[[m]][[4]][i,]
    }
    mList <- list(glmBoxCI,glmBoxSe,glmBoxCI2,glmBoxSe2)
    topList3 <- c(topList3,list(mList))
  }
  
  
  # Holds results for all m and synthesizers.
  glmBoxCI <- matrix(NA, nrow = length(mSet), ncol = length(te), 
                     dimnames = list(mSet, te))
  glmBoxCI2 <- glmBoxCI
  glmBoxCI3 <- glmBoxCI
  glmBoxCI4 <- glmBoxCI
  glmBoxCI5 <- glmBoxCI
  
  for(i in c(1:length(te))){
    mSetS <- c(1:mMax[[i]])
    if(is.null(CI)){
      glmBoxCI[mSetS,i] <- unlist(lapply(topList[[i]],function(x) min(apply(x,1,mean,na.rm = TRUE)))) # Min because it is normalized coefficient difference.
      glmBoxCI2[mSetS,i] <- unlist(lapply(topList[[i]],function(x) which.min(apply(x,1,mean,na.rm = TRUE)))) # Min index.
    }
    else{
      glmBoxCI[mSetS,i] <- unlist(lapply(topList[[i]],function(x) max(apply(x,1,mean,na.rm = TRUE)))) # Max because it is the average percentage of normalized coefficient difference below CI.
      glmBoxCI2[mSetS,i] <- unlist(lapply(topList[[i]],function(x) which.max(apply(x,1,mean,na.rm = TRUE)))) # Max index.
    }
    glmBoxCI3[mSetS,i] <- unlist(lapply(topList[[i]],function(x) mean(apply(x,1,mean,na.rm = TRUE)))) # Average
    glmBoxCI4[mSetS,i] <- unlist(lapply(topList[[i]],function(x) UDCI(apply(x,1,mean,na.rm = TRUE))$DW )) # 95 percentile
    glmBoxCI5[mSetS,i] <- unlist(lapply(topList[[i]],function(x) UDCI(apply(x,1,mean,na.rm = TRUE))$UP )) # 5 percentile
  }
  
  # List of different transforms of the k results (Max, max index, average, 95 and 5 percentile).
  topList4 <- list(glmBoxCI,glmBoxCI2,glmBoxCI3,glmBoxCI4,glmBoxCI5)
  
  return(list(topList,topList2,topList3,topList4))
}

# Method to calculate 95 and 5 percentile of a numerical vector.
# vec is a numerical vector.
# return a list with the 95 and the 5 percentile.
UDCI <- function(vec){
  CI <- NULL
  CI$UP <- quantile(vec,0.95,na.rm = TRUE)
  CI$DW <- quantile(vec,0.05,na.rm = TRUE)
  return(CI)
}

# Method to compare KL divergence for different synthesizers.
# kld is a character vector with the names of the KL divergence result matrices.
# te is a character vector with the names of the synthesizers.
# return is a list with a matrix with KL divergence score averages of the synthetic datasets.
avgModelsKLD <- function(kld,te){
  kn <- colnames(eval(as.name(kld[[1]])))
  glmBoxCI <- matrix(NA, nrow = length(kn), ncol = length(te), 
                     dimnames = list(kn, te))
  for(i in c(1:length(te))){
    #k <- length(eval(as.name(kld[[i]]))[[1]])
    #kd <- apply(matrix(unlist(lapply(eval(as.name(kld[[i]]))[[1]],function(x) x[1,])),length(kn),k),1,mean,na.rm = TRUE)
    k <- eval(as.name(kld[[i]]))
    kd <- apply(k,2,mean,na.rm = TRUE)
    glmBoxCI[,i] <- kd
  }
  return(list(glmBoxCI))
}

# Method to create a gplot2 plot object. Draws lines and points for each synthesizer.
# data is a matrix with data points where row is the number of synthetic datasets and column are the synthesizers.
# mSet is a numerical vector which hold how many synthetic dataset each row used.
# setting is a data.frame which tells the plot the order of the synthesizers, the color, the point and line shape.
# return is a plot.
createPlot <- function(data,mSet,setting){
  ms <- data.frame(mSet)
  colnames(ms) <- "m"
  df <- NULL
  for(i in c(1:ncol(data))){
    df.tmp <- data.frame(m = mSet,y = data[,i],Syn = setting[[i,1]] # m is the x-axis. data[,i] is the data for one synthesizer while syn is the name of that syntheiszer.
                         ,lntp = as.factor(setting[[i,3]]),shp = as.factor(setting[[i,4]])) #lntp is the line shape while shp is the point shape.
    df <- rbind(df,df.tmp)
  }
  plot <- ggplot(data = df,aes(x = m,y = y, colour = Syn,linetype = Syn,shape = Syn)) + #Set color, linetype and shape to syn so that the legend match.
    geom_line() + geom_point()
  plot <- plot + scale_x_log10(breaks=mSet) # Set x-axis to log. Can be changed!
  plot <- plot + scale_color_manual("Syn",values = as.character(setting[,2])) 
  plot <- plot + scale_linetype_manual("Syn",values = as.numeric(setting[,3]))
  plot <- plot + scale_shape_manual("Syn",values = as.numeric(setting[,4]))
  return(plot)
}


## Extra for Result

# All remT methods are used to combine the selective lists in different ways. This one is to (ironically) remove the selective ones. 
remT <- function(CIavg,synNa,est = FALSE,m = 5){
  newCI <- NULL
  for(s in CIavg){
    if(est == FALSE){
      ci <- eval(as.name(s))[[2]][[m]][[2]]
    }
    else{
      ci <- eval(as.name(s))[[1]][[m]][[1]]
    }
    cin <- nrow(ci)
    newCI <- rbind(newCI,ci[c(1:(cin/2)),])
  }
  rownames(newCI) <- synNa
  return(newCI)
}

# All remT methods are used to combine the selective lists in different ways. This one is to remove the proper synthesized ones.
remT2 <- function(CIavg,synNa,st){
  newCI <- NULL
  for(s in CIavg){
    ci <- eval(as.name(s))[[4]][[3]]
    cin <- ncol(ci)
    newCI <- cbind(newCI,ci[,seq(st,cin,2)])
  }
  colnames(newCI) <- synNa
  return(newCI)
}

# All remT methods are used to combine the selective lists in different ways. This one is to remove the selective ones.
remT3 <- function(CIavg,synNa){
  newCI <- NULL
  for(s in CIavg){
    ci <- eval(as.name(s))[[4]][[3]]
    cin <- ncol(ci)
    newCI <- cbind(newCI,ci[,c(1:(cin/2))])
  }
  colnames(newCI) <- synNa
  return(newCI)
}

# Used to create normalized difference of average value and 5 percentile.
NDCI <- function(CI,type){
  return(abs(CI[[3]] - CI[[type]])/CI[[3]] )
}

# Used to remove outliers for coefficient difference and create list to be used with NDCI.
AHFix <- function(DA,ss = c("S","P","D","PT","DT","PO","DO","POT","DOT","PV","DV","PVT","DVT","CP","CPT","CC","CCT")){
  sn <- length(ss)
  ADB <- matrix(NA, nrow = 8, ncol = sn, 
                dimnames = list(c(1:8), ss))
  ADBW <- matrix(NA, nrow = 8, ncol = sn, 
                 dimnames = list(c(1:8), ss))
  
  rInc <- function(base){
    nrm <- base > 10**8
    base[nrm] <- NA
    pre <- mean(base,na.rm = TRUE)
    return(pre)
  }
  
  for(m in c(1:8)){
    for(i in c(1:sn)){
      if(m <= length(DA[[i]])){
        act <- DA[[i]][[m]]
        actN <- apply(act,1,rInc)
        
        ADB[[m,i]] <- mean(actN)
        ADBW[[m,i]] <- UDCI(actN)$UP
      }
    }
  }
  return(list(c(12),ADBW,ADB))
}