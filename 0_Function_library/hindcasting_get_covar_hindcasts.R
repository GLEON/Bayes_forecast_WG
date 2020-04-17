# Title: Create hindcasted covariates for various forms of hindcast
# History:
# created MEL 17APR20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

#function
get_covar_hindcasts <- function(forecast_type, wk, yrsamp, Nmc, covar_ensemble){

  #make vector of columns corresponding to week of hindcast
  hindcast_wks <- c(wk,wk+1,wk+2,wk+3)
  #set up output matrices
  covar <- matrix(NA, Nmc, 4)

  if(forecast_type %in% c("det","IC","IC.P","IC.P.Pa")){
    for(i in 1:length(hindcast_wks)){
    covar[,i] <- mean(covar_ensemble[yrsamp,hindcast_wks[i]], na.rm = TRUE)

    }
  } else {
    for(i in 1:length(hindcast_wks)){
      covar[,i] <- covar_ensemble[yrsamp,hindcast_wks[i]]

    }
  }

  return(covar)
}


