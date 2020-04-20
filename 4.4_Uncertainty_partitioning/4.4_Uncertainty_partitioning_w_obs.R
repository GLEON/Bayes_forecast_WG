# Title: 4.4_Uncertainty_partitioning_w_obs (observation error)
# History:
# adapted by JAZ from from the EcoForecast Activity
# by Michael Dietze, with reference "Ecological Forecasting", chapter 11
# updated and expanded by MEL

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","GDD_test","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")
yrs <- c(2015:2016)
wks <- c(1:20)
forecast_weeks <- c(1:4)
N_weeks <- matrix(NA,2,20)
N_weeks[1,] <- c(1:20)
N_weeks[2,] <- c(21:40)

###########################UNCERTAINTY PARTITIONING###################################

for (i in 1:length(model_names)){

  for (n in 1:length(forecast_weeks)){

    #create empty matrices to populate from hindcast variance for each week
    vardat.det <- matrix(NA, 5000, 40)
    vardat.det <- data.frame(vardat.det)

    vardat.IC <- matrix(NA, 5000, 40)
    vardat.IC <- data.frame(vardat.IC)

    vardat.IC.P <- matrix(NA, 5000, 40)
    vardat.IC.P <- data.frame(vardat.IC.P)

    vardat.IC.P.Pa <- matrix(NA, 5000, 40)
    vardat.IC.P.Pa <- data.frame(vardat.IC.P.Pa)

    vardat.IC.P.Pa.D <- matrix(NA, 5000, 40)
    vardat.IC.P.Pa.D <- data.frame(vardat.IC.P.Pa.D)

    vardat.w_obs <- matrix(NA, 5000, 40)
    vardat.w_obs <- data.frame(vardat.w_obs)

    for (j in 1:length(yrs)){

      for (k in 1:length(wks)){

      #read in hindcast files from each week and append to matrix
      dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_det.prediction_',yrs[j],'_',wks[k],'.csv'))))
      vardat.det[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]

      dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC_',yrs[j],'_',wks[k],'.csv'))))
      vardat.IC[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]

      dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.P_',yrs[j],'_',wks[k],'.csv'))))
      vardat.IC.P[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]

      if(!model_names[i] %in% c("RW","RW_obs")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.P.Pa_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.P.Pa[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      if(!model_names[i] %in% c("RW","RW_obs","AR")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.P.Pa.D_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.P.Pa.D[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.w_obs_',yrs[j],'_',wks[k],'.csv'))))
      vardat.w_obs[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]
      }

      }

    #write variance matrices to .csv
    write.csv(vardat.w_obs,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.w_obs_',forecast_weeks[n],'.csv'))),row.names = FALSE)

    #calculate relative uncertainty contributions and write to file
    source('0_Function_library/uncertainty_partitioning_make_varmat_w_obs.R')

    varMat   <- make_varMat(model_name = model_names[i])
    write.csv(varMat,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varMat.w_obs',forecast_weeks[n],'.csv'))),row.names = FALSE)

    varRelative <- apply(varMat,2,function(x) {x/max(x)})
    write.csv(varRelative,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative.w_obs',forecast_weeks[n],'.csv'))),row.names = FALSE)

  }
}

##Hokie hokie hokie hi! You have partitioned uncertainty. Give someone a hug :-)
