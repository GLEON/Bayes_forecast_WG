# Title: 6A_Predictive_intervals (with observation error)
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
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","wnd_dir_2day_lag","GDD","schmidt_max_lag","wind_and_GDD","schmidt_and_wind","temp_and_wind")
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
    vardat.w_obs <- matrix(NA, 7500, 40)
    vardat.w_obs <- data.frame(vardat.w_obs)

    for (j in 1:length(yrs)){

      for (k in 1:length(wks)){

      #read in hindcast files from each week and append to matrix
      if(model_names[i] %in% c("RW","RW_obs")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.P.O_',yrs[j],'_',wks[k],'.csv'))))
        vardat.w_obs[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      if(model_names[i] %in% c("RW_bias","AC","base_DLM")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.Pa.P.O_',yrs[j],'_',wks[k],'.csv'))))
        vardat.w_obs[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      if(!model_names[i] %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.Pa.D.P.O_',yrs[j],'_',wks[k],'.csv'))))
        vardat.w_obs[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]
      }

      }
    }

    #write variance matrices to .csv
    write.csv(vardat.w_obs,file=file.path(paste("./6_Output_analysis/6.1_Predictive_intervals/",paste0(model_names[i],'_PI_',forecast_weeks[n],'.csv'))),row.names = FALSE)

  }
}

##Phew! You have calculated the predictive intervals. Have a nap.
