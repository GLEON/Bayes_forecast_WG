# Title: 4.4_Uncertainty_partitioning
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
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","schmidt_max_lag","precip","GDD","schmidt_and_precip","precip_and_GDD","schmidt_and_temp","temp_and_precip")
yrs <- c(2015:2016)
wks <- c(1:20)
forecast_weeks <- c(1:4)
N_weeks <- matrix(NA,2,20)
N_weeks[1,] <- c(1:20)
N_weeks[2,] <- c(21:40)

###########################UNCERTAINTY PARTITIONING###################################

for (i in 1:length(model_names)){

  for (n in 1:length(forecast_weeks)){

    #create empty matrices to populate with hindcast variance for each week

    #deterministic hindcasts (no uncertainty)
    vardat.det <- matrix(NA, 7500, 40)
    vardat.det <- data.frame(vardat.det)

    #initial conditions uncertainty only
    vardat.IC <- matrix(NA, 7500, 40)
    vardat.IC <- data.frame(vardat.IC)

    #initial conditions and parameter uncertainty
    vardat.IC.Pa <- matrix(NA, 7500, 40)
    vardat.IC.Pa <- data.frame(vardat.IC.Pa)

    #initial conditions, parameter, driver
    vardat.IC.Pa.D <- matrix(NA, 7500, 40)
    vardat.IC.Pa.D <- data.frame(vardat.IC.Pa.D)

    #initial conditions, parameter, driver, process
    vardat.IC.Pa.D.P <- matrix(NA, 7500, 40)
    vardat.IC.Pa.D.P <- data.frame(vardat.IC.Pa.D.P)

    #initial conditions and process (for models with no parameters/drivers)
    vardat.IC.P <- matrix(NA, 7500, 40)
    vardat.IC.P <- data.frame(vardat.IC.P)

    #initial conditions, parameter, process (for AR model with no drivers)
    vardat.IC.Pa.P <- matrix(NA, 7500, 40)
    vardat.IC.Pa.P <- data.frame(vardat.IC.Pa.P)

    for (j in 1:length(yrs)){

      for (k in 1:length(wks)){

      #read in hindcast files from each week and append to matrix
      dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_det.prediction_',yrs[j],'_',wks[k],'.csv'))))
      vardat.det[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]

      dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC_',yrs[j],'_',wks[k],'.csv'))))
      vardat.IC[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]

      if(model_names[i] %in% c("RW","RW_obs")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.P_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.P[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      if(!model_names[i] %in% c("RW","RW_obs")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.Pa_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.Pa[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      if(model_names[i] == "AR"){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.Pa.P_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.Pa.P[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      if(!model_names[i] %in% c("RW","RW_obs","AR")){
        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.Pa.D_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.Pa.D[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]

        dat <- read_csv(file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_names[i],'_hindcast.IC.Pa.D.P_',yrs[j],'_',wks[k],'.csv'))))
        vardat.IC.Pa.D.P[,N_weeks[j,k]] <- dat[,forecast_weeks[n]]}

      }}

    #write variance matrices to .csv
    write.csv(vardat.det,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.det_',forecast_weeks[n],'.csv'))),row.names = FALSE)
    write.csv(vardat.IC,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC_',forecast_weeks[n],'.csv'))),row.names = FALSE)
    if(model_names[i] %in% c("RW","RW_obs")){
      write.csv(vardat.IC.P,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.P_',forecast_weeks[n],'.csv'))),row.names = FALSE)}
    if(!model_names[i] %in% c("RW","RW_obs")){
      write.csv(vardat.IC.Pa,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa_',forecast_weeks[n],'.csv'))),row.names = FALSE)}
    if(model_names[i] == "AR"){
      write.csv(vardat.IC.Pa.P,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.P_',forecast_weeks[n],'.csv'))),row.names = FALSE)}
    if(!model_names[i] %in% c("RW","RW_obs","AR")){
      write.csv(vardat.IC.Pa.D,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.D_',forecast_weeks[n],'.csv'))),row.names = FALSE)
      write.csv(vardat.IC.Pa.D.P,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.D.P_',forecast_weeks[n],'.csv'))),row.names = FALSE)}

    #calculate relative uncertainty contributions and write to file
    source('0_Function_library/uncertainty_partitioning_make_varmat.R')

    varMat <- make_varMat(model_name = model_names[i])
    write.csv(varMat,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varMat_',forecast_weeks[n],'.csv'))),row.names = FALSE)

    varRelative <- apply(varMat,2,function(x) {x/max(x)})
    write.csv(varRelative,file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv'))),row.names = FALSE)

  }
}

##Hokie hokie hokie hi! You have partitioned uncertainty. Give someone a hug :-)
