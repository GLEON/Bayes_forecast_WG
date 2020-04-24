# Title: 7_Model_ensemble_analysis
# History:
# created by MEL 19APR20

###########APPROACH FOR SIMPLE ENSEMBLE EXERCISE###############################
#1. Read in all vardat files for all models EXCEPT RW MODELS for each type of hindcast (IC, IC.P, etc.)
#2. Append rows of all vardat files and write ensemble vardat to file
#3. Calculate varmat from appended vardats and write to file
#4. Calculate relative varmat from varmat and write to file
#5. Calculate ensemble PI
#6. Calculate output analysis metrics on ensemble PI
#7. Calculate uncertainty partitioning analysis metrics from relative varmat
#8. Repeat steps 1-7 for each forecast week
################################################################################


##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

#setting up counters and vectors for for-loop
model_names <- c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_max_lag","precip","schmidt_and_wnd","schmidt_and_precip","wnd_and_precip","wnd_and_GDD")
forecast_weeks <- c(1:4)
hindcast_types <- c("IC","IC.Pa","IC.Pa.D","IC.Pa.D.P")


#1. Read in all vardat files for all models EXCEPT RW MODELS for each type of hindcast (IC, IC.P, etc.)
#2. Append rows of all vardat files and write ensemble vardat to file
for (n in 4:length(forecast_weeks)){

  for (j in 1:length(hindcast_types)){

    ensemble_vardat <- read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[1],'_vardat.',hindcast_types[j],'_',forecast_weeks[n],'.csv'))))

  for (i in 2:length(model_names)){

    #read in vardats for various hindcast types for each model
    vardat <- read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.',hindcast_types[j],'_',forecast_weeks[n],'.csv'))))
    ensemble_vardat <- rbind(ensemble_vardat, vardat)
  }

    #manually add in AR since has different vardat file names
    if(hindcast_types[j] == "IC"){
      AR_vardat <- read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0('AR_vardat.IC_',forecast_weeks[n],'.csv'))))
      ensemble_vardat <- rbind(ensemble_vardat, AR_vardat)
    }
    if(hindcast_types[j] == "IC.Pa"){
      AR_vardat <- read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0('AR_vardat.IC.Pa_',forecast_weeks[n],'.csv'))))
      ensemble_vardat <- rbind(ensemble_vardat, AR_vardat)
    }
    if(hindcast_types[j] == "IC.Pa.D.P"){
      AR_vardat <- read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0('AR_vardat.IC.Pa.P_',forecast_weeks[n],'.csv'))))
      ensemble_vardat <- rbind(ensemble_vardat, AR_vardat)
    }

    write.csv(ensemble_vardat,file=file.path(paste("./7_Model_ensemble/",paste0('ensemble.vardat.',hindcast_types[j],'_',forecast_weeks[n],'.csv'))),row.names = FALSE)

  }

}

rm(list = ls())

#3. Calculate varmat from appended vardats and write to file
#4. Calculate relative varmat from varmat and write to file
model_name = "ensemble"
forecast_weeks <- c(1:4)

for (n in 1:length(forecast_weeks)){

  vardat.IC <- vardat <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble.vardat.IC_',forecast_weeks[n],'.csv')))))
  vardat.IC.Pa <- vardat <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble.vardat.IC.Pa_',forecast_weeks[n],'.csv')))))
  vardat.IC.Pa.D <- vardat <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble.vardat.IC.Pa.D_',forecast_weeks[n],'.csv')))))
  vardat.IC.Pa.D.P <- vardat <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble.vardat.IC.Pa.D.P_',forecast_weeks[n],'.csv')))))

  #calculate relative uncertainty contributions and write to file
  source('0_Function_library/uncertainty_partitioning_make_varmat.R')

  varMat   <- make_varMat(model_name = model_name)
  write.csv(varMat,file=file.path(paste("./7_Model_ensemble/",paste0(model_name,'_varMat_',forecast_weeks[n],'.csv'))),row.names = FALSE)

  varRelative <- apply(varMat,2,function(x) {x/max(x)})
  write.csv(varRelative,file=file.path(paste("./7_Model_ensemble/",paste0(model_name,'_varRelative_',forecast_weeks[n],'.csv'))),row.names = FALSE)

}

rm(list = ls())

#5. Calculate ensemble PI

#setting up counters and vectors for for-loop
model_names <- c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_max_lag","precip","schmidt_and_wnd","schmidt_and_precip","wnd_and_precip","wnd_and_GDD")
forecast_weeks <- c(1:4)

for (n in 1:length(forecast_weeks)){

  ensemble_PI <- read_csv(file=file.path(paste("./6_Output_analysis/6.1_Predictive_intervals/",paste0(model_names[1],'_PI_',forecast_weeks[n],'.csv'))))

    for (i in 2:length(model_names)){

      #read in PIs for each model
      PI <- read_csv(file=file.path(paste("./6_Output_analysis/6.1_Predictive_intervals/",paste0(model_names[i],'_PI_',forecast_weeks[n],'.csv'))))
      ensemble_PI <- rbind(ensemble_PI, PI)
    }

    write.csv(ensemble_PI,file=file.path(paste("./7_Model_ensemble/",paste0('ensemble_PI_',forecast_weeks[n],'.csv'))),row.names = FALSE)

}

rm(list = ls())


#6. Calculate output analysis metrics on ensemble PI

########################CALCULATE ASSESSMENT METRICS#####################################
#setting up counters and vectors for for-loop
model_name <- "ensemble"
forecast_weeks <- c(1:4)

########################CALCULATE ASSESSMENT METRICS#####################################
for (n in 1:length(forecast_weeks)){

  #read in observed data
  obs_log <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))
  obs_log <- obs_log[7:8,]

  obs_not_log <- exp(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")))-0.0035
  obs_not_log <- obs_not_log[7:8,]

  #subset observed data according to forecast week
  if(forecast_weeks[n] == 4){
    obs_log <- obs_log[,c(4:20)]
    obs_not_log <- obs_not_log[,c(4:20)]
  }
  if(forecast_weeks[n] == 3){
    obs_log <- obs_log[,c(3:20)]
    obs_not_log <- obs_not_log[,c(3:20)]
  }
  if(forecast_weeks[n] == 2){
    obs_log <- obs_log[,c(2:20)]
    obs_not_log <- obs_not_log[,c(2:20)]
  }

  #set up matrix for model assessment metrics
  hoa <- matrix(NA,1,11)

    hoa[1,1] <- model_name

    #source helper functions for assessment metrics
    source('0_Function_library/output_analysis_assessment_metrics.R')

    #read in appropriate hindcast summary files
    vardat <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0(model_name,'_PI_',forecast_weeks[n],'.csv')))))

    #subset vardat and varMat according to forecast week
    if(forecast_weeks[n] == 4){
      vardat <- vardat[,c(1:17,21:37)]
    }
    if(forecast_weeks[n] == 3){
      vardat <- vardat[,c(1:18,21:38)]
    }
    if(forecast_weeks[n] == 2){
      vardat <- vardat[,c(1:19,21:39)]
    }

    #1. RMSE of log(totalperL)
    mean_pred_log <- colMeans(vardat)
    RMSE <- rmse(mean_pred_log, obs_log)
    hoa[1,2] <- round(RMSE,2)

    #2. predictive variance of log(totalperL)
    pred_var <- mean(apply(vardat,2,var))
    hoa[1,3] <- round(pred_var,2)

    #3. coverage (% of values falling within 95% predictive interval)
    cov <- coverage(pred_dist = vardat, obs = c(obs_log[1,],obs_log[2,]))
    hoa[1,4] <- cov

    #4. peak timing metric (when did model predict the peak vs. when it occurred)
    #reported as difference in weeks, where -1 means model predicted 1 week early
    #and 1 means model predicted 1 week late
    pt <- peak_timing(pred = mean_pred_log, obs = c(obs_log[1,],obs_log[2,]))
    hoa[1,5] <- pt

    #5. Mean quantile of observations in distribution of predictions
    mean_quant <- mean_quantile(pred_dist = vardat, obs = c(obs_log[1,],obs_log[2,]))
    hoa[1,6] <- round(mean_quant,2)

    #6. Quantile of 2015 max. density in predictive interval
    max_quant <- max_quantile(pred_dist = vardat, obs = c(obs_log[1,],obs_log[2,]))
    hoa[1,7] <- round(max_quant,2)

    #7. Pearson's r btwn predicted and observed in log space
    corr <- cor(mean_pred_log, c(obs_log[1,],obs_log[2,]), method = "pearson", use = "complete.obs")
    hoa[1,8] <- round(corr,2)

    #8. **Mean diff. in predicted-observed in total per L
    bi <- bias(pred_dist = exp(vardat), obs = c(obs_not_log[1,],obs_not_log[2,]))
    hoa[1,9] <- round(bi,2)

    #9. **Bias in predictions during highest density point in 2015
    max_bi <- max_bias(pred_dist = exp(vardat), obs = c(obs_not_log[1,],obs_not_log[2,]))
    hoa[1,10] <- round(max_bi,2)

    #10. **Mean range of 95% predictive interval in total per L
    mr <- mean_range(pred_dist = exp(vardat))
    hoa[1,11] <- round(mr,2)

  #set column names for matrix and write to file
  hoa <- data.frame(hoa)
  colnames(hoa) <- c("model_name","RMSE","pred_var","coverage","peak_timing","mean_quantile",
                     "max_quantile","Pearsons_r","mean_bias","max_bias","mean_range")
  write.csv(hoa,file=file.path(paste("./7_Model_ensemble/",paste0('hindcast_output_analysis_wk_',forecast_weeks[n],'.csv'),sep = "")),row.names = FALSE)
}

rm(list = ls())

###############LOOK AT OUTPUT ANALYSIS#######################################
wk1 <- read_csv("./7_Model_ensemble/hindcast_output_analysis_wk_1.csv")
wk2 <- read_csv("./7_Model_ensemble/hindcast_output_analysis_wk_2.csv")
wk3 <- read_csv("./7_Model_ensemble/hindcast_output_analysis_wk_3.csv")
wk4 <- read_csv("./7_Model_ensemble/hindcast_output_analysis_wk_4.csv")

rm(list = ls())




#7. Calculate uncertainty partitioning analysis metrics from relative varmat

#setting up counters and vectors for for-loop
forecast_weeks <- c(1:4)
model_name = "ensemble"

########################CALCULATE ASSESSMENT METRICS#####################################

for (n in 1:length(forecast_weeks)){

  #set up matrix for uncertainty partitioning assessment metrics
  upam <- matrix(NA,1,13)

    #read in variance matrices
    varRel <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0(model_name,'_varRelative_',forecast_weeks[n],'.csv')))))

    #summarize proportional contributions of variance not including observation error
    upam[1,1] <- model_name

    #IC
    upam[1,2] <- round(mean(varRel[1,], na.rm = TRUE),2)
    upam[1,3] <- round(min(varRel[1,], na.rm = TRUE),2)
    upam[1,4] <- round(max(varRel[1,], na.rm = TRUE),2)

    #Parameter
    upam[1,5] <- round(mean((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
    upam[1,6] <- round(min((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
    upam[1,7] <- round(max((varRel[2,] - varRel[1,]), na.rm = TRUE),2)

    #Driver
      upam[1,8] <- round(mean((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[1,9] <- round(min((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[1,10] <- round(max((varRel[3,] - varRel[2,]), na.rm = TRUE),2)

    #Process
      upam[1,11] <- round(mean((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
      upam[1,12] <- round(min((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
      upam[1,13] <- round(max((varRel[4,] - varRel[3,]), na.rm = TRUE),2)

  #build final data frame and write to file
  upam <- data.frame(upam)
  colnames(upam) <- c("model_name","mean.IC","min.IC","max.IC",
                      "mean.Pa","min.Pa","max.Pa",
                      "mean.D","min.D","max.D",
                      "mean.P","min.P","max.P")

  write.csv(upam,file=file.path(paste("./7_Model_ensemble/",paste0('uncertainty_partitioning_output_analysis_wk_',forecast_weeks[n],'.csv'),sep = "")),row.names = FALSE)

}

rm(list = ls())


##########################ASSESS UNCERTAINTY PARTITIONING OUTPUT#################
wk1 <- read_csv("./7_Model_ensemble/uncertainty_partitioning_output_analysis_wk_1.csv")
wk2 <- read_csv("./7_Model_ensemble/uncertainty_partitioning_output_analysis_wk_2.csv")
wk3 <- read_csv("./7_Model_ensemble/uncertainty_partitioning_output_analysis_wk_3.csv")
wk4 <- read_csv("./7_Model_ensemble/uncertainty_partitioning_output_analysis_wk_4.csv")
