# Title: 6A_Hindcast_output_analysis_w_obs (observation error)
# History:
# State-space model diagnostics - revised by S. LaDeau (11/2017) from the
# EcoForecast Activity by Michael Dietze, with reference to
# "Ecological Forecasting", chapter 8
# updates JAZ, WB, MEL for Lofton et al. 2020

#####################LIST OF METRICS WE ARE CALCULATING###############################
#1. RMSE of log(totalperL)
#2. Predictive variance of log(totalperL)
#3. Coverage (% of values falling within 95% predictive interval)
#4. Peak timing metric (when did model predict the peak vs. when it occurred)
#5. Mean quantile of observations in distribution of predictions
#6. Quantile of 2015 max. density in predictive interval
#7. Pearson's r btwn predicted and observed in log space
#8. **Mean diff. in predicted-observed in total per L
#9. **Bias in predictions during highest density point in 2015
#10. **Mean range of 95% predictive interval in total per L

##**metrics that are not in log space
#######################################################################################


##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","GDD_test","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")
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
  hoa <- matrix(NA,length(model_names),11)

  for (i in 1:length(model_names)){

  hoa[i,1] <- model_names[i]

    #source helper functions for assessment metrics
    source('0_Function_library/output_analysis_assessment_metrics.R')

    #read in appropriate hindcast summary files
    vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.w_obs_',forecast_weeks[n],'.csv')))))

    varMat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varMat.w_obs',forecast_weeks[n],'.csv')))))

    #subset vardat and varMat according to forecast week
    if(forecast_weeks[n] == 4){
      vardat <- vardat[,c(1:17,21:37)]
      varMat <- varMat[,c(1:17,21:37)]
    }
    if(forecast_weeks[n] == 3){
      vardat <- vardat[,c(1:18,21:38)]
      varMat <- varMat[,c(1:18,21:38)]
    }
    if(forecast_weeks[n] == 2){
      vardat <- vardat[,c(1:19,21:39)]
      varMat <- varMat[,c(1:19,21:39)]
    }

    #1. RMSE of log(totalperL)
    mean_pred_log <- colMeans(vardat)
    RMSE <- rmse(mean_pred_log, obs_log)
    hoa[i,2] <- round(RMSE,2)

    #2. predictive variance of log(totalperL)
    pred_var <- mean(varMat[nrow(varMat),])
    hoa[i,3] <- round(pred_var,2)

    #3. coverage (% of values falling within 95% predictive interval)
    cov <- coverage(pred_dist = vardat, obs = c(obs_log[1,],obs_log[2,]))
    hoa[i,4] <- cov

    #4. peak timing metric (when did model predict the peak vs. when it occurred)
    #reported as difference in weeks, where -1 means model predicted 1 week early
    #and 1 means model predicted 1 week late
    pt <- peak_timing(pred = mean_pred_log, obs = c(obs_log[1,],obs_log[2,]))
    hoa[i,5] <- pt

    #5. Mean quantile of observations in distribution of predictions
    mean_quant <- mean_quantile(pred_dist = vardat, obs = c(obs_log[1,],obs_log[2,]))
    hoa[i,6] <- round(mean_quant,2)

    #6. Quantile of 2015 max. density in predictive interval
    max_quant <- max_quantile(pred_dist = vardat, obs = c(obs_log[1,],obs_log[2,]))
    hoa[i,7] <- round(max_quant,2)

    #7. Pearson's r btwn predicted and observed in log space
    corr <- cor(mean_pred_log, c(obs_log[1,],obs_log[2,]), method = "pearson", use = "complete.obs")
    hoa[i,8] <- round(corr,2)

    #8. **Mean diff. in predicted-observed in total per L
    bi <- bias(pred_dist = exp(vardat), obs = c(obs_not_log[1,],obs_not_log[2,]))
    hoa[i,9] <- round(bi,2)

    #9. **Bias in predictions during highest density point in 2015
    max_bi <- max_bias(pred_dist = exp(vardat), obs = c(obs_not_log[1,],obs_not_log[2,]))
    hoa[i,10] <- round(max_bi,2)

    #10. **Mean range of 95% predictive interval in total per L
    mr <- mean_range(pred_dist = exp(vardat))
    hoa[i,11] <- round(mr,2)

}
    #set column names for matrix and write to file
    hoa <- data.frame(hoa)
    colnames(hoa) <- c("model_name","RMSE","pred_var","coverage","peak_timing","mean_quantile",
                       "max_quantile","Pearsons_r","mean_bias","max_bias","mean_range")
    write.csv(hoa,file=file.path(paste("./6_Output_analysis/",paste0('hindcast_output_analysis_w_obs_wk_',forecast_weeks[n],'.csv'),sep = "")),row.names = FALSE)
}


###############LOOK AT OUTPUT ANALYSIS#######################################
wk1 <- read_csv("./6_Output_analysis/hindcast_output_analysis_w_obs_wk_1.csv")
wk2 <- read_csv("./6_Output_analysis/hindcast_output_analysis_w_obs_wk_2.csv")
wk3 <- read_csv("./6_Output_analysis/hindcast_output_analysis_w_obs_wk_3.csv")
wk4 <- read_csv("./6_Output_analysis/hindcast_output_analysis_w_obs_wk_4.csv")

