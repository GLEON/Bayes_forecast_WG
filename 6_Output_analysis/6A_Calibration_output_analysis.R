# Title: 6A_Calibration_output_analysis
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

#set a directory to use as a local file repository for plots if desire to write to file
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Bayes_model_calibration_output"

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...)
}
########################CALCULATE ASSESSMENT METRICS#####################################

  #read in observed data
  obs_log <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))
  obs_log <- obs_log[1:6,]

  obs_not_log <- exp(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")))-0.0035
  obs_not_log <- obs_not_log[1:6,]

  #set up matrix for model assessment metrics
  hoa <- matrix(NA,length(model_names),11)

  for (i in 1:length(model_names)){

  hoa[i,1] <- model_names[i]

    #source helper functions for assessment metrics
    source('0_Function_library/output_analysis_assessment_metrics.R')

    #read in appropriate calibration summary files
    pred0 <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.1_Calibration/",paste0(model_names[i],'_predicted_states.csv'),sep = ""))))
    mus=c(grep("mu\\[1,", colnames(pred0)),grep("mu\\[2,", colnames(pred0)),
          grep("mu\\[3,", colnames(pred0)),grep("mu\\[4,", colnames(pred0)),
          grep("mu\\[5,", colnames(pred0)),grep("mu\\[6,", colnames(pred0)))
    pred = pred0[,mus]

    #plot timeseries of pred CI and obs
    ci <- apply(pred,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    times <- c(1:120)
    plot_obs <- c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,])

    png(file=file.path(my_directory,paste(paste0(model_names[i],'_CI_and_obs.png'), sep = '_')), res=300, width=10, height=5, units='in')
    plot(times,ci[2,],type='n', ylab="log Gloeo", ylim = c(min(ci[1,], na.rm = TRUE)-0.2,max(ci[3,], na.rm = TRUE)+0.2),
         main="",xlab = "sampling times")
    ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
    points(times,plot_obs)
    dev.off()

    #1. RMSE of log(totalperL)
    mean_pred_log <- colMeans(pred)
    RMSE <- rmse(mean_pred_log, c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,]))
    hoa[i,2] <- round(RMSE,2)

    #2. predictive variance of log(totalperL)
    pred_var <- mean(apply(pred,2,var))
    hoa[i,3] <- round(pred_var,2)

    #3. coverage (% of values falling within 95% predictive interval)
    cov <- coverage(pred_dist = pred, obs = c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,]))
    hoa[i,4] <- cov

    #4. peak timing metric (when did model predict the peak vs. when it occurred)
    #reported as difference in weeks, where -1 means model predicted 1 week early
    #and 1 means model predicted 1 week late
    pt <- peak_timing(pred = mean_pred_log, obs = c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,]))
    hoa[i,5] <- pt

    #5. Mean quantile of observations in distribution of predictions
    mean_quant <- mean_quantile(pred_dist = pred, obs = c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,]))
    hoa[i,6] <- round(mean_quant,2)

    #6. Quantile of 2015 max. density in predictive interval
    max_quant <- max_quantile(pred_dist = pred, obs = c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,]))
    hoa[i,7] <- round(max_quant,2)

    #7. Pearson's r btwn predicted and observed in log space
    corr <- cor(mean_pred_log, c(obs_log[1,],obs_log[2,],obs_log[3,],obs_log[4,],obs_log[5,],obs_log[6,]), method = "pearson", use = "complete.obs")
    hoa[i,8] <- round(corr,2)

    #8. **Mean diff. in predicted-observed in total per L
    bi <- bias(pred_dist = exp(pred), obs = c(obs_not_log[1,],obs_not_log[2,],obs_not_log[3,],obs_not_log[4,],obs_not_log[5,],obs_not_log[6,]))
    hoa[i,9] <- round(bi,2)

    #9. **Bias in predictions during highest density point in 2015
    max_bi <- max_bias(pred_dist = exp(pred), obs = c(obs_not_log[1,],obs_not_log[2,],obs_not_log[3,],obs_not_log[4,],obs_not_log[5,],obs_not_log[6,]))
    hoa[i,10] <- round(max_bi,2)

    #10. **Mean range of 95% predictive interval in total per L
    mr <- mean_range(pred_dist = exp(pred))
    hoa[i,11] <- round(mr,2)

}
    #set column names for matrix and write to file
    hoa <- data.frame(hoa)
    colnames(hoa) <- c("model_name","RMSE","pred_var","coverage","peak_timing","mean_quantile",
                       "max_quantile","Pearsons_r","mean_bias","max_bias","mean_range")
    write.csv(hoa,file=file.path(paste("./6_Output_analysis/",paste0('calibration_output_analysis.csv'),sep = "")),row.names = FALSE)



