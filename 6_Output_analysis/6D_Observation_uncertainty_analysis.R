#6D_Observation_uncertainty_analysis
#Author: MEL
#Date: 18MAY20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, lubridate)

#set local directory for writing plots
#my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Bayes_model_analysis_output/"
#my_directory <- "~/Documents/Gloeo Bayesian Modeling/R Output/Bayes_model_uncertainty_analysis_output/"

#setting up counters and vectors for for-loop
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_max_lag","wnd_dir_2day_lag","GDD","schmidt_and_wind","temp_and_wind","wind_and_GDD")
forecast_weeks <- c(1,4)

final <- matrix(NA,length(forecast_weeks),4)
ci_all <- matrix(NA,3,1)
pi_all <- matrix(NA,3,1)


for (n in 1:length(forecast_weeks)){
for (i in 1:length(model_names)){

#confidence intervals
if(model_names[i] %in% c("RW","RW_obs")){
  vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.P_',forecast_weeks[n],'.csv')))))}
else if(model_names[i] %in% c("RW_bias","AC","base_DLM")){
  vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.P_',forecast_weeks[n],'.csv')))))}
else{vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.D.P_',forecast_weeks[n],'.csv')))))}

#predictive intervals
PI <- as.matrix(read_csv(file=file.path(paste("./6_Output_analysis/6.1_Predictive_intervals/",paste0(model_names[i],'_PI_',forecast_weeks[n],'.csv')))))

#subset vardat and varMat according to forecast week
if(forecast_weeks[n] == 4){
  vardat2015 <- vardat[,1:17]
  vardat2016 <- vardat[,21:37]
  PI2015 <- PI[,1:17]
  PI2016 <- PI[,21:37]
}
if(forecast_weeks[n] == 3){
  vardat2015 <- vardat[,1:18]
  vardat2016 <- vardat[,21:38]
  PI2015 <- PI[,1:18]
  PI2016 <- PI[,21:38]
}
if(forecast_weeks[n] == 2){
  vardat2015 <- vardat[,1:19]
  vardat2016 <- vardat[,21:39]
  PI2015 <- PI[,1:19]
  PI2016 <- PI[,21:39]
}
if(forecast_weeks[n] == 1){
  vardat2015 <- vardat[,1:20]
  vardat2016 <- vardat[,21:40]
  PI2015 <- PI[,1:20]
  PI2016 <- PI[,21:40]
}

#calculate mean predicted values in log space
ci2015_log <- apply(vardat2015,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
ci2016_log <- apply(vardat2016,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
ci <- cbind(ci2015_log, ci2016_log)

pi2015_log <- apply(PI2015,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
pi2016_log <- apply(PI2016,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
pi <- cbind(pi2015_log, pi2016_log)

ci_all <- cbind(ci_all, ci)
pi_all <- cbind(pi_all, pi)

}

  ci_range <- ci_all[3,]-ci_all[1,]
  ci_mean_range <- mean(ci_range, na.rm = TRUE)
  ci_sd_range <- sd(ci_range, na.rm = TRUE)

  pi_range <- pi_all[3,]-pi_all[1,]
  pi_mean_range <- mean(pi_range, na.rm = TRUE)
  pi_sd_range <- sd(pi_range, na.rm = TRUE)

  final[n,1] <- ci_mean_range
  final[n,2] <- ci_sd_range
  final[n,3] <- pi_mean_range
  final[n,4] <- pi_sd_range

  }

final <- data.frame(final)
colnames(final) <- c("ci_mean_range","ci_sd_range","pi_mean_range","pi_sd_range")
final$forecast_week <- c(1,4)
