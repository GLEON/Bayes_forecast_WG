# Title: 6C_Uncertainty_partitioning_output_analysis
# History:
# created by MEL 19APR20

#####################LIST OF METRICS WE ARE CALCULATING###############################
#1. mean percent contribution of each uncertainty type by model and forecast week
#2. mean percent contribution of each uncertainty type across models for each forecast week
#3. range percent contribution of each uncertainty type by model and forecast week
#4. range percent contribution of each uncertainty type across models for each forecast week

#######################################################################################

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")
forecast_weeks <- c(1:4)

########################CALCULATE ASSESSMENT METRICS#####################################

for (n in 1:length(forecast_weeks)){

  #set up matrix for uncertainty partitioning assessment metrics
  upam <- matrix(NA,length(model_names)+1,28)

  for (i in 1:length(model_names)){

    #read in variance matrices
    varRel <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv')))))
    varRel_w_obs <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative.w_obs',forecast_weeks[n],'.csv')))))

    #summarize proportional contributions of variance not including observation error
    upam[i,1] <- NA

    #IC
    upam[i,2] <- round(mean(varRel[1,], na.rm = TRUE),2)
    upam[i,3] <- round(min(varRel[1,], na.rm = TRUE),2)
    upam[i,4] <- round(max(varRel[1,], na.rm = TRUE),2)

    #Process
    upam[i,5] <- round(mean((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
    upam[i,6] <- round(min((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
    upam[i,7] <- round(max((varRel[2,] - varRel[1,]), na.rm = TRUE),2)

    #Parameter
    if(model_names[i] %in% c("RW","RW_obs")){
      upam[i,8] <- NA
      upam[i,9] <- NA
      upam[i,10] <- NA
    } else {
      upam[i,8] <- round(mean((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[i,9] <- round(min((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[i,10] <- round(max((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
    }

    #Driver
    if(model_names[i] %in% c("RW","RW_obs","AR")){
      upam[i,11] <- NA
      upam[i,12] <- NA
      upam[i,13] <- NA
    } else {
      upam[i,11] <- round(mean((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
      upam[i,12] <- round(min((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
      upam[i,13] <- round(max((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
    }

    #summarize proportional contributions of variance INCLUDING observation error

    #IC
    upam[i,14] <- round(mean(varRel_w_obs[1,], na.rm = TRUE),2)
    upam[i,15] <- round(min(varRel_w_obs[1,], na.rm = TRUE),2)
    upam[i,16] <- round(max(varRel_w_obs[1,], na.rm = TRUE),2)

    #Process
    upam[i,17] <- round(mean((varRel_w_obs[2,] - varRel_w_obs[1,]), na.rm = TRUE),2)
    upam[i,18] <- round(min((varRel_w_obs[2,] - varRel_w_obs[1,]), na.rm = TRUE),2)
    upam[i,19] <- round(max((varRel_w_obs[2,] - varRel_w_obs[1,]), na.rm = TRUE),2)

    if(model_names[i] %in% c("RW","RW_obs")){
      upam[i,20] <- NA
      upam[i,21] <- NA
      upam[i,22] <- NA
    } else {
      upam[i,20] <- round(mean((varRel_w_obs[3,] - varRel_w_obs[2,]), na.rm = TRUE),2)
      upam[i,21] <- round(min((varRel_w_obs[3,] - varRel_w_obs[2,]), na.rm = TRUE),2)
      upam[i,22] <- round(max((varRel_w_obs[3,] - varRel_w_obs[2,]), na.rm = TRUE),2)
    }

    if(model_names[i] %in% c("RW","RW_obs","AR")){
      upam[i,23] <- NA
      upam[i,24] <- NA
      upam[i,25] <- NA
    } else {
      upam[i,23] <- round(mean((varRel_w_obs[4,] - varRel_w_obs[3,]), na.rm = TRUE),2)
      upam[i,24] <- round(min((varRel_w_obs[4,] - varRel_w_obs[3,]), na.rm = TRUE),2)
      upam[i,25] <- round(max((varRel_w_obs[4,] - varRel_w_obs[3,]), na.rm = TRUE),2)
    }

    if(model_names[i] %in% c("RW","RW_obs")){
      upam[i,26] <- round(mean((varRel_w_obs[3,] - varRel_w_obs[2,]), na.rm = TRUE),2)
      upam[i,27] <- round(min((varRel_w_obs[3,] - varRel_w_obs[2,]), na.rm = TRUE),2)
      upam[i,28] <- round(max((varRel_w_obs[3,] - varRel_w_obs[2,]), na.rm = TRUE),2)
    } else if (model_names[i] == "AR"){
      upam[i,26] <- round(mean((varRel_w_obs[4,] - varRel_w_obs[3,]), na.rm = TRUE),2)
      upam[i,27] <- round(min((varRel_w_obs[4,] - varRel_w_obs[3,]), na.rm = TRUE),2)
      upam[i,28] <- round(max((varRel_w_obs[4,] - varRel_w_obs[3,]), na.rm = TRUE),2)
    } else {
      upam[i,26] <- round(mean((varRel_w_obs[5,] - varRel_w_obs[4,]), na.rm = TRUE),2)
      upam[i,27] <- round(min((varRel_w_obs[5,] - varRel_w_obs[4,]), na.rm = TRUE),2)
      upam[i,28] <- round(max((varRel_w_obs[5,] - varRel_w_obs[4,]), na.rm = TRUE),2)
    }
  }

  #build final data frame and write to file
  means <- round(colMeans(upam, na.rm = TRUE),2)
  upam[13,] <- means
  upam[,1] <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed","all_models")
  upam <- data.frame(upam)
  colnames(upam) <- c("model_name","mean.IC","min.IC","max.IC",
                      "mean.P","min.P","max.P",
                      "mean.Pa","min.Pa","max.Pa",
                      "mean.D","min.D","max.D",
                      "mean.IC.w_obs","min.IC.w_obs","max.IC.w_obs",
                      "mean.P.w_obs","min.P.w_obs","max.P.w_obs",
                      "mean.Pa.w_obs","min.Pa.w_obs","max.Pa.w_obs",
                      "mean.D.w_obs","min.D.w_obs","max.D.w_obs",
                      "mean.obs","min.obs","max.obs")

  write.csv(upam,file=file.path(paste("./6_Output_analysis/",paste0('uncertainty_partitioning_output_analysis_wk_',forecast_weeks[n],'.csv'),sep = "")),row.names = FALSE)

}

##########################ASSESS UNCERTAINTY PARTITIONING OUTPUT#################
wk1 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_1.csv")
wk2 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_2.csv")
wk3 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_3.csv")
wk4 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_4.csv")


