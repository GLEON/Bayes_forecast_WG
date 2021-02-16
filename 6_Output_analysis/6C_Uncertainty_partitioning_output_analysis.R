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
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","wnd_dir_2day_lag","GDD","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD")
forecast_weeks <- c(1:4)

########################CALCULATE ASSESSMENT METRICS#####################################

for (n in 1:length(forecast_weeks)){

  #set up matrix for uncertainty partitioning assessment metrics
  upam <- matrix(NA,length(model_names)+1,13)

  for (i in 1:length(model_names)){

    #read in variance matrices
    varRel <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv')))))

    #summarize proportional contributions of variance not including observation error
    upam[i,1] <- NA

    #IC
    upam[i,2] <- round(mean(varRel[1,], na.rm = TRUE),2)
    upam[i,3] <- round(min(varRel[1,], na.rm = TRUE),2)
    upam[i,4] <- round(max(varRel[1,], na.rm = TRUE),2)

    #Parameter
    if(model_names[i] %in% c("RW","RW_obs")){
      upam[i,5] <- NA
      upam[i,6] <- NA
      upam[i,7] <- NA
    } else {
      upam[i,5] <- round(mean((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
      upam[i,6] <- round(min((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
      upam[i,7] <- round(max((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
    }

    #Driver
    if(model_names[i] %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
      upam[i,8] <- NA
      upam[i,9] <- NA
      upam[i,10] <- NA
    } else {
      upam[i,8] <- round(mean((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[i,9] <- round(min((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[i,10] <- round(max((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
    }

    #Process
    if(model_names[i] %in% c("RW","RW_obs")){
      upam[i,11] <- round(mean((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
      upam[i,12] <- round(min((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
      upam[i,13] <- round(max((varRel[2,] - varRel[1,]), na.rm = TRUE),2)
    } else if (model_names[i] %in% c("RW_bias","AC","base_DLM")) {
      upam[i,11] <- round(mean((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[i,12] <- round(min((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
      upam[i,13] <- round(max((varRel[3,] - varRel[2,]), na.rm = TRUE),2)
    } else {
      upam[i,11] <- round(mean((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
      upam[i,12] <- round(min((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
      upam[i,13] <- round(max((varRel[4,] - varRel[3,]), na.rm = TRUE),2)
    }

  }

  #build final data frame and write to file
  means <- round(colMeans(upam, na.rm = TRUE),2)
  upam[length(model_names)+1,] <- means
  upam[,1] <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","wnd_dir_2day_lag","GDD","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD","all_models")
  upam <- data.frame(upam)
  colnames(upam) <- c("model_name","mean.IC","min.IC","max.IC",
                      "mean.Pa","min.Pa","max.Pa",
                      "mean.D","min.D","max.D",
                      "mean.P","min.P","max.P")

  write.csv(upam,file=file.path(paste("./6_Output_analysis/",paste0('uncertainty_partitioning_output_analysis_wk_',forecast_weeks[n],'.csv'),sep = "")),row.names = FALSE)

}

##########################ASSESS UNCERTAINTY PARTITIONING OUTPUT#################
wk1 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_1.csv")
wk2 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_2.csv")
wk3 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_3.csv")
wk4 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_4.csv")
