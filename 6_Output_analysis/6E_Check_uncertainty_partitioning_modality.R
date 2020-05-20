# Title: 6E_Check_uncertainty_partitioning_modality
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
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_and_temp","schmidt_max_lag","precip","schmidt_and_precip","temp_and_precip","precip_and_GDD")
forecast_weeks <- c(1,4)

########################CALCULATE ASSESSMENT METRICS#####################################

for (n in 1:length(forecast_weeks)){

  #varRel_all <- matrix(NA,5,1)

  for (i in 1:length(model_names)){

    #read in variance matrices
    varRel <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv')))))

    #IC
    png(filename = paste0("C:/Users/Mary Lofton/Dropbox/Ch5/histograms_for_KLC/",model_names[i],"_IC_",forecast_weeks[n],".png"))
    plot(density(varRel[1,], na.rm = TRUE),main = paste(model_names[i],"IC"))
    abline(v = mean(varRel[1,], na.rm = TRUE), col = "red")
    dev.off()

    #Parameter
    if(!model_names[i] %in% c("RW","RW_obs")){
      png(filename = paste0("C:/Users/Mary Lofton/Dropbox/Ch5/histograms_for_KLC/",model_names[i],"_PARAMETER_",forecast_weeks[n],".png"))
      plot(density(varRel[2,] - varRel[1,], na.rm = TRUE),main = paste(model_names[i],"Parameter"))
      abline(v = mean(varRel[2,] - varRel[1,], na.rm = TRUE), col = "red")
      dev.off()

    }

    #Driver
    if(!model_names[i] %in% c("RW","RW_obs","AR")){
      png(filename = paste0("C:/Users/Mary Lofton/Dropbox/Ch5/histograms_for_KLC/",model_names[i],"_DRIVER_",forecast_weeks[n],".png"))
      plot(density(varRel[3,] - varRel[2,], na.rm = TRUE),main = paste(model_names[i],"Driver"))
      abline(v = mean(varRel[3,] - varRel[2,], na.rm = TRUE), col = "red")
      dev.off()


    }

    #Process
    if(model_names[i] %in% c("RW","RW_obs")){
      png(filename = paste0("C:/Users/Mary Lofton/Dropbox/Ch5/histograms_for_KLC/",model_names[i],"_PROCESS_",forecast_weeks[n],".png"))
      plot(density(varRel[2,] - varRel[1,], na.rm = TRUE), main = paste(model_names[i],"Process"))
      abline(v = mean(varRel[2,] - varRel[1,], na.rm = TRUE), col = "red")
      dev.off()


    } else if (model_names[i] == "AR") {
      png(filename = paste0("C:/Users/Mary Lofton/Dropbox/Ch5/histograms_for_KLC/",model_names[i],"_PROCESS_",forecast_weeks[n],".png"))
      plot(density(varRel[3,] - varRel[2,], na.rm = TRUE), main = paste(model_names[i],"Process"))
      abline(v = mean(varRel[3,] - varRel[2,], na.rm = TRUE), col = "red")
      dev.off()


    } else {
      png(filename = paste0("C:/Users/Mary Lofton/Dropbox/Ch5/histograms_for_KLC/",model_names[i],"_PROCESS_",forecast_weeks[n],".png"))
      plot(density(varRel[4,] - varRel[3,], na.rm = TRUE), main = paste(model_names[i],"Process"))
      abline(v = mean(varRel[4,] - varRel[3,], na.rm = TRUE), col = "red")
      dev.off()


    }

  }

}

##########################ASSESS UNCERTAINTY PARTITIONING OUTPUT#################
wk1 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_1.csv")
wk2 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_2.csv")
wk3 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_3.csv")
wk4 <- read_csv("./6_Output_analysis/uncertainty_partitioning_output_analysis_wk_4.csv")
