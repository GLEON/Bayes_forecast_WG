# Title: Pull appropriate data files for model calibration runs
# History:
# created MEL 27MAR20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

get_calibration_data <- function(model_name){

#set calibration years and weeks of season - do not edit
years <- c(2009:2014)
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)

#read in Gloeo data
y <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))
#remove 2015-2016 data
y <- y[-c(7:8),]

###############################GLOEO-ONLY MODELS#####################################
#for RW, RW_obs, and AR models
if(model_name %in% c("RW","RW_obs","AR")){
  return(list(year_no = year_no, season_weeks = season_weeks, y = y))
}

###############################SINGLE COVARIATE LINEAR MODELS#####################################

#for wtrtemp_min model
if(model_name == "wtrtemp_min"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Temp, week_avg = week_avg))
}

#for wtrtemp_min_lag model
if(model_name == "wtrtemp_min_lag"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Temp, week_avg = week_avg))
}

#for wtrtemp_MA7 model
if(model_name == "wtrtemp_MA7"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Temp, week_avg = week_avg))
}

#for schmidt_med_diff model
if(model_name == "schmidt_med_diff"){

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Schmidt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Schmidt, week_avg = week_avg))
}

###############################SINGLE COVARIATE QUADRATIC MODELS#####################################

#for GDD model
if(model_name == "GDD" | model_name == "GDD_test"){

  #read in covariate data
  GDD <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
  #remove 2015-2016 data
  GDD <- GDD[-c(7:8),]
  #standardize within year to account for different start dates in different years
  GDD <- apply(GDD,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
  #transpose
  GDD <- t(GDD)
  #standardize across years
  GDD <- (GDD - mean(GDD, na.rm = TRUE))/sd(GDD, na.rm = TRUE)

  #read in data from Site 2 for data gap-filling
  GDD_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
  #remove 2015-2016 data
  GDD_prior <- GDD_prior[-c(7:8),]
  #standardize within year to account for different start dates in different years
  GDD_prior <- apply(GDD_prior,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
  #transpose
  GDD_prior <- t(GDD_prior)
  #standardize across years
  GDD_prior <- (GDD_prior - mean(GDD_prior, na.rm = TRUE))/sd(GDD_prior, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(GDD_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = GDD, week_avg = week_avg))
}

#for Linear_2var model
if(model_name == "Linear_2var"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as gap-filler for weeks 19 & 20
  week_avg_T[is.na(week_avg_T)] <- week_avg_T[19]

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_mean.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg_S = colMeans(Schmidt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, Temp = Temp, Schmidt = Schmidt, week_avg_T = week_avg_T, week_avg_S = week_avg_S))

}

#for Quad_2var model
if(model_name == "Quad_2var"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as gap-filler for weeks 19 & 20
  week_avg_T[is.na(week_avg_T)] <- week_avg_T[19]

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_mean.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg_S = colMeans(Schmidt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, Temp = Temp, Schmidt = Schmidt, week_avg_T = week_avg_T, week_avg_S = week_avg_S))

}


}
