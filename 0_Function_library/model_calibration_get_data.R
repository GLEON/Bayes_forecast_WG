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
if(model_name %in% c("RW","RW_obs","AC","RY","RW_bias","base_DLM")){
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

#for schmidt_max_lag model
if(model_name == "schmidt_max_lag"){

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Schmidt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Schmidt, week_avg = week_avg))
}

#for wnd_dir_2day_lag model
if(model_name == "wnd_dir_2day_lag"){

  #read in covariate data
  Wnd <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
  #remove 2015-2016 data
  Wnd <- Wnd[-c(7:8),]
  #center covariate data
  Wnd <- (Wnd - mean(Wnd, na.rm = TRUE))/sd(Wnd, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Wnd, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Wnd, week_avg = week_avg))
}

#for precip model
if(model_name == "precip"){

  #read in covariate data
  Ppt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
  #remove 2015-2016 data
  Ppt <- Ppt[-c(7:8),]
  #center covariate data
  Ppt <- (Ppt - mean(Ppt, na.rm = TRUE))/sd(Ppt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Ppt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Ppt, week_avg = week_avg))
}

###############################SINGLE COVARIATE QUADRATIC MODELS#####################################

#for GDD model
if(model_name == "GDD"){

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

###############################TWO COVARIATE MODELS#####################################

#for schmidt_and_temp
if(model_name == "schmidt_and_temp"){

  #read in covariate 1 data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg1 = colMeans(Schmidt, na.rm = TRUE)

  #read in covariate 2 data
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
  week_avg2 = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg2[is.na(week_avg2)] <- week_avg2[19]


  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Schmidt, covar2 = Temp, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

#for schmidt_and_wind
if(model_name == "schmidt_and_wind"){

  #read in covariate 1 data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg1 = colMeans(Schmidt, na.rm = TRUE)

  #read in covariate 2 data
  Wind <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
  #remove 2015-2016 data
  Wind <- Wind[-c(7:8),]
  #center covariate data
  Wind <- (Wind - mean(Wind, na.rm = TRUE))/sd(Wind, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg2 = colMeans(Wind, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Schmidt, covar2 = Wind, week_avg1 = week_avg1, week_avg2 = week_avg2))

}


#for temp_and_wind
if(model_name == "temp_and_wind"){

  #read in covariate 1 data
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
  week_avg1 = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg1[is.na(week_avg1)] <- week_avg1[19]

  #read in covariate 2 data
  Wind <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
  #remove 2015-2016 data
  Wind <- Wind[-c(7:8),]
  #center covariate data
  Wind <- (Wind - mean(Wind, na.rm = TRUE))/sd(Wind, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg2 = colMeans(Wind, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Temp, covar2 = Wind, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

###############################TWO COVARIATE QUADRATIC MODELS#####################################

#for schmidt_and_GDD
if(model_name == "schmidt_and_GDD"){

  #read in covariate 1 data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg1 = colMeans(Schmidt, na.rm = TRUE)

  #read in covariate 2 data
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

  #calculate weekly average of covariate 2 from past years for gap filling
  week_avg2 = colMeans(GDD_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg2[is.na(week_avg2)] <- week_avg2[19]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Schmidt, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

#for wind_and_GDD
if(model_name == "wind_and_GDD"){

  #read in covariate 1 data
  Wind <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
  #remove 2015-2016 data
  Wind <- Wind[-c(7:8),]
  #center covariate data
  Wind <- (Wind - mean(Wind, na.rm = TRUE))/sd(Wind, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg1 = colMeans(Wind, na.rm = TRUE)

  #read in covariate 2 data
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

  #calculate weekly average of covariate 2 from past years for gap filling
  week_avg2 = colMeans(GDD_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg2[is.na(week_avg2)] <- week_avg2[19]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Wind, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

}
