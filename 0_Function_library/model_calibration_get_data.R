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
y <- log(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))+0.0036)
#remove 2015-2016 data
y <- y[-c(7:8),]

#for RW, RW_obs, and AR models
if(model_name %in% c("RW","RW_obs","AR")){
  return(list(year_no = year_no, season_weeks = season_weeks, y = y))
}

#for Linear_1var model
if(model_name == "Linear_1var"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- as.matrix(scale(Temp, center = TRUE, scale = TRUE))
  #remove attributes that will crash JAGS
  attr(Temp,"scaled:center")<-NULL
  attr(Temp,"scaled:scale")<-NULL

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[18]

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, Temp = Temp, week_avg = week_avg))
}

#for Quad_1var model
if(model_name == "Quad_1var"){

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_mean.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg = colMeans(Schmidt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, Schmidt = Schmidt, week_avg = week_avg))
}

#for Linear_2var model
if(model_name == "Linear_2var"){

  #read in covariate data
  Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
  #remove 2015-2016 data
  Temp <- Temp[-c(7:8),]
  #center covariate data
  Temp <- as.matrix(scale(Temp, center = TRUE, scale = TRUE))
  #remove attributes that will crash JAGS
  attr(Temp,"scaled:center")<-NULL
  attr(Temp,"scaled:scale")<-NULL

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as gap-filler for weeks 19 & 20
  week_avg_T[is.na(week_avg_T)] <- week_avg_T[18]

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_mean.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)
  #remove attributes that will crash JAGS
  attr(Schmidt,"scaled:center")<-NULL
  attr(Schmidt,"scaled:scale")<-NULL

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
  Temp <- as.matrix(scale(Temp, center = TRUE, scale = TRUE))
  #remove attributes that will crash JAGS
  attr(Temp,"scaled:center")<-NULL
  attr(Temp,"scaled:scale")<-NULL

  #read in data from Site 2 for data gap-filling
  Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
  #remove 2015-2016 data
  Temp_prior <- Temp_prior[-c(7:8),]
  #center water temp data
  Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
  #use weekly average from last sampled week (18) to serve as gap-filler for weeks 19 & 20
  week_avg_T[is.na(week_avg_T)] <- week_avg_T[18]

  #read in covariate data
  Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_mean.csv"))
  #remove 2015-2016 data
  Schmidt <- Schmidt[-c(7:8),]
  #center covariate data
  Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)
  #remove attributes that will crash JAGS
  attr(Schmidt,"scaled:center")<-NULL
  attr(Schmidt,"scaled:scale")<-NULL

  #calculate weekly average of covariate from past years for gap filling
  week_avg_S = colMeans(Schmidt, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, Temp = Temp, Schmidt = Schmidt, week_avg_T = week_avg_T, week_avg_S = week_avg_S))

}


}
