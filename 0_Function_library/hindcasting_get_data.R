# Title: Pull appropriate data files for model calibration runs
# History:
# created MEL 27MAR20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

get_hindcast_data <- function(model_name, year, season_week){

###############################FOR ALL MODELS#####################################

#set calibration years and weeks of season - do not edit
if(year == 2015){years <- c(2009:2015)} else {years <- c(2009:2016)}
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)

#read in Gloeo data
y0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))

#subset data depending on year and season_week
if(year == 2015){
  y <- y0[c(1:7),]
  y[7,] <- NA

  if(season_week %in% c(2:20)){
  y[7,1:(season_week-1)] <- y0[7,1:(season_week-1)]}

} else {
  y <- y0[c(1:8),]
  y[8,] <- NA

  if(season_week %in% c(2:20)){
  y[8,1:(season_week-1)] <- y0[8,1:(season_week-1)]}

}

###############################GLOEO-ONLY MODELS#####################################
#for RW, RW_obs, and AR models
if(model_name %in% c("RW","RW_obs","AR")){
  return(list(year_no = year_no, season_weeks = season_weeks, y = y))
}

###############################READ IN DATA FOR SINGLE COVAR MODELS#####################################
if (model_name == "wtrtemp_min"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
}
if (model_name == "wtrtemp_min_lag"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site1.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site2.csv"))
}
if (model_name == "wtrtemp_MA7"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site1.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site2.csv"))
}
if (model_name == "schmidt_med_diff"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))
}
if (model_name == "wnd_dir_2day_lag"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_2day_lag.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_2day_lag.csv"))
}
if (model_name == "GDD" | model_name == "GDD_test"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
}

###############################WRANGLE DATA FOR SINGLE COVAR MODELS#####################################

#subset covar data depending on year and season_week
if(model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7")){
  if(year == 2015){

    #create covar timeseries
    covar <- covar0[c(1:7),]
    covar[7,] <- NA

    #create covar hindcast
    covar_hindcast <- covar0[c(1:6),]
    #standardize covar hindcast
    covar_hindcast <- (covar_hindcast - mean(covar_hindcast, na.rm = TRUE))/sd(covar_hindcast, na.rm = TRUE)

    #standardize gap-filling dataset
    prior <- (prior - mean(prior, na.rm = TRUE))/sd(prior, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg = colMeans(prior[c(1:6),], na.rm = TRUE)
    week_avg[is.na(week_avg)] <- week_avg[19]

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar[7,1:(season_week-1)] <- covar0[7,1:(season_week-1)]}

  } else {

    #create covar timeseries
    covar <- covar0[c(1:8),]
    covar[8,] <- NA

    #create covar hindcast
    covar_hindcast <- covar0[c(1:7),]
    #standardize covar_hindcast
    covar_hindcast <- (covar_hindcast - mean(covar_hindcast, na.rm = TRUE))/sd(covar_hindcast, na.rm = TRUE)

    #standardize gap-filling dataset
    prior <- (prior - mean(prior, na.rm = TRUE))/sd(prior, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg = colMeans(prior[c(1:7),], na.rm = TRUE)
    week_avg[is.na(week_avg)] <- week_avg[19]

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar[8,1:(season_week-1)] <- covar0[8,1:(season_week-1)]}

  }

  #standardize covar timeseries
  covar <- (covar - mean(covar, na.rm = TRUE))/sd(covar, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = covar, covar_hindcast = covar_hindcast, week_avg = week_avg))

}

#subset covar data depending on year and season_week
if(model_name %in% c("schmidt_med_diff","wnd_dir_2day_lag")){
  if(year == 2015){

    #create covar timeseries
    covar <- covar0[c(1:7),]
    covar[7,] <- NA

    #create covar hindcast
    covar_hindcast <- covar0[c(1:6),]
    #standardize covar hindcast
    covar_hindcast <- (covar_hindcast - mean(covar_hindcast, na.rm = TRUE))/sd(covar_hindcast, na.rm = TRUE)

    #standardize gap-filling dataset
    prior <- (prior - mean(prior, na.rm = TRUE))/sd(prior, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg = colMeans(prior[c(1:6),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar[7,1:(season_week-1)] <- covar0[7,1:(season_week-1)]}

  } else {

    #create covar timeseries
    covar <- covar0[c(1:8),]
    covar[8,] <- NA

    #create covar hindcast
    covar_hindcast <- covar0[c(1:7),]
    #standardize covar_hindcast
    covar_hindcast <- (covar_hindcast - mean(covar_hindcast, na.rm = TRUE))/sd(covar_hindcast, na.rm = TRUE)

    #standardize gap-filling dataset
    prior <- (prior - mean(prior, na.rm = TRUE))/sd(prior, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg = colMeans(prior[c(1:7),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar[8,1:(season_week-1)] <- covar0[8,1:(season_week-1)]}

  }

  #standardize covar timeseries
  covar <- (covar - mean(covar, na.rm = TRUE))/sd(covar, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = covar, covar_hindcast = covar_hindcast, week_avg = week_avg))

}

#subset covar data depending on year and season_week
if(model_name %in% c("GDD","GDD_test")){
  if(year == 2015){

    #create covar timeseries
    covar <- covar0[c(1:7),]
    covar[7,] <- NA

    #create covar hindcast
    covar_hindcast <- covar0[c(1:6),]
    #standardize within year to account for different start dates in different years
    covar_hindcast <- apply(covar_hindcast,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    covar_hindcast <- t(covar_hindcast)
    #standardize covar hindcast
    covar_hindcast <- (covar_hindcast - mean(covar_hindcast, na.rm = TRUE))/sd(covar_hindcast, na.rm = TRUE)

    #standardize within year to account for different start dates in different years
    prior <- apply(prior,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    prior <- t(prior)
    #standardize gap-filling dataset
    prior <- (prior - mean(prior, na.rm = TRUE))/sd(prior, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg = colMeans(prior[c(1:6),], na.rm = TRUE)
    week_avg[is.na(week_avg)] <- week_avg[19]

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar[7,1:(season_week-1)] <- covar0[7,1:(season_week-1)]}

  } else {

    #create covar timeseries
    covar <- covar0[c(1:8),]
    covar[8,] <- NA

    #create covar hindcast
    covar_hindcast <- covar0[c(1:7),]
    #standardize within year to account for different start dates in different years
    covar_hindcast <- apply(covar_hindcast,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    covar_hindcast <- t(covar_hindcast)
    #standardize covar_hindcast
    covar_hindcast <- (covar_hindcast - mean(covar_hindcast, na.rm = TRUE))/sd(covar_hindcast, na.rm = TRUE)

    #standardize within year to account for different start dates in different years
    prior <- apply(prior,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    prior <- t(prior)
    #standardize gap-filling dataset
    prior <- (prior - mean(prior, na.rm = TRUE))/sd(prior, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg = colMeans(prior[c(1:7),], na.rm = TRUE)
    week_avg[is.na(week_avg)] <- week_avg[19]

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar[8,1:(season_week-1)] <- covar0[8,1:(season_week-1)]}

  }

  #standardize within year to account for different start dates in different years
  covar <- apply(covar,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
  #transpose
  covar <- t(covar)
  #standardize covar timeseries
  covar <- (covar - mean(covar, na.rm = TRUE))/sd(covar, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = covar, covar_hindcast = covar_hindcast, week_avg = week_avg))

}

}
