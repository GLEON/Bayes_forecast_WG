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
if (model_name == "schmidt_max_lag"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
}
if (model_name == "wnd_dir_2day_lag"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_2day_lag.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_2day_lag.csv"))
}
if (model_name == "precip"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
}
if (model_name == "GDD"){
  #read in covar data
  covar0 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
  #read in prior data
  prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
}
if (model_name == "schmidt_and_temp"){
  #read in covar1 data
  covar01 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))
  #read in prior1 data
  prior1 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))

  #read in covar2 data
  covar02 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site1.csv"))
  #read in prior data
  prior2 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site2.csv"))

}
if (model_name == "schmidt_and_precip"){
  #read in covar1 data
  covar01 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))
  #read in prior1 data
  prior1 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))

  #read in covar2 data
  covar02 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
  #read in prior2 data
  prior2 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
}
if (model_name == "temp_and_precip"){
  #read in covar1 data
  covar01 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site1.csv"))
  #read in prior data
  prior1 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site2.csv"))

  #read in covar2 data
  covar02 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
  #read in prior2 data
  prior2 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
}
if (model_name == "precip_and_GDD"){
  #read in covar1 data
  covar01 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
  #read in prior2 data
  prior1 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))

  #read in covar2 data
  covar02 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
  #read in prior2 data
  prior2 <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
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
if(model_name %in% c("schmidt_med_diff","wnd_dir_2day_lag","schmidt_max_lag","precip")){
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
if(model_name %in% c("GDD")){
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

#subset covar data depending on year and season_week
if(model_name == "schmidt_and_precip"){
  if(year == 2015){

    #create covar1 timeseries
    covar1 <- covar01[c(1:7),]
    covar1[7,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:6),]
    #standardize covar hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:6),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[7,1:(season_week-1)] <- covar01[7,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:7),]
    covar2[7,] <- NA

    #create covar1 hindcast
    covar2_hindcast <- covar02[c(1:6),]
    #standardize covar hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:6),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar2[7,1:(season_week-1)] <- covar02[7,1:(season_week-1)]}

  } else {

    #create covar1 timeseries
    covar1 <- covar01[c(1:8),]
    covar1[8,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:7),]
    #standardize covar_hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:7),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[8,1:(season_week-1)] <- covar01[8,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:8),]
    covar2[8,] <- NA

    #create covar2 hindcast
    covar2_hindcast <- covar02[c(1:7),]
    #standardize covar2_hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize covar2 gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:7),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar2 timeseries according to season_week
      covar2[8,1:(season_week-1)] <- covar02[8,1:(season_week-1)]}

  }

  #standardize covar timeseries
  covar1 <- (covar1 - mean(covar1, na.rm = TRUE))/sd(covar1, na.rm = TRUE)
  covar2 <- (covar2 - mean(covar2, na.rm = TRUE))/sd(covar2, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = covar1, covar2 = covar2, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

#subset covar data depending on year and season_week
if(model_name == "temp_and_precip"){
  if(year == 2015){

    #create covar1 timeseries
    covar1 <- covar01[c(1:7),]
    covar1[7,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:6),]
    #standardize covar hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:6),], na.rm = TRUE)
    week_avg1[is.na(week_avg1)] <- week_avg1[19]

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[7,1:(season_week-1)] <- covar01[7,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:7),]
    covar2[7,] <- NA

    #create covar1 hindcast
    covar2_hindcast <- covar02[c(1:6),]
    #standardize covar hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:6),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar2[7,1:(season_week-1)] <- covar02[7,1:(season_week-1)]}

  } else {

    #create covar1 timeseries
    covar1 <- covar01[c(1:8),]
    covar1[8,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:7),]
    #standardize covar_hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:7),], na.rm = TRUE)
    week_avg1[is.na(week_avg1)] <- week_avg1[19]

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[8,1:(season_week-1)] <- covar01[8,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:8),]
    covar2[8,] <- NA

    #create covar2 hindcast
    covar2_hindcast <- covar02[c(1:7),]
    #standardize covar2_hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize covar2 gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:7),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar2 timeseries according to season_week
      covar2[8,1:(season_week-1)] <- covar02[8,1:(season_week-1)]}

  }

  #standardize covar timeseries
  covar1 <- (covar1 - mean(covar1, na.rm = TRUE))/sd(covar1, na.rm = TRUE)
  covar2 <- (covar2 - mean(covar2, na.rm = TRUE))/sd(covar2, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = covar1, covar2 = covar2, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

#subset covar data depending on year and season_week
if(model_name == "schmidt_and_temp"){
  if(year == 2015){

    #create covar1 timeseries
    covar1 <- covar01[c(1:7),]
    covar1[7,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:6),]
    #standardize covar hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:6),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[7,1:(season_week-1)] <- covar01[7,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:7),]
    covar2[7,] <- NA

    #create covar1 hindcast
    covar2_hindcast <- covar02[c(1:6),]
    #standardize covar hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:6),], na.rm = TRUE)
    week_avg2[is.na(week_avg2)] <- week_avg2[19]

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar2[7,1:(season_week-1)] <- covar02[7,1:(season_week-1)]}

  } else {

    #create covar1 timeseries
    covar1 <- covar01[c(1:8),]
    covar1[8,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:7),]
    #standardize covar_hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:7),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[8,1:(season_week-1)] <- covar01[8,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:8),]
    covar2[8,] <- NA

    #create covar2 hindcast
    covar2_hindcast <- covar02[c(1:7),]
    #standardize covar2_hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize covar2 gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:7),], na.rm = TRUE)
    week_avg2[is.na(week_avg2)] <- week_avg2[19]

    if(season_week %in% c(2:20)){
      #populate covar2 timeseries according to season_week
      covar2[8,1:(season_week-1)] <- covar02[8,1:(season_week-1)]}

  }

  #standardize covar timeseries
  covar1 <- (covar1 - mean(covar1, na.rm = TRUE))/sd(covar1, na.rm = TRUE)
  covar2 <- (covar2 - mean(covar2, na.rm = TRUE))/sd(covar2, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = covar1, covar2 = covar2, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

#subset covar data depending on year and season_week
if(model_name %in% c("precip_and_GDD")){
  if(year == 2015){

    #create covar1 timeseries
    covar1 <- covar01[c(1:7),]
    covar1[7,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:6),]
    #standardize covar hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:6),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[7,1:(season_week-1)] <- covar01[7,1:(season_week-1)]}

    #create covar 2 timeseries
    covar2 <- covar02[c(1:7),]
    covar2[7,] <- NA

    #create covar 2 hindcast
    covar2_hindcast <- covar02[c(1:6),]
    #standardize within year to account for different start dates in different years
    covar2_hindcast <- apply(covar2_hindcast,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    covar2_hindcast <- t(covar2_hindcast)
    #standardize covar 2 hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize within year to account for different start dates in different years
    prior2 <- apply(prior2,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    prior2 <- t(prior2)
    #standardize gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:6),], na.rm = TRUE)
    week_avg2[is.na(week_avg2)] <- week_avg2[19]

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar2[7,1:(season_week-1)] <- covar02[7,1:(season_week-1)]}

  } else {

    #create covar1 timeseries
    covar1 <- covar01[c(1:8),]
    covar1[8,] <- NA

    #create covar1 hindcast
    covar1_hindcast <- covar01[c(1:7),]
    #standardize covar_hindcast
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)

    #standardize covar1 gap-filling dataset
    prior1 <- (prior1 - mean(prior1, na.rm = TRUE))/sd(prior1, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg1 = colMeans(prior1[c(1:7),], na.rm = TRUE)

    if(season_week %in% c(2:20)){
      #populate covar1 timeseries according to season_week
      covar1[8,1:(season_week-1)] <- covar01[8,1:(season_week-1)]}

    #create covar2 timeseries
    covar2 <- covar02[c(1:8),]
    covar2[8,] <- NA

    #create covar2 hindcast
    covar2_hindcast <- covar02[c(1:7),]
    #standardize within year to account for different start dates in different years
    covar2_hindcast <- apply(covar2_hindcast,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    covar2_hindcast <- t(covar2_hindcast)
    #standardize covar2_hindcast
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)

    #standardize within year to account for different start dates in different years
    prior2 <- apply(prior2,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #transpose
    prior2 <- t(prior2)
    #standardize gap-filling dataset
    prior2 <- (prior2 - mean(prior2, na.rm = TRUE))/sd(prior2, na.rm = TRUE)
    #create gap-filling weekly avg
    week_avg2 = colMeans(prior2[c(1:7),], na.rm = TRUE)
    week_avg2[is.na(week_avg2)] <- week_avg2[19]

    if(season_week %in% c(2:20)){
      #populate covar timeseries according to season_week
      covar2[8,1:(season_week-1)] <- covar02[8,1:(season_week-1)]}

  }

  #standardize covar timeseries
  covar1 <- (covar1 - mean(covar1, na.rm = TRUE))/sd(covar1, na.rm = TRUE)
  covar2 <- (covar2 - mean(covar2, na.rm = TRUE))/sd(covar2, na.rm = TRUE)

  return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = covar1, covar2 = covar2, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

}
