# Title: Pull appropriate data files for model calibration runs
# History:
# created MEL 27MAR20

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

get_hindcast_data <- function(model_name, year, season_week){

#set calibration years and weeks of season - do not edit
if(year == 2015){years <- c(2009:2015)} else {years <- c(2009:2016)}
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)

#read in Gloeo data
y0 <- log(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))+0.0035)

#subset data depending on year and season_week
if(year == 2015){
  y <- y0[c(1:6),]
  y[7,] <- NA

  if(season_week %in% c(2:20)){
  y[7,1:(season_week-1)] <- y0[7,1:(season_week-1)]}

} else {
  y <- y0[c(1:7),]
  y[8,] <- NA

  if(season_week %in% c(2:20)){
  y[8,1:(season_week-1)] <- y0[8,1:(season_week-1)]}

}

# #for watertemp_min
# Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_16AUG19.csv"))
# Temp <- scale(Temp, center = TRUE, scale = TRUE)
# Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv"))
# Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)
#
# #for DayLength
# DayLength <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/daylength_year_by_week_28JAN20.csv"))
#
# #for max Schmidt
# Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv"))
#
# #for Ppt
# Ppt <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_summed_precip_10OCT19.csv"))
#
# #for underwater light from HOBOs
# Light <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv"))
#
# #for Wnd
# Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_wind_perc90_14OCT19.csv"))
#
# #for GDD
# GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_28JAN20.csv"))
#
# years <- c(2009:2016)
# forecast_years <- c(2015:2016)
# year_no = as.numeric(as.factor(years))
# season_weeks = c(1:20)
# site = "Midge"
#
# #for min water temp
# week_min = colMeans(Temp_prior, na.rm = TRUE)
#
# #for DayLength
# week_avg = colMeans(DayLength, na.rm = TRUE)
#
# #for max Schmidt
# week_max = colMeans(Schmidt, na.rm = TRUE)
#
# #for cv Wnd
# week_cv = colMeans(Wnd, na.rm = TRUE)
#
# #for combined covariate model
# week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
# week_avg_S = colMeans(Schmidt, na.rm = TRUE)
#
# #for combined covariate model
# week_min_T = colMeans(Temp_prior, na.rm = TRUE)
# week_min_S = colMeans(Schmidt, na.rm = TRUE)
# week_min_W = colMeans(Wnd, na.rm = TRUE)

# ## Forward Simulation
# N_weeks <- c(1:40)
# observ <- c(forecast_y[1,],forecast_y[2,])
# y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
#
# Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_forecast_05OCT19.csv"))
# Temp <- scale(Temp, center = TRUE, scale = TRUE)
# observ_Temp <- c(Temp[7,],Temp[8,])
#
# SW <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_forecast_03MAR20.csv"))
# SW <- scale(SW, center = TRUE, scale = TRUE)
# observ_SW <- c(SW[7,],SW[8,])
#
# GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_forecast_03MAR20.csv"))
# GDD <- scale(GDD, center = TRUE, scale = TRUE)
# observ_GDD <- c(GDD[7,],GDD[8,])

# N_weeks <- c(1:40)
# observ <- c(forecast_y[1,],forecast_y[2,])
# y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
#
# Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_forecast_05OCT19.csv"))
# Temp <- scale(Temp, center = TRUE, scale = TRUE)
# observ_Temp <- c(Temp[7,],Temp[8,])
#
# SW <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_forecast_03MAR20.csv"))
# SW <- scale(SW, center = TRUE, scale = TRUE)
# observ_SW <- c(SW[7,],SW[8,])
#
# GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_forecast_03MAR20.csv"))
# GDD <- scale(GDD, center = TRUE, scale = TRUE)
# observ_GDD <- c(GDD[7,],GDD[8,])
#
# if(i == 1){
#   obs_data <- rep(NA,40)
#   obs_Temp <- rep(NA,40)
#   obs_SW <- rep(NA,40)
#   obs_GDD <- rep(NA,40)
# } else{
#   obs_data <- rep(NA,40)
#   obs_data[1:N_weeks[i-1]] <- observ[1:N_weeks[i-1]]
#
#   obs_Temp <- rep(NA,40)
#   obs_Temp[1:N_weeks[i-1]] <- observ_Temp[1:N_weeks[i-1]]
#
#   obs_SW <- rep(NA,40)
#   obs_SW[1:N_weeks[i-1]] <- observ_SW[1:N_weeks[i-1]]
#
#   obs_GDD <- rep(NA,40)
#   obs_GDD[1:N_weeks[i-1]] <- observ_GDD[1:N_weeks[i-1]]
#
# }
#
# y[7,] <- obs_data[1:20]
# y[8,] <- obs_data[21:40]
#
# Temp[7,] <- obs_Temp[1:20]
# Temp[8,] <- obs_Temp[21:40]
#
# SW[7,] <- obs_SW[1:20]
# SW[8,] <- obs_SW[21:40]
#
# GDD[7,] <- obs_GDD[1:20]
# GDD[8,] <- obs_GDD[21:40]
#
# if(N_weeks[i] %in% c(1:20)){
#   y <- y[1:7,]
#   Temp <- Temp[1:7,]
#   SW <- SW[1:7,]
#   GDD <- GDD[1:7,]
#   year_no = as.numeric(as.factor(c(2009:2015)))
# } else {
#   y <- y[1:8,]
#   Temp <- Temp[1:8,]
#   SW <- SW[1:8,]
#   GDD <- GDD[1:8,]
#   year_no = as.numeric(as.factor(c(2009:2016)))
# }




return(list(year_no = year_no, season_weeks = season_weeks, y = y))

}
