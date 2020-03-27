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
y <- log(as.matrix(read_csv("./00_Data_files/Gechinulata_Site1.csv"))+0.0036)
#remove 2015-2016 data
y <- y[-c(7:8),]

# #for GDD
# GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_28JAN20.csv"))
# GDD <- scale(GDD, center = TRUE, scale = TRUE)
#
# #for DayLength
# DayLength <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/daylength_year_by_week_28JAN20.csv"))
# DayLength <- scale(DayLength, center = TRUE, scale = TRUE)
#
# #for SW radiation
# SW <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_24FEB20.csv"))
# SW <- scale(SW, center = TRUE, scale = TRUE)
#
# #for Minwind
# Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_NLDASminwind_03MAR20.csv"))
# Wnd <- scale(Wnd, center = TRUE, scale = TRUE)
#
# #for CVwind
# Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_CVwind_24FEB20.csv"))
# Wnd <- scale(Wnd, center = TRUE, scale = TRUE)
#
# #for watertemp_min
# Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_16AUG19.csv"))
# Temp <- scale(Temp, center = TRUE, scale = TRUE)
# Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv"))
# Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)
#
# #for max Schmidt
# Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv"))
# Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)
#
# #for min Schmidt
# Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_min_Schmidt_28JAN20.csv"))
# Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)
#
# #for underwater light from HOBOs
# Light <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv"))
# Light <- scale(Light, center = TRUE, scale = TRUE)
#
# #for Ppt
# Ppt <- read_csv("C:/Users/Mary Lofton/Documents/RProjects/GLEON_Bayesian_WG/Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_summed_precip_10OCT19.csv")
#
# #for water temp
# week_avg = colMeans(Temp_prior, na.rm = TRUE)
#
# #for min water temp
# week_min = colMeans(Temp_prior, na.rm = TRUE)
#
# #for Schmidt
# week_avg = colMeans(Schmidt, na.rm = TRUE)
#
# #for max Schmidt
# week_max = colMeans(Schmidt, na.rm = TRUE)
#
# #for min Schmidt
# week_min = colMeans(Schmidt, na.rm = TRUE)
#
# #for GDD
# week_avg = colMeans(GDD, na.rm = TRUE)
#
# #for DayLength
# week_avg = colMeans(DayLength, na.rm = TRUE)
#
# #for SW radiation
# week_avg = colMeans(SW, na.rm = TRUE)
#
# #for precipitation
# week_avg = colMeans(Ppt, na.rm = TRUE)
#
# #for underwater light
# week_avg = colMeans(Light, na.rm = TRUE)
# week_avg[c(19,20)]<- week_avg[18]
#
# #for Minwind
# week_min = colMeans(Wnd, na.rm = TRUE)
#
# #for CVwind
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

return(list(year_no = year_no, season_weeks = season_weeks, y = y))

}
