# Title: Pull appropriate data files for model calibration runs
# History:
# created MEL 27MAR20
# JB Edits 4 May 2020 - run GDD and min water temp model on single site (HC)
# data in long format - with sites as columns

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

get_calibration_data <- function(model_name){

#set calibration years and weeks of season - do not edit
# years <- c(2009:2016) # changed to full time period now
# year_no = as.numeric(as.factor(years))

season_weeks = c(1:160) # full season weeks instead of 1:20

site_no = c(1:4)

year_no = c(1:8) # or does this need to be long 160?

#read in Gloeo data
gloeo_hc <- read_csv("./00_Data_files/Bayesian_model_input_data/Gloeo_HC.csv")

y = gloeo_hc$hc_gloeo_ln


###############################GLOEO-ONLY MODELS#####################################
#for RW, RW_obs, and AR models
# if(model_name %in% c("RW","RW_obs","AC","RY","RW_bias","base_DLM")){
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y))
# }

###############################SINGLE COVARIATE LINEAR MODELS#####################################

#for wtrtemp_min model
# if(model_name == "wtrtemp_min"){
#
#   #read in covariate data
#   Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
#   #remove 2015-2016 data
#   Temp <- Temp[-c(7:8),]
#   #center covariate data
#   Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
#   #remove 2015-2016 data
#   Temp_prior <- Temp_prior[-c(7:8),]
#   #center water temp data
#   Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Temp_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg[is.na(week_avg)] <- week_avg[19]
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Temp, week_avg = week_avg))
# }
#
# #for wtrtemp_min_lag model
# if(model_name == "wtrtemp_min_lag"){
#
#   #read in covariate data
#   Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site1.csv"))
#   #remove 2015-2016 data
#   Temp <- Temp[-c(7:8),]
#   #center covariate data
#   Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site2.csv"))
#   #remove 2015-2016 data
#   Temp_prior <- Temp_prior[-c(7:8),]
#   #center water temp data
#   Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Temp_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg[is.na(week_avg)] <- week_avg[19]
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Temp, week_avg = week_avg))
# }
#
# #for wtrtemp_MA7 model
# if(model_name == "wtrtemp_MA7"){
#
#   #read in covariate data
#   Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site1.csv"))
#   #remove 2015-2016 data
#   Temp <- Temp[-c(7:8),]
#   #center covariate data
#   Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site2.csv"))
#   #remove 2015-2016 data
#   Temp_prior <- Temp_prior[-c(7:8),]
#   #center water temp data
#   Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Temp_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg[is.na(week_avg)] <- week_avg[19]
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Temp, week_avg = week_avg))
# }
#
# #for schmidt_med_diff model
# if(model_name == "schmidt_med_diff"){
#
#   #read in covariate data
#   Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv"))
#   #remove 2015-2016 data
#   Schmidt <- Schmidt[-c(7:8),]
#   #center covariate data
#   Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Schmidt, na.rm = TRUE)
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Schmidt, week_avg = week_avg))
# }
#
# #for schmidt_max_lag model
# if(model_name == "schmidt_max_lag"){
#
#   #read in covariate data
#   Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
#   #remove 2015-2016 data
#   Schmidt <- Schmidt[-c(7:8),]
#   #center covariate data
#   Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Schmidt, na.rm = TRUE)
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Schmidt, week_avg = week_avg))
# }
#
# #for wnd_dir_2day_lag model
# if(model_name == "wnd_dir_2day_lag"){
#
#   #read in covariate data
#   Wnd <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
#   #remove 2015-2016 data
#   Wnd <- Wnd[-c(7:8),]
#   #center covariate data
#   Wnd <- (Wnd - mean(Wnd, na.rm = TRUE))/sd(Wnd, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Wnd, na.rm = TRUE)
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Wnd, week_avg = week_avg))
# }
#
# #for precip model
# if(model_name == "precip"){
#
#   #read in covariate data
#   Ppt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/precip_mm.csv"))
#   #remove 2015-2016 data
#   Ppt <- Ppt[-c(7:8),]
#   #center covariate data
#   Ppt <- (Ppt - mean(Ppt, na.rm = TRUE))/sd(Ppt, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(Ppt, na.rm = TRUE)
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = Ppt, week_avg = week_avg))
# }

###############################SINGLE COVARIATE QUADRATIC MODELS#####################################

#for GDD model
# if(model_name == "GDD"){
#
#   #read in covariate data
#   GDD <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
#   #remove 2015-2016 data
#   GDD <- GDD[-c(7:8),]
#   #standardize within year to account for different start dates in different years
#   GDD <- apply(GDD,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
#   #transpose
#   GDD <- t(GDD)
#   #standardize across years
#   GDD <- (GDD - mean(GDD, na.rm = TRUE))/sd(GDD, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   GDD_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
#   #remove 2015-2016 data
#   GDD_prior <- GDD_prior[-c(7:8),]
#   #standardize within year to account for different start dates in different years
#   GDD_prior <- apply(GDD_prior,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
#   #transpose
#   GDD_prior <- t(GDD_prior)
#   #standardize across years
#   GDD_prior <- (GDD_prior - mean(GDD_prior, na.rm = TRUE))/sd(GDD_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg = colMeans(GDD_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg[is.na(week_avg)] <- week_avg[19]
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar = GDD, week_avg = week_avg))
# }

###############################TWO COVARIATE MODELS#####################################

#for schmidt_and_temp
# if(model_name == "schmidt_and_temp"){
#
#   #read in covariate 1 data
#   Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
#   #remove 2015-2016 data
#   Schmidt <- Schmidt[-c(7:8),]
#   #center covariate data
#   Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg1 = colMeans(Schmidt, na.rm = TRUE)
#
#   #read in covariate 2 data
#   Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
#   #remove 2015-2016 data
#   Temp <- Temp[-c(7:8),]
#   #center covariate data
#   Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
#   #remove 2015-2016 data
#   Temp_prior <- Temp_prior[-c(7:8),]
#   #center water temp data
#   Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg2 = colMeans(Temp_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg2[is.na(week_avg2)] <- week_avg2[19]
#
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Schmidt, covar2 = Temp, week_avg1 = week_avg1, week_avg2 = week_avg2))
#
# }
#
# #for schmidt_and_wind
# if(model_name == "schmidt_and_wind"){
#
#   #read in covariate 1 data
#   Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
#   #remove 2015-2016 data
#   Schmidt <- Schmidt[-c(7:8),]
#   #center covariate data
#   Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg1 = colMeans(Schmidt, na.rm = TRUE)
#
#   #read in covariate 2 data
#   Wind <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
#   #remove 2015-2016 data
#   Wind <- Wind[-c(7:8),]
#   #center covariate data
#   Wind <- (Wind - mean(Wind, na.rm = TRUE))/sd(Wind, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg2 = colMeans(Wind, na.rm = TRUE)
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Schmidt, covar2 = Wind, week_avg1 = week_avg1, week_avg2 = week_avg2))
#
# }
#
#
# #for temp_and_wind
# if(model_name == "temp_and_wind"){
#
#   #read in covariate 1 data
#   Temp <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv"))
#   #remove 2015-2016 data
#   Temp <- Temp[-c(7:8),]
#   #center covariate data
#   Temp <- (Temp - mean(Temp, na.rm = TRUE))/sd(Temp, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   Temp_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv"))
#   #remove 2015-2016 data
#   Temp_prior <- Temp_prior[-c(7:8),]
#   #center water temp data
#   Temp_prior <- (Temp_prior - mean(Temp_prior, na.rm = TRUE))/sd(Temp_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg1 = colMeans(Temp_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg1[is.na(week_avg1)] <- week_avg1[19]
#
#   #read in covariate 2 data
#   Wind <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
#   #remove 2015-2016 data
#   Wind <- Wind[-c(7:8),]
#   #center covariate data
#   Wind <- (Wind - mean(Wind, na.rm = TRUE))/sd(Wind, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg2 = colMeans(Wind, na.rm = TRUE)
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Temp, covar2 = Wind, week_avg1 = week_avg1, week_avg2 = week_avg2))
#
# }

###############################TWO COVARIATE QUADRATIC MODELS#####################################

#for wtrtemp_min_and_GDD
if(model_name %in% c("wtrtemp_min_and_GDD", "wtrtemp_min_and_GDD_RY")){

  #read in covariate 1 (min water temp)  data from Herrick Cove
  Temp <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_HC.csv")

  #center covariate data
  Temp$HCS.tempC_min_stand <- (Temp$HCS.tempC_min - mean(Temp$HCS.tempC_min, na.rm = TRUE))/sd(Temp$HCS.tempC_min, na.rm = TRUE)

  Temp <- Temp$HCS.tempC_min_stand

  #read in data from Fichter for data gap-filling
  Temp_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_SOTF.csv")

  #center water temp data
  Temp_prior$SOTF.tempC_min_stand <- (Temp_prior$SOTF.tempC_min - mean(Temp_prior$SOTF.tempC_min, na.rm = TRUE))/sd(Temp_prior$SOTF.tempC_min, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  # convert to wide first
  Temp_prior_wide <- Temp_prior %>%
    select(-SOTF.tempC_min) %>%
    pivot_wider(1:3, names_from = season_week, values_from = SOTF.tempC_min_stand)

  week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)

  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19] # not sure if should be 18? more data available

  # repeat 8 times to match long format data
  week_avg_v2 <- as_tibble(rep(week_avg, 8))

  week_avg1 <- week_avg_v2$value

  #read in covariate 2 (GDD) data
  GDD <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_HC.csv")

  #standardize within year to account for different start dates in different years
  # convert to wide first
  GDD_wide <- GDD %>%
    pivot_wider(1:3, names_from = season_week, values_from = hc_gdd_sum)

  GDD_stand <- apply(GDD_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #standardize across years
  GDD_stand_yr <- (GDD_stand - mean(GDD_stand, na.rm = TRUE))/sd(GDD_stand, na.rm = TRUE)

  #transpose - to make full wide and then convert back to full long
   GDD_trans <- t(GDD_stand_yr)

   GDD_long <- as.data.frame(GDD_trans) %>%
     pivot_longer(cols = 1:20, names_to = "season_week", values_to = "hc_gdd_sum", names_transform = list(season_week = as.integer)) %>%
     mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
     mutate(season_weeks = c(1:160)) %>%
    select(year, season_weeks, hc_gdd_sum)

   GDD <- GDD_long$hc_gdd_sum

  #read in data from Fichter for data gap-filling
  GDD_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_SOTF.csv")

  #standardize within year to account for different start dates in different years
  # convert to wide first
  GDD_prior_wide <- GDD_prior %>%
    pivot_wider(1:3, names_from = season_week, values_from = sotf_gdd_sum)

  GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #standardize across years
  GDD_prior_stand_yr <- (GDD_prior_stand - mean(GDD_prior_stand, na.rm = TRUE))/sd(GDD_prior_stand, na.rm = TRUE)

  #transpose - to make full wide and then convert back to full long
  GDD_prior_trans <- t(GDD_prior_stand_yr)

  GDD_prior_long <- as.data.frame(GDD_prior_trans) %>%
    pivot_longer(cols = 1:20, names_to = "season_week", values_to = "sotf_gdd_sum", names_transform = list(season_week = as.integer)) %>%
    mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
    select(year, season_week, sotf_gdd_sum)

  #calculate weekly average of covariate 2 from past years for gap filling
  week_avg2 = colMeans(GDD_prior_trans, na.rm = TRUE)

  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg2[is.na(week_avg2)] <- week_avg2[19]

  # repeat 8 times to match long format data
  week_avg2_v2 <- as_tibble(rep(week_avg2, 8))

  week_avg2 <- week_avg2_v2$value

  return(list(season_weeks = season_weeks, year_no = year_no, y = y, covar1 = Temp, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2)) #removed year_no = year_no,

}

# #for schmidt_and_GDD
# if(model_name == "schmidt_and_GDD"){
#
#   #read in covariate 1 data
#   Schmidt <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv"))
#   #remove 2015-2016 data
#   Schmidt <- Schmidt[-c(7:8),]
#   #center covariate data
#   Schmidt <- (Schmidt - mean(Schmidt, na.rm = TRUE))/sd(Schmidt, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg1 = colMeans(Schmidt, na.rm = TRUE)
#
#   #read in covariate 2 data
#   GDD <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
#   #remove 2015-2016 data
#   GDD <- GDD[-c(7:8),]
#   #standardize within year to account for different start dates in different years
#   GDD <- apply(GDD,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
#   #transpose
#   GDD <- t(GDD)
#   #standardize across years
#   GDD <- (GDD - mean(GDD, na.rm = TRUE))/sd(GDD, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   GDD_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
#   #remove 2015-2016 data
#   GDD_prior <- GDD_prior[-c(7:8),]
#   #standardize within year to account for different start dates in different years
#   GDD_prior <- apply(GDD_prior,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
#   #transpose
#   GDD_prior <- t(GDD_prior)
#   #standardize across years
#   GDD_prior <- (GDD_prior - mean(GDD_prior, na.rm = TRUE))/sd(GDD_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate 2 from past years for gap filling
#   week_avg2 = colMeans(GDD_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg2[is.na(week_avg2)] <- week_avg2[19]
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Schmidt, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2))
#
# }

#for wind_and_GDD
# if(model_name == "wind_and_GDD"){
#
#   #read in covariate 1 data
#   Wind <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv"))
#   #remove 2015-2016 data
#   Wind <- Wind[-c(7:8),]
#   #center covariate data
#   Wind <- (Wind - mean(Wind, na.rm = TRUE))/sd(Wind, na.rm = TRUE)
#
#   #calculate weekly average of covariate from past years for gap filling
#   week_avg1 = colMeans(Wind, na.rm = TRUE)
#
#   #read in covariate 2 data
#   GDD <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv"))
#   #remove 2015-2016 data
#   GDD <- GDD[-c(7:8),]
#   #standardize within year to account for different start dates in different years
#   GDD <- apply(GDD,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
#   #transpose
#   GDD <- t(GDD)
#   #standardize across years
#   GDD <- (GDD - mean(GDD, na.rm = TRUE))/sd(GDD, na.rm = TRUE)
#
#   #read in data from Site 2 for data gap-filling
#   GDD_prior <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv"))
#   #remove 2015-2016 data
#   GDD_prior <- GDD_prior[-c(7:8),]
#   #standardize within year to account for different start dates in different years
#   GDD_prior <- apply(GDD_prior,1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
#   #transpose
#   GDD_prior <- t(GDD_prior)
#   #standardize across years
#   GDD_prior <- (GDD_prior - mean(GDD_prior, na.rm = TRUE))/sd(GDD_prior, na.rm = TRUE)
#
#   #calculate weekly average of covariate 2 from past years for gap filling
#   week_avg2 = colMeans(GDD_prior, na.rm = TRUE)
#   #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
#   week_avg2[is.na(week_avg2)] <- week_avg2[19]
#
#   return(list(year_no = year_no, season_weeks = season_weeks, y = y, covar1 = Wind, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2))
#
# }

}
