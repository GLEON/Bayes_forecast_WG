#Title: 3 Format G. echinulata and covariate data for Bayesian models
#Author: Mary Lofton
#Date: 15APR20

#Load packages

#run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate)

##############format G. echinulata data
gloeo <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv") %>%
  select(year, season_week, ln_totalperL) %>%
  spread(key = season_week, value = ln_totalperL) %>%
  select(-year)

colnames(gloeo) <- paste("wk", colnames(gloeo), sep = "_")

write.csv(gloeo, "./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv", row.names = FALSE)

############format water temp data
wtr <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all.csv")
wtr2 <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_SOTF.csv")

#min water temp for Site 1
mintemp <- wtr %>%
  select(date, HCS.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = HCS.tempC_min) %>%
  select(-year)

colnames(mintemp) <- paste("wk", colnames(mintemp), sep = "_")

write.csv(mintemp, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv", row.names = FALSE)

#min water temp for Site 2
mintemp2 <- wtr2 %>%
  select(date, SOTF.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = SOTF.tempC_min) %>%
  select(-year)

colnames(mintemp2) <- paste("wk", colnames(mintemp2), sep = "_")

write.csv(mintemp2, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv", row.names = FALSE)

#min water temp lag for Site 1
mintemp_lag <- wtr %>%
  select(date, HCS.tempC_min_lag) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = HCS.tempC_min_lag) %>%
  select(-year)

colnames(mintemp_lag) <- paste("wk", colnames(mintemp_lag), sep = "_")

write.csv(mintemp_lag, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site1.csv", row.names = FALSE)

#min water temp lag for Site 2
mintemp_lag2 <- wtr2 %>%
  select(date, SOTF.tempC_min_lag) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = SOTF.tempC_min_lag) %>%
  select(-year)

colnames(mintemp_lag2) <- paste("wk", colnames(mintemp_lag2), sep = "_")

write.csv(mintemp_lag2, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_lag_Site2.csv", row.names = FALSE)

#7 day moving avg water temp
#Site 1
ma7 <- wtr %>%
  select(date, ma_7) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = ma_7) %>%
  select(-year)

colnames(ma7) <- paste("wk", colnames(ma7), sep = "_")

write.csv(ma7, "./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site1.csv", row.names = FALSE)

#Site 2
ma7_2 <- wtr2 %>%
  select(date, ma_7) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = ma_7) %>%
  select(-year)

colnames(ma7_2) <- paste("wk", colnames(ma7_2), sep = "_")

write.csv(ma7_2, "./00_Data_files/Bayesian_model_input_data/wtrtemp_MA7_Site2.csv", row.names = FALSE)

############format Schmidt stability data
schmidt <- read_csv("./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")

#1 week difference in median Schmidt stability
med_diff <- schmidt %>%
  select(date, schmidt.stability_median_diff) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = schmidt.stability_median_diff) %>%
  select(-year)

colnames(med_diff) <- paste("wk", colnames(med_diff), sep = "_")

write.csv(med_diff, "./00_Data_files/Bayesian_model_input_data/schmidt_med_diff.csv", row.names = FALSE)

#1 week lag in maximum Schmidt stability
max_lag <- schmidt %>%
  select(date, schmidt.stability_max_lag) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = schmidt.stability_max_lag) %>%
  select(-year)

colnames(max_lag) <- paste("wk", colnames(max_lag), sep = "_")

write.csv(max_lag, "./00_Data_files/Bayesian_model_input_data/schmidt_max_lag.csv", row.names = FALSE)

############format GDD data
gdd <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days.csv")
gdd2 <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days_SOTF.csv")

#GDD for Site 1
gdd1 <- gdd %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = gdd_sum) %>%
  select(-year)

colnames(gdd1) <- paste("wk", colnames(gdd1), sep = "_")

write.csv(gdd1, "./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv", row.names = FALSE)

#GDD for Site 2
gdd3 <- gdd2 %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = gdd_sum) %>%
  select(-year)

colnames(gdd3) <- paste("wk", colnames(gdd3), sep = "_")

write.csv(gdd3, "./00_Data_files/Bayesian_model_input_data/GDD_Site2.csv", row.names = FALSE)

############format wind data
wnd <- read_csv("./00_Data_files/Covariate_analysis_data/wind_speed_data_all_combined.csv")

#avg. wind direction with a 2 day lag
wnd1 <- wnd %>%
  select(date, AveWindDir_cove_mean_2daylag) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = AveWindDir_cove_mean_2daylag) %>%
  select(-year)

colnames(wnd1) <- paste("wk", colnames(wnd1), sep = "_")

write.csv(wnd1, "./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv", row.names = FALSE)

wnd <- read_csv("./00_Data_files/Covariate_analysis_data/wind_speed_data_all_combined.csv")

#min. wind speed with a 3 day lag
wnd2 <- wnd %>%
  select(date, AveWindSp_ms_min_3daylag) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = AveWindSp_ms_min_3daylag) %>%
  select(-year)

colnames(wnd2) <- paste("wk", colnames(wnd2), sep = "_")

write.csv(wnd2, "./00_Data_files/Bayesian_model_input_data/wnd_sp_mean_3daylag.csv", row.names = FALSE)

