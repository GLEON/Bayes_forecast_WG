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

#min water temp
mintemp <- wtr %>%
  select(date, HCS.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = HCS.tempC_min) %>%
  select(-year)

colnames(mintemp) <- paste("wk", colnames(mintemp), sep = "_")

write.csv(mintemp, "./00_Data_files/Bayesian_model_input_data/Min_watertemp_Site1.csv", row.names = FALSE)

#min water temp lag
mintemp_lag <- wtr %>%
  select(date, HCS.tempC_min_lag) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = HCS.tempC_min_lag) %>%
  select(-year)

colnames(mintemp_lag) <- paste("wk", colnames(mintemp_lag), sep = "_")

write.csv(mintemp_lag, "./00_Data_files/Bayesian_model_input_data/Min_watertemp_lag_Site1.csv", row.names = FALSE)

#7 day moving avg water temp
ma7 <- wtr %>%
  select(date, ma_7) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = ma_7) %>%
  select(-year)

colnames(ma7) <- paste("wk", colnames(ma7), sep = "_")

write.csv(ma7, "./00_Data_files/Bayesian_model_input_data/MA_watertemp_7day_Site1.csv", row.names = FALSE)

############format Schmidt stability data
schmidt <- read_csv("./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")

#min water temp
med_diff <- schmidt %>%
  select(date, schmidt.stability_median_diff) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = schmidt.stability_median_diff) %>%
  select(-year)

colnames(med_diff) <- paste("wk", colnames(med_diff), sep = "_")

write.csv(med_diff, "./00_Data_files/Bayesian_model_input_data/Med_diff_Schmidt_Site1.csv", row.names = FALSE)

############format GDD data
gdd <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days.csv")

#min water temp
gdd1 <- gdd %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(-date) %>%
  spread(key = season_week, value = gdd_sum) %>%
  select(-year)

colnames(gdd1) <- paste("wk", colnames(gdd1), sep = "_")

write.csv(gdd1, "./00_Data_files/Bayesian_model_input_data/GDD_Site1.csv", row.names = FALSE)
