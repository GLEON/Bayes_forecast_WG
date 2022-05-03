#Title: 3 Format G. echinulata and covariate data for Bayesian models
#Author: Mary Lofton
#Date: 15APR20
# JB edits 24 Jan 2022 to read in Newbury instead of Herrick cove data on line 15
# JB edits in new script to change data structure to stay long - columns = site, driver variables, rows = observations
#Load packages

#run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate)

##############format G. echinulata data
# gloeo <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv") %>% #read_csv("./00_Data_files/Covariate_analysis_data/NB_Gechinulata_long.csv")
#   select(year, season_week, ln_totalperL) %>%
#   spread(key = season_week, value = ln_totalperL) %>%
#   select(-year)
#
# colnames(gloeo) <- paste("wk", colnames(gloeo), sep = "_")
#
# write.csv(gloeo, "./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv", row.names = FALSE)

## Site format of gloeo data
gloeo_hc <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv")
gloeo_nb <- read_csv("./00_Data_files/Covariate_analysis_data/NB_Gechinulata_long.csv")
gloeo_sotf <- read_csv("./00_Data_files/Covariate_analysis_data/SOTF_Gechinulata_long.csv")
gloeo_nsh <- read_csv("./00_Data_files/Covariate_analysis_data/NSH_Gechinulata_long.csv")

# Keep year, season week, gloeo from each site own column

gloeo_hc2 <- gloeo_hc %>%
  select(year, season_week, ln_totalperL) %>%
  rename(hc_gloeo_ln = ln_totalperL)

write.csv(gloeo_hc2, "./00_Data_files/Bayesian_model_input_data/Gloeo_HC.csv", row.names = FALSE)

gloeo_nb2 <- gloeo_nb %>%
  select(year, season_week, ln_totalperL) %>%
  rename(nb_gloeo_ln = ln_totalperL)

gloeo_sotf2 <- gloeo_sotf %>%
  select(year, season_week, ln_totalperL) %>%
  rename(sotf_gloeo_ln = ln_totalperL)

gloeo_nsh2 <- gloeo_nsh %>%
  select(year, season_week, ln_totalperL) %>%
  rename(nsh_gloeo_ln = ln_totalperL)

# Bind together all sites - only keep gloeo columns so only 1 year and season week column
gloeo_all_sites <- bind_cols(gloeo_hc2, gloeo_nb2[,3],  gloeo_sotf2[,3],  gloeo_nsh2[,3])

write.csv(gloeo_all_sites, "./00_Data_files/Bayesian_model_input_data/Gloeo_AllSites.csv", row.names = FALSE)

############format water temp data
wtr_hc <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_HC.csv")
wtr_nb <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_NB.csv")
wtr_sotf <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_SOTF.csv")
wtr_nsh <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_NSH.csv")

# min water temp for All sites

# HC
mintemp_hc <- wtr_hc %>%
  select(date, HCS.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8), year = year(date)) %>%
  select(year, season_week, HCS.tempC_min)

write.csv(mintemp_hc, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_HC.csv", row.names = FALSE)

# NB
mintemp_nb <- wtr_nb %>%
  select(date, NB.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8), year = year(date)) %>%
  select(year, season_week, NB.tempC_min)

# SOTF
mintemp_sotf <- wtr_sotf %>%
  select(date, SOTF.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8), year = year(date)) %>%
  select(year, season_week, SOTF.tempC_min)

write.csv(mintemp_sotf, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_SOTF.csv", row.names = FALSE)

# NSH
mintemp_nsh <- wtr_nsh %>%
  select(date, NSH.tempC_min) %>%
  mutate(season_week = rep(c(1:20),times = 8), year = year(date)) %>%
  select(year, season_week, NSH.tempC_min)

# Bind together all sites - only keep wtrtemp min columns so only 1 year and season week column
mintemp_all_sites <- bind_cols(mintemp_hc, mintemp_nb[,3],  mintemp_sotf[,3],  mintemp_nsh[,3])

write.csv(mintemp_all_sites, "./00_Data_files/Bayesian_model_input_data/wtrmin_AllSites.csv", row.names = FALSE)

# GDD for All sites

#GDD for HC
gdd_hc <- wtr_hc %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),year = year(date)) %>%
  rename(hc_gdd_sum = gdd_sum) %>%
  select(year, season_week, hc_gdd_sum)

write.csv(gdd_hc, "./00_Data_files/Bayesian_model_input_data/GDD_HC.csv", row.names = FALSE)

#GDD for NB
gdd_nb <- wtr_nb %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),year = year(date)) %>%
  rename(nb_gdd_sum = gdd_sum) %>%
  select(year, season_week, nb_gdd_sum)

#GDD for SOTF
gdd_sotf <- wtr_sotf %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),year = year(date)) %>%
  rename(sotf_gdd_sum = gdd_sum) %>%
  select(year, season_week, sotf_gdd_sum)

write.csv(gdd_sotf, "./00_Data_files/Bayesian_model_input_data/GDD_SOTF.csv", row.names = FALSE)

#GDD for NSH
gdd_nsh <- wtr_nsh %>%
  select(date, gdd_sum) %>%
  mutate(season_week = rep(c(1:20),times = 8),year = year(date)) %>%
  rename(nsh_gdd_sum = gdd_sum) %>%
  select(year, season_week, nsh_gdd_sum)

# Bind together all sites - only keep gdd columns so only 1 year and season week column
gdd_all_sites <- bind_cols(gdd_hc, gdd_nb[,3],  gdd_sotf[,3],  gdd_nsh[,3])

write.csv(gdd_all_sites, "./00_Data_files/Bayesian_model_input_data/gdd_AllSites.csv", row.names = FALSE)


# ############format wind data
# wnd <- read_csv("./00_Data_files/Covariate_analysis_data/wind_data_all.csv")
#
# #avg. wind direction with a 2 day lag
# wnd1 <- wnd %>%
#   select(date, AveWindDir_cove_mean_2daylag) %>%
#   mutate(season_week = rep(c(1:20),times = 8),
#          year = year(date)) %>%
#   select(-date) %>%
#   spread(key = season_week, value = AveWindDir_cove_mean_2daylag) %>%
#   select(-year)
#
# colnames(wnd1) <- paste("wk", colnames(wnd1), sep = "_")
#
# write.csv(wnd1, "./00_Data_files/Bayesian_model_input_data/wnd_dir_mean_2daylag.csv", row.names = FALSE)
#
# ##############format precip data
# ppt <- read_csv("./00_Data_files/Covariate_analysis_data/PRISM_precipitation_2009-2016.csv")
#
# # precipitation data
# ppt1 <- ppt %>%
#   select(date, precip_mm) %>%
#   mutate(season_week = rep(c(1:20),times = 8),
#          year = year(date)) %>%
#   select(-date) %>%
#   spread(key = season_week, value = precip_mm) %>%
#   select(-year)
#
# colnames(ppt1) <- paste("wk", colnames(ppt1), sep = "_")
#
# write.csv(ppt1, "./00_Data_files/Bayesian_model_input_data/precip_mm.csv", row.names = FALSE)
