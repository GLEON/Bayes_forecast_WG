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
gloeo_nsh <- read_csv("./00_Data_files/Covariate_analysis_data/NSH_Gechinulata_long.csv")
gloeo_sotf <- read_csv("./00_Data_files/Covariate_analysis_data/SOTF_Gechinulata_long.csv")

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
gloeo_all_sites <- bind_cols(gloeo_hc2, gloeo_nb2[,3], gloeo_nsh2[,3], gloeo_sotf2[,3])

write.csv(gloeo_all_sites, "./00_Data_files/Bayesian_model_input_data/Gloeo_AllSites.csv", row.names = FALSE)

############format water temp data
wtr_hc <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_HC.csv")
wtr_nb <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_NB.csv")
wtr_nsh <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_NSH.csv")
wtr_sotf <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_SOTF.csv")

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
mintemp_all_sites <- bind_cols(mintemp_hc, mintemp_nb[,3],  mintemp_nsh[,3],  mintemp_sotf[,3])

write.csv(mintemp_all_sites, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv", row.names = FALSE)

# Use data from 3 sites for min water temp prior
#wtrtempmin_allsites <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv")

wtr_weeklymean <- rowMeans(mintemp_all_sites[,-c(1:2,6)], na.rm = T) # drop year, season week & Fichter data

# Fill NA - Bring back season week
wtr_weeklymean_v2 <- cbind(mintemp_all_sites$year, mintemp_all_sites$season_week, wtr_weeklymean)

colnames(wtr_weeklymean_v2)[1:2] <- c("year", "season_week")

sum(is.na(wtr_weeklymean_v2)) # 16 missing values mostly season weeks 19 & 20 but also 1 &2

missing <- which(is.na(wtr_weeklymean_v2[,3]))

for(i in 1:length(wtr_weeklymean_v2[,3])) {
  if(is.na(wtr_weeklymean_v2[i,3]) & wtr_weeklymean_v2[i,2] %in% c(19:20)) {
    wtr_weeklymean_v2[i,3] <- wtr_weeklymean_v2[i-1,3]

  } else if (is.na(wtr_weeklymean_v2[i,3]) & wtr_weeklymean_v2[i,2] %in% c(2))
    wtr_weeklymean_v2[i,3] <- wtr_weeklymean_v2[i+1,3]


  else{

    if(is.na(wtr_weeklymean_v2[i,3]) & wtr_weeklymean_v2[i,2] %in% c(1)) {
      wtr_weeklymean_v2[i,3] <- wtr_weeklymean_v2[i+1,3]
    }
  }
}

# Missing one value
wtr_weeklymean_v2[121,3] <- 15.51333

write.csv(wtr_weeklymean_v2, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_allsites_prior.csv", row.names = F)


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
gdd_all_sites <- bind_cols(gdd_hc, gdd_nb[,3], gdd_nsh[,3], gdd_sotf[,3])

write.csv(gdd_all_sites, "./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv", row.names = FALSE)

# Calculate weekly avg for all sites except Fichter to use as prior
#gdd_allsites <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv")

gdd_weeklymean <- rowMeans(gdd_all_sites[,-c(1:2,6)], na.rm = T) # drop year, season week & Fichter data

# Fill NA - Bring back season week
gdd_weeklymean_v2 <- cbind(gdd_all_sites$year, gdd_all_sites$season_week, gdd_weeklymean)

colnames(gdd_weeklymean_v2)[1:2] <- c("year", "season_week")

sum(is.na(gdd_weeklymean_v2)) # 16 missing values mostly season weeks 19 & 20 but also 1 &2

missing <- which(is.na(gdd_weeklymean_v2[,3]))

for(i in 1:length(gdd_weeklymean_v2[,3])) {
  if(is.na(gdd_weeklymean_v2[i,3]) & gdd_weeklymean_v2[i,2] %in% c(19:20)) {
    gdd_weeklymean_v2[i,3] <- gdd_weeklymean_v2[i-1,3]

  } else if (is.na(gdd_weeklymean_v2[i,3]) & gdd_weeklymean_v2[i,2] %in% c(2))
    gdd_weeklymean_v2[i,3] <- gdd_weeklymean_v2[i+1,3]


  else{

    if(is.na(gdd_weeklymean_v2[i,3]) & gdd_weeklymean_v2[i,2] %in% c(1)) {
      gdd_weeklymean_v2[i,3] <- gdd_weeklymean_v2[i+1,3]
    }
  }
}

write.csv(gdd_weeklymean_v2, "./00_Data_files/Bayesian_model_input_data/gdd_allsites_prior.csv", row.names = F)


