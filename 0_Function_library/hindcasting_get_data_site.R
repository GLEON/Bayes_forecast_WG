# Title: Get data for hindcasting model runs include covariate hindcasts to fill missing data and generated driver data uncertainty
# History:
# created MEL 27MAR20, updated JAB 26 July 2022

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse)

get_hindcast_data <- function(model_name){

  # Read in Gloeo data - all sites
  gloeo_all <- read_csv("./00_Data_files/Bayesian_model_input_data/Gloeo_AllSites.csv")

  gloeo_short <- gloeo_all %>%
    filter(year < 2016) %>%
    mutate(hc_gloeo_ln_pred = ifelse(year==2015, NA, hc_gloeo_ln)) %>% # set 2015 to NA to forecast
    mutate(nb_gloeo_ln_pred = ifelse(year==2015, NA, nb_gloeo_ln)) %>%
    mutate(nsh_gloeo_ln_pred = ifelse(year==2015, NA, nsh_gloeo_ln))

  # set year, season_weeks
  year_no = as.numeric(as.factor(gloeo_short$year))
  season_weeks = c(1:140) # full season weeks up to 2015


###############################FOR ALL MODELS#####################################

# GLOEO-ONLY MODELS ####

#for RW_obs and DLM models
if(model_name %in% c("RW_obs_1site","DLM_1site")){
  # Set gloeo site data
  y = gloeo_short$hc_gloeo_ln_pred

  return(list(season_weeks = season_weeks, y = y))
}

# READ IN DATA FOR 2 COVAR MODELS ####

  # for HCS wtrtemp_min_and_airtempGDD with Random Year Effect ####
  if(model_name %in% c("wtrtemp_min_and_airtempGDD_1site_RY_HC")){

    # Set gloeo site data
    y = gloeo_short$hc_gloeo_ln_pred

    #read in covariate 1 (min water temp)  data
    Temp_all <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv") %>%
      select(year, season_week, HCS.tempC_min)  %>%  # SELECT SITE HCS.tempC_min
      filter(year < 2016) # use known driver data

    #center covariate data
    Temp_all$HCS.tempC_min_stand <- (Temp_all$HCS.tempC_min - mean(Temp_all$HCS.tempC_min, na.rm = TRUE))/sd(Temp_all$HCS.tempC_min, na.rm = TRUE)

    Temp <- Temp_all$HCS.tempC_min_stand

    # Create covar hindcast to get at driver uncertainty
    covar1_hindcast <- Temp_all$HCS.tempC_min[1:120] # go up to end of 2014

     #Standardize
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)


    #read in data for site avg for prior
    Temp_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_allsites_prior.csv") %>%
      filter(year < 2016)

    #center water temp data
    Temp_prior$tempC_min_stand <- (Temp_prior$wtr_weeklymean - mean(Temp_prior$wtr_weeklymean, na.rm = TRUE))/sd(Temp_prior$wtr_weeklymean, na.rm = TRUE)

    week_avg1 <- Temp_prior$tempC_min_stand

    #calculate weekly average of covariate from past years for gap filling
    # # convert to wide first
    # Temp_prior_wide <- Temp_prior %>%
    #   select(-NB.tempC_min) %>%
    #   pivot_wider(1:3, names_from = season_week, values_from = tempC_min_stand)
    #
    # week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)
    #
    # #use weekly average to serve as prior for missing weeks
    # week_avg[is.na(week_avg)] <- week_avg[19]
    #
    # # repeat 8 times to match long format data, 6 for short
    # week_avg_v2 <- as_tibble(rep(week_avg, 6))


    #read in covariate 2 air temp GDD data
    GDD_all <- read_csv("./00_Data_files/Bayesian_model_input_data/PRISM_airtemp_GDD_2009-2016.csv") %>%
      select(year, season_week, gdd_hc_sum)  %>% #SELECT SITE
      filter(year < 2016) #use known driver data for 2015

    #standardize within year to account for different start dates in different years
    # convert to wide first
    GDD_wide <- GDD_all %>%
      pivot_wider(1:3, names_from = season_week, values_from = gdd_hc_sum)

    GDD_stand <- apply(GDD_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

    #standardize across years
    GDD_stand_yr <- (GDD_stand - mean(GDD_stand, na.rm = TRUE))/sd(GDD_stand, na.rm = TRUE)

    #transpose - to make full wide and then convert back to full long
    GDD_trans <- t(GDD_stand_yr)

    GDD_long <- as.data.frame(GDD_trans) %>%
      pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
      #  mutate(year = rep(2009:2014,times = 1, each = 20)) %>%
      #  mutate(season_weeks = c(1:120)) %>%
      select(gdd_sum)

    GDD <- GDD_long$gdd_sum

    # Create covar hindcast to get at driver uncertainty
    covar2_hindcast <- GDD_all$gdd_hc_sum[1:120] # go up to end of 2014

    #Standardize
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)


    ### No missing data in GDD air temp so no missing data model
    # #read in data from  Newbury or avg. of other sites
    # GDD_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv") %>%
    #   select(year, season_week, nb_gdd_sum) %>% #SELECT SITE
    #   filter(year < 2015)
    #
    # #standardize within year to account for different start dates in different years
    # # convert to wide first
    # GDD_prior_wide <- GDD_prior %>%
    #   pivot_wider(1:3, names_from = season_week, values_from = nb_gdd_sum)
    #
    # GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #
    # #standardize across years
    # GDD_prior_stand_yr <- (GDD_prior_stand - mean(GDD_prior_stand, na.rm = TRUE))/sd(GDD_prior_stand, na.rm = TRUE)
    #
    # #transpose - to make full wide and then convert back to full long
    # GDD_prior_trans <- t(GDD_prior_stand_yr)
    #
    # GDD_prior_long <- as.data.frame(GDD_prior_trans) %>%
    #   pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
    #   #    mutate(year = rep(2007:2008,times = 1, each = 20)) %>%
    #   select(gdd_sum)
    #
    # #calculate weekly average of covariate for gap filling
    # week_avg2 = colMeans(GDD_prior_trans, na.rm = TRUE)
    #
    # #use weekly average to serve as prior for missing weeks
    # week_avg2[is.na(week_avg2)] <- week_avg2[19]
    #
    # # repeat 8 times to match long format data, 6 for short gloeo
    # week_avg2_v2 <- as_tibble(rep(week_avg2, 6))
    #
    # week_avg2 <- week_avg2_v2$value

    return(list(season_weeks = season_weeks, year_no = year_no, totYr = length(unique(year_no)), y = y, covar1 = Temp, covar2 = GDD, week_avg1 = week_avg1, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast))

  }

  # for NB wtrtemp_min_and_airtempGDD with Random Year Effect ####
  if(model_name %in% c("wtrtemp_min_and_airtempGDD_1site_RY_NB")){

    # Set gloeo site data
    y = gloeo_short$nb_gloeo_ln_pred

    #read in covariate 1 (min water temp)  data
    Temp_all <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv") %>%
      select(year, season_week, NB.tempC_min)  %>%  # SELECT SITE HCS.tempC_min
      filter(year < 2016) # use known driver data

    #center covariate data
    Temp_all$NB.tempC_min_stand <- (Temp_all$NB.tempC_min - mean(Temp_all$NB.tempC_min, na.rm = TRUE))/sd(Temp_all$NB.tempC_min, na.rm = TRUE)

    Temp <- Temp_all$NB.tempC_min_stand

    # Create covar hindcast to get at driver uncertainty
    covar1_hindcast <- Temp_all$NB.tempC_min[1:120] # go up to end of 2014

    #Standardize
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)


    #read in data for site avg for prior
    Temp_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_allsites_prior.csv") %>%
      filter(year < 2016)

    #center water temp data
    Temp_prior$tempC_min_stand <- (Temp_prior$wtr_weeklymean - mean(Temp_prior$wtr_weeklymean, na.rm = TRUE))/sd(Temp_prior$wtr_weeklymean, na.rm = TRUE)

    week_avg1 <- Temp_prior$tempC_min_stand

    #calculate weekly average of covariate from past years for gap filling
    # # convert to wide first
    # Temp_prior_wide <- Temp_prior %>%
    #   select(-NB.tempC_min) %>%
    #   pivot_wider(1:3, names_from = season_week, values_from = tempC_min_stand)
    #
    # week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)
    #
    # #use weekly average to serve as prior for missing weeks
    # week_avg[is.na(week_avg)] <- week_avg[19]
    #
    # # repeat 8 times to match long format data, 6 for short
    # week_avg_v2 <- as_tibble(rep(week_avg, 6))


    #read in covariate 2 air temp GDD data
    GDD_all <- read_csv("./00_Data_files/Bayesian_model_input_data/PRISM_airtemp_GDD_2009-2016.csv") %>%
      select(year, season_week, gdd_nb_sum)  %>% #SELECT SITE
      filter(year < 2016) #use known driver data for 2015

    #standardize within year to account for different start dates in different years
    # convert to wide first
    GDD_wide <- GDD_all %>%
      pivot_wider(1:3, names_from = season_week, values_from = gdd_nb_sum)

    GDD_stand <- apply(GDD_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

    #standardize across years
    GDD_stand_yr <- (GDD_stand - mean(GDD_stand, na.rm = TRUE))/sd(GDD_stand, na.rm = TRUE)

    #transpose - to make full wide and then convert back to full long
    GDD_trans <- t(GDD_stand_yr)

    GDD_long <- as.data.frame(GDD_trans) %>%
      pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
      #  mutate(year = rep(2009:2014,times = 1, each = 20)) %>%
      #  mutate(season_weeks = c(1:120)) %>%
      select(gdd_sum)

    GDD <- GDD_long$gdd_sum

    # Create covar hindcast to get at driver uncertainty
    covar2_hindcast <- GDD_all$gdd_nb_sum[1:120] # go up to end of 2014

    #Standardize
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)


    ### No missing data in GDD air temp so no missing data model
    # #read in data from  Newbury or avg. of other sites
    # GDD_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv") %>%
    #   select(year, season_week, nb_gdd_sum) %>% #SELECT SITE
    #   filter(year < 2015)
    #
    # #standardize within year to account for different start dates in different years
    # # convert to wide first
    # GDD_prior_wide <- GDD_prior %>%
    #   pivot_wider(1:3, names_from = season_week, values_from = nb_gdd_sum)
    #
    # GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #
    # #standardize across years
    # GDD_prior_stand_yr <- (GDD_prior_stand - mean(GDD_prior_stand, na.rm = TRUE))/sd(GDD_prior_stand, na.rm = TRUE)
    #
    # #transpose - to make full wide and then convert back to full long
    # GDD_prior_trans <- t(GDD_prior_stand_yr)
    #
    # GDD_prior_long <- as.data.frame(GDD_prior_trans) %>%
    #   pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
    #   #    mutate(year = rep(2007:2008,times = 1, each = 20)) %>%
    #   select(gdd_sum)
    #
    # #calculate weekly average of covariate for gap filling
    # week_avg2 = colMeans(GDD_prior_trans, na.rm = TRUE)
    #
    # #use weekly average to serve as prior for missing weeks
    # week_avg2[is.na(week_avg2)] <- week_avg2[19]
    #
    # # repeat 8 times to match long format data, 6 for short gloeo
    # week_avg2_v2 <- as_tibble(rep(week_avg2, 6))
    #
    # week_avg2 <- week_avg2_v2$value

    return(list(season_weeks = season_weeks, year_no = year_no, totYr = length(unique(year_no)), y = y, covar1 = Temp, covar2 = GDD, week_avg1 = week_avg1, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast))

  }

  # for NSH wtrtemp_min_and_airtempGDD with Random Year Effect ####
  if(model_name %in% c("wtrtemp_min_and_airtempGDD_1site_RY_NSH")){

    # Set gloeo site data
    y = gloeo_short$nsh_gloeo_ln_pred

    #read in covariate 1 (min water temp)  data
    Temp_all <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv") %>%
      select(year, season_week, NSH.tempC_min)  %>%  # SELECT SITE HCS.tempC_min
      filter(year < 2016) # use known driver data

    #center covariate data
    Temp_all$NSH.tempC_min_stand <- (Temp_all$NSH.tempC_min - mean(Temp_all$NSH.tempC_min, na.rm = TRUE))/sd(Temp_all$NSH.tempC_min, na.rm = TRUE)

    Temp <- Temp_all$NSH.tempC_min_stand

    # Create covar hindcast to get at driver uncertainty
    covar1_hindcast <- Temp_all$NSH.tempC_min[1:120] # go up to end of 2014

    #Standardize
    covar1_hindcast <- (covar1_hindcast - mean(covar1_hindcast, na.rm = TRUE))/sd(covar1_hindcast, na.rm = TRUE)


    #read in data for site avg for prior
    Temp_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_allsites_prior.csv") %>%
      filter(year < 2016)

    #center water temp data
    Temp_prior$tempC_min_stand <- (Temp_prior$wtr_weeklymean - mean(Temp_prior$wtr_weeklymean, na.rm = TRUE))/sd(Temp_prior$wtr_weeklymean, na.rm = TRUE)

    week_avg1 <- Temp_prior$tempC_min_stand

    #calculate weekly average of covariate from past years for gap filling
    # # convert to wide first
    # Temp_prior_wide <- Temp_prior %>%
    #   select(-NB.tempC_min) %>%
    #   pivot_wider(1:3, names_from = season_week, values_from = tempC_min_stand)
    #
    # week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)
    #
    # #use weekly average to serve as prior for missing weeks
    # week_avg[is.na(week_avg)] <- week_avg[19]
    #
    # # repeat 8 times to match long format data, 6 for short
    # week_avg_v2 <- as_tibble(rep(week_avg, 6))


    #read in covariate 2 air temp GDD data
    GDD_all <- read_csv("./00_Data_files/Bayesian_model_input_data/PRISM_airtemp_GDD_2009-2016.csv") %>%
      select(year, season_week, gdd_nsh_sum)  %>% #SELECT SITE
      filter(year < 2016) #use known driver data for 2015

    #standardize within year to account for different start dates in different years
    # convert to wide first
    GDD_wide <- GDD_all %>%
      pivot_wider(1:3, names_from = season_week, values_from = gdd_nsh_sum)

    GDD_stand <- apply(GDD_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

    #standardize across years
    GDD_stand_yr <- (GDD_stand - mean(GDD_stand, na.rm = TRUE))/sd(GDD_stand, na.rm = TRUE)

    #transpose - to make full wide and then convert back to full long
    GDD_trans <- t(GDD_stand_yr)

    GDD_long <- as.data.frame(GDD_trans) %>%
      pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
      #  mutate(year = rep(2009:2014,times = 1, each = 20)) %>%
      #  mutate(season_weeks = c(1:120)) %>%
      select(gdd_sum)

    GDD <- GDD_long$gdd_sum

    # Create covar hindcast to get at driver uncertainty
    covar2_hindcast <- GDD_all$gdd_nsh_sum[1:120] # go up to end of 2014

    #Standardize
    covar2_hindcast <- (covar2_hindcast - mean(covar2_hindcast, na.rm = TRUE))/sd(covar2_hindcast, na.rm = TRUE)


    ### No missing data in GDD air temp so no missing data model
    # #read in data from  Newbury or avg. of other sites
    # GDD_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv") %>%
    #   select(year, season_week, nb_gdd_sum) %>% #SELECT SITE
    #   filter(year < 2015)
    #
    # #standardize within year to account for different start dates in different years
    # # convert to wide first
    # GDD_prior_wide <- GDD_prior %>%
    #   pivot_wider(1:3, names_from = season_week, values_from = nb_gdd_sum)
    #
    # GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})
    #
    # #standardize across years
    # GDD_prior_stand_yr <- (GDD_prior_stand - mean(GDD_prior_stand, na.rm = TRUE))/sd(GDD_prior_stand, na.rm = TRUE)
    #
    # #transpose - to make full wide and then convert back to full long
    # GDD_prior_trans <- t(GDD_prior_stand_yr)
    #
    # GDD_prior_long <- as.data.frame(GDD_prior_trans) %>%
    #   pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
    #   #    mutate(year = rep(2007:2008,times = 1, each = 20)) %>%
    #   select(gdd_sum)
    #
    # #calculate weekly average of covariate for gap filling
    # week_avg2 = colMeans(GDD_prior_trans, na.rm = TRUE)
    #
    # #use weekly average to serve as prior for missing weeks
    # week_avg2[is.na(week_avg2)] <- week_avg2[19]
    #
    # # repeat 8 times to match long format data, 6 for short gloeo
    # week_avg2_v2 <- as_tibble(rep(week_avg2, 6))
    #
    # week_avg2 <- week_avg2_v2$value

    return(list(season_weeks = season_weeks, year_no = year_no, totYr = length(unique(year_no)), y = y, covar1 = Temp, covar2 = GDD, week_avg1 = week_avg1, covar1_hindcast = covar1_hindcast, covar2_hindcast = covar2_hindcast))

  }


}
