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

# Read in Gloeo data - all sites
gloeo_all <- read_csv("./00_Data_files/Bayesian_model_input_data/Gloeo_AllSites.csv")

# set site, year, season_weeks

y = gloeo_all$hc_gloeo_ln
year_no = as.numeric(as.factor(gloeo_all$year))
season_weeks = c(1:160) # full season weeks instead of 1:20


###############################GLOEO-ONLY MODELS#####################################
#for RW_obs_1site, and DLM_1site
if(model_name %in% c("RW_obs_1site","DLM_1site")){
  return(list(season_weeks = season_weeks, y = y))
}

###############################TWO COVARIATE QUADRATIC MODELS#####################################

#for wtrtemp_min_and_GDD_1site
if(model_name %in% c("wtrtemp_min_and_GDD_1site")){

  #read in covariate 1 (min water temp)  data
  Temp <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv") %>%
    select(year, season_week, HCS.tempC_min) # SELECT SITE

  #center covariate data
  Temp$HCS.tempC_min_stand <- (Temp$HCS.tempC_min - mean(Temp$HCS.tempC_min, na.rm = TRUE))/sd(Temp$HCS.tempC_min, na.rm = TRUE)

  Temp <- Temp$HCS.tempC_min_stand

  #read in data from pre-2009 for data gap-filling
  Temp_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/onset_minwtrtemp_gdd_allsites_pre2009.csv") %>%
    select(year, season_week, HC.tempC_min)

  #center water temp data
  Temp_prior$tempC_min_stand <- (Temp_prior$HC.tempC_min - mean(Temp_prior$HC.tempC_min, na.rm = TRUE))/sd(Temp_prior$HC.tempC_min, na.rm = TRUE)

  #calculate weekly average of covariate from past years for gap filling
  # convert to wide first
  Temp_prior_wide <- Temp_prior %>%
    select(-HC.tempC_min) %>%
    pivot_wider(1:3, names_from = season_week, values_from = tempC_min_stand)

  week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)

  #use weekly average to serve as prior for missing weeks
  week_avg[1:3] <- week_avg[4] #fill missing holes beginning of season weeks with data from week 4
  week_avg[18:20] <- week_avg[17] #fill missing holes end of season weeks with data from week 17

  # repeat 8 times to match long format data
  week_avg_v2 <- as_tibble(rep(week_avg, 8))

  week_avg1 <- week_avg_v2$value

  #read in covariate 2 (GDD) data
  GDD <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv") %>%
    select(year, season_week, hc_gdd_sum) #SELECT SITE

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

  #read in data from pre-2009 for data gap-filling
  GDD_prior <- read_csv("./00_Data_files/Bayesian_model_input_data/onset_minwtrtemp_gdd_allsites_pre2009.csv") %>%
    select(year, season_week, gdd_HC_sum) #SELECT SITE

  #standardize within year to account for different start dates in different years
  # convert to wide first
  GDD_prior_wide <- GDD_prior %>%
    pivot_wider(1:3, names_from = season_week, values_from = gdd_HC_sum)

  GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #standardize across years
  GDD_prior_stand_yr <- (GDD_prior_stand - mean(GDD_prior_stand, na.rm = TRUE))/sd(GDD_prior_stand, na.rm = TRUE)

  #transpose - to make full wide and then convert back to full long
  GDD_prior_trans <- t(GDD_prior_stand_yr)

  GDD_prior_long <- as.data.frame(GDD_prior_trans) %>%
    pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_sum", names_transform = list(season_week = as.integer)) %>%
    mutate(year = rep(2007:2008,times = 1, each = 20)) %>%
    select(year, season_week, gdd_sum)

  #calculate weekly average of covariate 2 from past years for gap filling
  week_avg2 = colMeans(GDD_prior_trans, na.rm = TRUE)

  #use weekly average to serve as prior for missing weeks
  week_avg2[1:3] <- week_avg2[4] #fill missing holes beginning of season weeks with data from week 4
  week_avg2[18:20] <- week_avg2[17] #fill missing holes end of season weeks with data from week 17

  # repeat 8 times to match long format data
  week_avg2_v2 <- as_tibble(rep(week_avg2, 8))

  week_avg2 <- week_avg2_v2$value

  return(list(season_weeks = season_weeks, y = y, covar1 = Temp, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

#for wtrtemp_min_and_GDD with Random Year Effect
if(model_name %in% c("wtrtemp_min_and_GDD_RY")){

  return(list(season_weeks = season_weeks, year_no = year_no, totYr = length(unique(year_no)), y = y, covar1 = Temp, covar2 = GDD, week_avg1 = week_avg1, week_avg2 = week_avg2))

}


}
