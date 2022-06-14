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

#read in Gloeo data
gloeo_all <- read_csv("./00_Data_files/Bayesian_model_input_data/Gloeo_AllSites.csv")

# Arrange site order to be hc, nb, nsh, sotf (alphabetical)

gloeo_all_v2 <- gloeo_all %>%
  mutate(season_weeks = 1:160) %>%
  select(year,season_weeks, hc_gloeo_ln, nb_gloeo_ln, nsh_gloeo_ln, sotf_gloeo_ln)

# Remove 1 site
gloeo_3sites <-  gloeo_all_v2 %>%
  select(-sotf_gloeo_ln) # removed SOTF Fichter

#set calibration years and weeks of season and sites

y = as.matrix(gloeo_3sites[,3:5])
year_no = as.numeric(as.factor(gloeo_3sites$year))
season_weeks = gloeo_3sites$season_weeks # full season weeks instead of 1:20
site_no = c(1:3)

###############################TWO COVARIATE QUADRATIC MODELS#####################################

#for wtrtemp_min_and_GDD 3 sites
if(model_name %in% c("wtrtemp_min_and_GDD_3sites")){

  #read in covariate 1 (min water temp)  data from all 3 sites
  Temp <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv")

  #center covariate data
  Temp$HCS.tempC_min_stand <- (Temp$HCS.tempC_min - mean(Temp$HCS.tempC_min, na.rm = TRUE))/sd(Temp$HCS.tempC_min, na.rm = TRUE)

  Temp$NB.tempC_min_stand <- (Temp$NB.tempC_min - mean(Temp$NB.tempC_min, na.rm = TRUE))/sd(Temp$NB.tempC_min, na.rm = TRUE)


  Temp$NSH.tempC_min_stand <- (Temp$NSH.tempC_min - mean(Temp$NSH.tempC_min, na.rm = TRUE))/sd(Temp$NSH.tempC_min, na.rm = TRUE)

  Temp$SOTF.tempC_min_stand <- (Temp$SOTF.tempC_min - mean(Temp$SOTF.tempC_min, na.rm = TRUE))/sd(Temp$SOTF.tempC_min, na.rm = TRUE)

  # 3 sites
  Temp_3sites <- as.matrix(Temp[,7:9])

  # use average of 3 sites as prior for gap filling
  Temp_avg <- rowMeans(Temp[,7:9], na.rm = T)

  Temp_avg_v2 <- bind_cols(Temp$year, Temp$season_week,Temp_avg)

  colnames(Temp_avg_v2) <- c("year", "season_week", "Temp_avg")

  #calculate weekly average of covariate from past years for gap filling
  # convert to wide first
  Temp_prior_wide <- Temp_avg_v2 %>%
    pivot_wider(1:3, names_from = season_week, values_from = Temp_avg)

  week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)

  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19] # not sure if should be 18? more data available

  # repeat 8 times to match long format data, 24 for 3 sites
  week_avg_v2 <- as_tibble(rep(week_avg, 8))

  week_avg1 <- week_avg_v2$value

  #read in covariate 2 (GDD) data
  GDD <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv")

  #standardize within year to account for different start dates in different years
  # convert to wide first for each site

  # HC
  GDD_wide_hc <- GDD %>%
    pivot_wider(1:3, names_from = season_week, values_from = hc_gdd_sum)

  GDD_stand <- apply(GDD_wide_hc[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  GDD_trans <- t(GDD_stand)

  #standardize across years
  GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)

  #transpose - to make full wide and then convert back to full long


   GDD_long <- as.data.frame(GDD_stand_yr) %>%
     pivot_longer(cols = 1:20, names_to = "season_week", values_to = "hc_gdd_sum", names_transform = list(season_week = as.integer)) %>%
     mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
     mutate(season_weeks = c(1:160)) %>%
    select(year, season_weeks, hc_gdd_sum)

   GDD_hc <- GDD_long$hc_gdd_sum

   # NB
   GDD_wide_nb <- GDD %>%
     pivot_wider(c(1:2,4), names_from = season_week, values_from = nb_gdd_sum)

   GDD_stand <- apply(GDD_wide_nb[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

   GDD_trans <- t(GDD_stand)

   #standardize across years
   GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)

   #transpose - to make full wide and then convert back to full long

   GDD_long <- as.data.frame(GDD_stand_yr) %>%
     pivot_longer(cols = 1:20, names_to = "season_week", values_to = "nb_gdd_sum", names_transform = list(season_week = as.integer)) %>%
     mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
     mutate(season_weeks = c(1:160)) %>%
     select(year, season_weeks, nb_gdd_sum)

   GDD_nb <- GDD_long$nb_gdd_sum

   # NSH
   GDD_wide_nsh <- GDD %>%
     pivot_wider(c(1:2,6), names_from = season_week, values_from = nsh_gdd_sum)

   GDD_stand <- apply(GDD_wide_nsh[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

   #transpose - to make full wide and then convert back to full long
   GDD_trans <- t(GDD_stand)

   #standardize across years
   GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)

   GDD_long <- as.data.frame(GDD_stand_yr) %>%
     pivot_longer(cols = 1:20, names_to = "season_week", values_to = "nsh_gdd_sum", names_transform = list(season_week = as.integer)) %>%
     mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
     mutate(season_weeks = c(1:160)) %>%
     select(year, season_weeks, nsh_gdd_sum)

   GDD_nsh <- GDD_long$nsh_gdd_sum

   # SOTF
   GDD_wide_sotf <- GDD %>%
     pivot_wider(c(1:2,5), names_from = season_week, values_from = sotf_gdd_sum)

   GDD_stand <- apply(GDD_wide_sotf[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

   #transpose - to make full wide and then convert back to full long
   GDD_trans <- t(GDD_stand)

   #standardize across years
   GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)


   GDD_long <- as.data.frame(GDD_stand_yr) %>%
     pivot_longer(cols = 1:20, names_to = "season_week", values_to = "sotf_gdd_sum", names_transform = list(season_week = as.integer)) %>%
     mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
     mutate(season_weeks = c(1:160)) %>%
     select(year, season_weeks, sotf_gdd_sum)

   GDD_sotf <- GDD_long$sotf_gdd_sum


   # Combine 3 sites gdd data in same order as gloeo
   GDD_all <- bind_rows("GDD_hc" = GDD_hc, "GDD_nb" = GDD_nb, "GDD_nsh" = GDD_nsh, "GDD_sotf" = GDD_sotf)

   GDD_3sites <-  as.matrix(GDD_all[,1:3]) # drop Fichter

  ### take avg of 3 sites for data gap-filling ###
   GDD_avg <- rowMeans(GDD_all[,1:3], na.rm = T)

   GDD_avg_v2 <- bind_cols(Temp$year, Temp$season_week, GDD_avg)

   colnames(GDD_avg_v2) <- c("year", "season_week", "GDD_avg")

  #standardize within year to account for different start dates in different years
  # convert to wide first
  GDD_prior_wide <- GDD_avg_v2 %>%
    pivot_wider(1:3, names_from = season_week, values_from = GDD_avg)

  GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #transpose - to make full wide and then convert back to full long
  GDD_prior_trans <- t(GDD_prior_stand)

  #standardize across years
  GDD_prior_stand_yr <- (GDD_prior_trans - mean(GDD_prior_trans, na.rm = TRUE))/sd(GDD_prior_trans, na.rm = TRUE)


  # GDD_prior_long <- as.data.frame(GDD_prior_stand_yr) %>%
  #   pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_avg", names_transform = list(season_week = as.integer)) %>%
  #   mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
  #   select(year, season_week, gdd_avg)

  #calculate weekly average of covariate 2 from past years for gap filling
  week_avg2 = colMeans(GDD_prior_stand_yr, na.rm = TRUE)

  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg2[is.na(week_avg2)] <- week_avg2[19]

  # repeat 8 times to match long format data
  week_avg2_v2 <- as_tibble(rep(week_avg2, 8))

  week_avg2 <- week_avg2_v2$value

  return(list(season_weeks = season_weeks, year_no = year_no, site_no = site_no, y = y, covar1 = Temp_3sites, covar2 = GDD_3sites, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

# output <- cbind(gloeo_all_v2, Temp[,7:10], GDD_all, week_avg1, week_avg2)
#
# write_csv(output, "./00_Data_files/Bayesian_model_input_data/data_4site_model_format.csv")

#for wtrtemp_min_and_GDD 3 sites & Random Year
if(model_name %in% c("wtrtemp_min_and_GDD_3sites_RY")){

  #read in covariate 1 (min water temp)  data from all 3 sites
  Temp <- read_csv("./00_Data_files/Bayesian_model_input_data/wtrtemp_min_AllSites.csv")

  #center covariate data
  Temp$HCS.tempC_min_stand <- (Temp$HCS.tempC_min - mean(Temp$HCS.tempC_min, na.rm = TRUE))/sd(Temp$HCS.tempC_min, na.rm = TRUE)

  Temp$NB.tempC_min_stand <- (Temp$NB.tempC_min - mean(Temp$NB.tempC_min, na.rm = TRUE))/sd(Temp$NB.tempC_min, na.rm = TRUE)


  Temp$NSH.tempC_min_stand <- (Temp$NSH.tempC_min - mean(Temp$NSH.tempC_min, na.rm = TRUE))/sd(Temp$NSH.tempC_min, na.rm = TRUE)

  Temp$SOTF.tempC_min_stand <- (Temp$SOTF.tempC_min - mean(Temp$SOTF.tempC_min, na.rm = TRUE))/sd(Temp$SOTF.tempC_min, na.rm = TRUE)

  # 3 sites
  Temp_3sites <- as.matrix(Temp[,7:9])

  # use average of 3 sites as prior for gap filling
  Temp_avg <- rowMeans(Temp[,7:9], na.rm = T)

  Temp_avg_v2 <- bind_cols(Temp$year, Temp$season_week,Temp_avg)

  colnames(Temp_avg_v2) <- c("year", "season_week", "Temp_avg")

  #calculate weekly average of covariate from past years for gap filling
  # convert to wide first
  Temp_prior_wide <- Temp_avg_v2 %>%
    pivot_wider(1:3, names_from = season_week, values_from = Temp_avg)

  week_avg = colMeans(Temp_prior_wide[,-1], na.rm = TRUE)

  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg[is.na(week_avg)] <- week_avg[19] # not sure if should be 18? more data available

  # repeat 8 times to match long format data, 24 for 3 sites
  week_avg_v2 <- as_tibble(rep(week_avg, 8))

  week_avg1 <- week_avg_v2$value

  #read in covariate 2 (GDD) data
  GDD <- read_csv("./00_Data_files/Bayesian_model_input_data/GDD_AllSites.csv")

  #standardize within year to account for different start dates in different years
  # convert to wide first for each site

  # HC
  GDD_wide_hc <- GDD %>%
    pivot_wider(1:3, names_from = season_week, values_from = hc_gdd_sum)

  GDD_stand <- apply(GDD_wide_hc[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  GDD_trans <- t(GDD_stand)

  #standardize across years
  GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)

  #transpose - to make full wide and then convert back to full long


  GDD_long <- as.data.frame(GDD_stand_yr) %>%
    pivot_longer(cols = 1:20, names_to = "season_week", values_to = "hc_gdd_sum", names_transform = list(season_week = as.integer)) %>%
    mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
    mutate(season_weeks = c(1:160)) %>%
    select(year, season_weeks, hc_gdd_sum)

  GDD_hc <- GDD_long$hc_gdd_sum

  # NB
  GDD_wide_nb <- GDD %>%
    pivot_wider(c(1:2,4), names_from = season_week, values_from = nb_gdd_sum)

  GDD_stand <- apply(GDD_wide_nb[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  GDD_trans <- t(GDD_stand)

  #standardize across years
  GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)

  #transpose - to make full wide and then convert back to full long

  GDD_long <- as.data.frame(GDD_stand_yr) %>%
    pivot_longer(cols = 1:20, names_to = "season_week", values_to = "nb_gdd_sum", names_transform = list(season_week = as.integer)) %>%
    mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
    mutate(season_weeks = c(1:160)) %>%
    select(year, season_weeks, nb_gdd_sum)

  GDD_nb <- GDD_long$nb_gdd_sum

  # NSH
  GDD_wide_nsh <- GDD %>%
    pivot_wider(c(1:2,6), names_from = season_week, values_from = nsh_gdd_sum)

  GDD_stand <- apply(GDD_wide_nsh[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #transpose - to make full wide and then convert back to full long
  GDD_trans <- t(GDD_stand)

  #standardize across years
  GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)

  GDD_long <- as.data.frame(GDD_stand_yr) %>%
    pivot_longer(cols = 1:20, names_to = "season_week", values_to = "nsh_gdd_sum", names_transform = list(season_week = as.integer)) %>%
    mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
    mutate(season_weeks = c(1:160)) %>%
    select(year, season_weeks, nsh_gdd_sum)

  GDD_nsh <- GDD_long$nsh_gdd_sum

  # SOTF
  GDD_wide_sotf <- GDD %>%
    pivot_wider(c(1:2,5), names_from = season_week, values_from = sotf_gdd_sum)

  GDD_stand <- apply(GDD_wide_sotf[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #transpose - to make full wide and then convert back to full long
  GDD_trans <- t(GDD_stand)

  #standardize across years
  GDD_stand_yr <- (GDD_trans - mean(GDD_trans, na.rm = TRUE))/sd(GDD_trans, na.rm = TRUE)


  GDD_long <- as.data.frame(GDD_stand_yr) %>%
    pivot_longer(cols = 1:20, names_to = "season_week", values_to = "sotf_gdd_sum", names_transform = list(season_week = as.integer)) %>%
    mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
    mutate(season_weeks = c(1:160)) %>%
    select(year, season_weeks, sotf_gdd_sum)

  GDD_sotf <- GDD_long$sotf_gdd_sum


  # Combine 3 sites gdd data in same order as gloeo
  GDD_all <- bind_rows("GDD_hc" = GDD_hc, "GDD_nb" = GDD_nb, "GDD_nsh" = GDD_nsh, "GDD_sotf" = GDD_sotf)

  GDD_3sites <-  as.matrix(GDD_all[,1:3]) # drop Fichter

  ### take avg of 3 sites for data gap-filling ###
  GDD_avg <- rowMeans(GDD_all[,1:3], na.rm = T)

  GDD_avg_v2 <- bind_cols(Temp$year, Temp$season_week, GDD_avg)

  colnames(GDD_avg_v2) <- c("year", "season_week", "GDD_avg")

  #standardize within year to account for different start dates in different years
  # convert to wide first
  GDD_prior_wide <- GDD_avg_v2 %>%
    pivot_wider(1:3, names_from = season_week, values_from = GDD_avg)

  GDD_prior_stand <- apply(GDD_prior_wide[,-1],1,function(x) {(x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)})

  #transpose - to make full wide and then convert back to full long
  GDD_prior_trans <- t(GDD_prior_stand)

  #standardize across years
  GDD_prior_stand_yr <- (GDD_prior_trans - mean(GDD_prior_trans, na.rm = TRUE))/sd(GDD_prior_trans, na.rm = TRUE)


  # GDD_prior_long <- as.data.frame(GDD_prior_stand_yr) %>%
  #   pivot_longer(cols = 1:20, names_to = "season_week", values_to = "gdd_avg", names_transform = list(season_week = as.integer)) %>%
  #   mutate(year = rep(2009:2016,times = 1, each = 20)) %>%
  #   select(year, season_week, gdd_avg)

  #calculate weekly average of covariate 2 from past years for gap filling
  week_avg2 = colMeans(GDD_prior_stand_yr, na.rm = TRUE)

  #use weekly average from last sampled week (18) to serve as prior for weeks 19 & 20
  week_avg2[is.na(week_avg2)] <- week_avg2[19]

  # repeat 8 times to match long format data
  week_avg2_v2 <- as_tibble(rep(week_avg2, 8))

  week_avg2 <- week_avg2_v2$value

  return(list(season_weeks = season_weeks, year_no = year_no,  totYr = length(unique(year_no)), site_no = site_no, y = y, covar1 = Temp_3sites, covar2 = GDD_3sites, week_avg1 = week_avg1, week_avg2 = week_avg2))

}

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
