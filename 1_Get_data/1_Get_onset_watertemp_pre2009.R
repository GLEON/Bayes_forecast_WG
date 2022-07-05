# Script to download and wrangle Onset water temp data for pre 2009 from EDI
# Last updated 2022 June 29 - JB
# Data used for water temp prior

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, openair, zoo)

# Download data from EDI to local folder ####

# High-frequency temperature data from four near-shore sites, Lake Sunapee, NH, USA, 2006-2018
# EDI Package ID: edi.498.1
# Citation: Cottingham, K.L., C.C. Carey, and K.C. Weathers. 2020. High-frequency temperature data from four near-shore sites, Lake Sunapee, NH, USA, 2006-2018 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/3e325757f0e981d91cd297f257f05f55. Accessed 2020-05-21.

data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.498.1&entityid=b4f60789ceb87db613924ca43a2f71ed"

destination <- "./00_Data_files/EDI_data_clones/temp_2006-2018_QAQC_vert_09May2020.csv"

download.file(data,destfile = destination, method='libcurl')

# Load onset water temp data into R ####
wtr <- read_csv("./00_Data_files/EDI_data_clones/temp_2006-2018_QAQC_vert_09May2020.csv",
                col_types = list(year = col_double(),
                                 dayofyr = col_double(),
                                 time = col_time(),
                                 datetime = col_datetime(),
                                 site = col_character(),
                                 temp_degC = col_double()))

# Remove outlier values in HerrickCove 2008 data @ beginning of time series (logger out of water)
wtr_pre <- wtr %>%
  filter(year %in% 2007:2008) %>%
  filter(site != "HerrickCoveSouth")

wtr_hc <- wtr %>%
  filter(year %in% 2007:2008) %>%
  filter(site=="HerrickCoveSouth")

# Bad HC data pre 	2008-06-18 11:00:00
# Jump @ 2008-06-19 11:00:00, 12, 13 - adjust time by 5

wtr_hc2 <- wtr_hc %>%
  mutate(temp_degC2 = ifelse(datetime > "2007-09-20 08:00:00" & datetime < "2008-06-18 06:00:00", NA, temp_degC)) %>%
  mutate(temp_degC3 = ifelse(datetime > "2008-06-19 05:00:00" & datetime < "2008-06-19 09:00:00", NA, temp_degC2)) %>%
  select(-c(temp_degC,temp_degC2)) %>%
  rename(temp_degC = temp_degC3)

# Join HC data back together with other sites
wtr_pre2 <- full_join(wtr_pre, wtr_hc2, by = c("year", "dayofyr", "time", "datetime", "site", "temp_degC")) %>%
  arrange(site, datetime) %>%
  pivot_wider(names_from = site, values_from = temp_degC)

# Add full time series to data to be able to calculate daily averages but cut-off days with water temp for only part of day
full_datetime <- seq(from = ymd_hms("2007-06-30 00:00:00"), to = ymd_hms("2008-09-16 23:00:00"), by = dhours(1))

full_datetime.df <- as_tibble_col(full_datetime, column_name = "datetime")


# Join full time series with water temp data
dat2 <- left_join(full_datetime.df, wtr_pre2, by = "datetime")

# Summarize hourly water temp to daily data ####

# open air function needs date column so renamed datetime
dat3 <- dat2 %>%
  rename(date = datetime)

# Set threshold to 75% of hourly data needs to be present to calculate the daily summary

watertemp_daily_mean <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "mean", start.date = "2007-06-30 00:00:00", end.date = "2008-09-16 23:00:00", interval = "hour")
watertemp_daily_median <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "median", start.date = "2007-06-30 00:00:00", end.date = "2008-09-16 23:00:00", interval = "hour")
watertemp_daily_min <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "min", start.date = "2007-06-30 00:00:00", end.date = "2008-09-16 23:00:00", interval = "hour")
watertemp_daily_max <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "max", start.date = "2007-06-30 00:00:00", end.date = "2008-09-16 23:00:00", interval = "hour")
watertemp_daily_sd <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "sd", start.date = "2007-06-30 00:00:00", end.date = "2008-09-16 23:00:00", interval = "hour")

#Bind summaries together
watertemp_daily_summary <- bind_cols(watertemp_daily_mean[,-4], watertemp_daily_median[,5:8], watertemp_daily_min[,5:8], watertemp_daily_max[,5:8], watertemp_daily_sd[,5:8])

# rename columns
colnames(watertemp_daily_summary)[4:7] <- c("HC.tempC_mean","NB.tempC_mean","NSH.tempC_mean", "SOTF.tempC_mean")
colnames(watertemp_daily_summary)[8:11] <- c("HC.tempC_median","NB.tempC_median","NSH.tempC_median", "SOTF.tempC_median")
colnames(watertemp_daily_summary)[12:15] <- c("HC.tempC_min","NB.tempC_min","NSH.tempC_min", "SOTF.tempC_min")
colnames(watertemp_daily_summary)[16:19] <- c("HC.tempC_max","NB.tempC_max","NSH.tempC_max", "SOTF.tempC_max")
colnames(watertemp_daily_summary)[20:23] <- c("HC.tempC_sd","NB.tempC_sd","NSH.tempC_sd", "SOTF.tempC_sd")


# Filter water temp data for sampling dates ####

# Read in sampling dates
sampling_dates_pre2009 <- read_csv("00_Data_files/EDI_data_clones/weekly_surface_gloeo_4sites_2005_2016_v10April2020.csv",
                                   col_types = list(
                                     date = col_date(format = ""),
                                     site = col_character(),
                                     year = col_double(),
                                     dayofyr = col_double(),
                                     n_sample = col_double(),
                                     coloniesperL = col_double(),
                                     filbundperL = col_double(),
                                     totalperL = col_double()
                                   ))

sampling_dates_pre2009_v2 <- sampling_dates_pre2009 %>%
  filter(year %in% 2007:2008) %>%
  select(date,site)

watertemp_daily_summary_v2 <- watertemp_daily_summary %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates_pre2009_v2$date)

# Gap fill with water temp for gloeo sample days within 1 or 2 days ####

watertemp_daily_summary_subset <- watertemp_daily_summary %>%
  mutate(month = month(date)) %>%
  filter(month %in% 6:9) %>%
  slice(1,82, 201)

# 1 day difference - wtr temp data on Sep 19, 2007 data on Sep. 20, wtr temp data on Sep. 16, 2008, data on Sep. 17
watertemp_daily_summary_subset_Sep <- watertemp_daily_summary_subset %>%
  mutate(date = date(date)) %>%
  slice(2,3) %>%
  mutate(date2 = date + ddays(1))

# 2 days difference in June #sample on June 28, temp data starts June 30
watertemp_daily_summary_subset_Jun <- watertemp_daily_summary_subset %>%
  mutate(date = date(date)) %>%
  slice(1) %>%
  mutate(date2 = date - ddays(2))

# Bind new dates together
watertemp_daily_summary_subset2 <- bind_rows(watertemp_daily_summary_subset_Jun, watertemp_daily_summary_subset_Sep) %>%
  select(-c(date, month)) %>%
  rename(date = date2) %>%
  select(date, year:SOTF.tempC_sd)

# filter dat5 to remove NA dates
watertemp_daily_summary_v3 <- watertemp_daily_summary_v2 %>%
  filter(!date %in% watertemp_daily_summary_subset2$date)

# full join for more complete water temp data
watertemp_daily_summary_v4 <- full_join(watertemp_daily_summary_v3, watertemp_daily_summary_subset2) %>%
  arrange(date)

# Write data for 2nd dataset with water temp holes filled in
#write_csv(dat7, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled_SOTF.csv")

#saveRDS(dat7, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled_SOTF.rds")


# Growing Degree Days ####

# set base temp to 4°C - water temp limit for growth of gloeo
base_temp <- 4

gdd1 <- watertemp_daily_summary %>% # use daily water temp data summary for June-Sep
  mutate(month = month(date)) %>%
  filter(month %in% 6:9) %>%
  mutate(gdd_HC = ((HC.tempC_max + HC.tempC_min)/2) - base_temp) %>%
  mutate(gdd_NB = ((NB.tempC_max + NB.tempC_min)/2) - base_temp) %>%
  mutate(gdd_NSH = ((NSH.tempC_max + NSH.tempC_min)/2) - base_temp) %>%
  mutate(gdd_SOTF = ((SOTF.tempC_max + SOTF.tempC_min)/2) - base_temp) %>%
  mutate(dayofyr = yday(date)) %>%
  select(year,dayofyr,gdd_HC:gdd_SOTF) %>%
  filter(!is.na(gdd_HC)) %>%
  pivot_wider(names_from = year, values_from = gdd_HC:gdd_SOTF) %>% # make wide to do each year separately
  arrange(dayofyr)

# Calculate gdd as column sum of daily data - separate each year since different number of missing points
# Note could also try rollsum

chunk <- rep(NA, 10)
chunk2 <- rep(NA, 2)


gdd2 <- gdd1 %>%
  mutate(gdd_HC_sum07 = c(chunk,cumsum(na.omit(gdd_HC_2007)))) %>%
  mutate(gdd_NB_sum07 = c(chunk,cumsum(na.omit(gdd_NB_2007)))) %>%
  mutate(gdd_NSH_sum07 = c(chunk,cumsum(na.omit(gdd_NSH_2007)))) %>%
  mutate(gdd_SOTF_sum07 = c(chunk,cumsum(na.omit(gdd_SOTF_2007)))) %>%
  mutate(gdd_HC_sum08 = c(cumsum(na.omit(gdd_HC_2008)),chunk2)) %>%
  mutate(gdd_NB_sum08 = c(cumsum(na.omit(gdd_NB_2008)),chunk2)) %>%
  mutate(gdd_NSH_sum08 = c(cumsum(na.omit(gdd_NSH_2008)),chunk2)) %>%
  mutate(gdd_SOTF_sum08 = c(cumsum(na.omit(gdd_SOTF_2008)),chunk2))

# Join with date again and keep just the correct years in one column

gdd2_long <- gdd2 %>%
  select(1,10:17)

gdd3 <- left_join(gdd2_long, watertemp_daily_summary[,1:3], by = "dayofyr") %>%
  arrange(date)

gdd3_07 <- gdd3 %>%
  select(date, year, dayofyr, 2:5) %>%
  filter(year==2007) %>%
  rename(gdd_HC_sum = gdd_HC_sum07, gdd_NB_sum = gdd_NB_sum07, gdd_NSH_sum = gdd_NSH_sum07, gdd_SOTF_sum = gdd_SOTF_sum07)

gdd3_08 <- gdd3 %>%
  select(date, year, dayofyr, 6:9) %>%
  filter(year==2008) %>%
  rename(gdd_HC_sum = gdd_HC_sum08, gdd_NB_sum = gdd_NB_sum08, gdd_NSH_sum = gdd_NSH_sum08, gdd_SOTF_sum = gdd_SOTF_sum08)

gdd4 <- bind_rows(gdd3_07, gdd3_08)

gdd4_subset_Sep <- gdd4 %>%
  slice(82, 172) %>%
  mutate(date = date(date)) %>%
  mutate(date2 = date + ddays(1))

gdd4_subset_Jun <- gdd4 %>%
  slice(1) %>%
  mutate(date = date(date)) %>%
  mutate(date2 = date - ddays(2))

# Bind new dates together
gdd4_subset <- bind_rows(gdd4_subset_Jun, gdd4_subset_Sep) %>%
  select(-date) %>%
  rename(date = date2) %>%
  select(date, 3:6)

# Filter gdd data for sampling dates and join with water temp data
watertemp_all <- left_join(watertemp_daily_summary_v4, gdd4, by = c("date", "year", "dayofyr"))

# replace with data subset
watertemp_all[1,24:27] <- gdd4_subset[1,2:5]
watertemp_all[13,24:27] <- gdd4_subset[2,2:5]
watertemp_all[29,24:27] <- gdd4_subset[3,2:5]

# add in sampling week to match full dataset
watertemp_all <- watertemp_all %>%
  mutate(season_week = c(5:17, 2:17)) %>%
  mutate(year = year(date))

season_week1 <- tibble(season_week = c(1:4, 18:20), year = 2007)
season_week2 <- tibble(season_week = c(1, 18:20), year = 2008)


watertemp_all2 <- full_join(season_week1, watertemp_all) %>%
  arrange(year, season_week)

watertemp_all3 <- full_join(watertemp_all2, season_week2) %>%
  arrange(year, season_week)


# Write all water temp data
write_csv(watertemp_all3, "./00_Data_files/Covariate_analysis_data/onset_watertemp_all_pre2009.csv")

# Only keep water temp min and GDD
write_csv(watertemp_all3[,c(1:3,12:15,24:28)], "./00_Data_files/Bayesian_model_input_data/onset_minwtrtemp_gdd_allsites_pre2009.csv")

