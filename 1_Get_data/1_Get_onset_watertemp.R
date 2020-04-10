# Script to download and wrangle Onset water temp data from EDI
# Last updated 2020 April 9 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive, openair)

# Download data ####
#download data file into appropriate local folder
drive_download(
  file = as_id(drive_find(pattern = "temp_2006-2018_L1_v07April2020.csv")$id),
  path = "./00_Data_files/EDI_data_clones/temp_2006-2018_L1_v07April2020.csv", overwrite = TRUE)

# Alternative way to get file ID
as_id(drive_find(pattern = "temp_2006-2018_L1_v07April2020.csv")$id)

drive_download(file = as_id("1MWhFGVG7Y4Oej4OIdj99H_VSUAiA5M00"),
               path = "./00_Data_files/EDI_data_clones/temp_2006-2018_L1_v07April2020.csv", overwrite = TRUE)

# Load data into R ####
dat <- read_csv("./00_Data_files/EDI_data_clones/temp_2006-2018_L1_v07April2020.csv",
                col_types = list(datetime = col_datetime(),
                                 year = col_double(),
                                 dayofyr = col_double(),
                                 time = col_time(),
                                 HCS_tempC = col_double(),
                                 HCS_flag = col_character(),
                                 NBRY_tempC = col_double(),
                                 NBRY_flag = col_character(),
                                 SOTF_tempC = col_double(),
                                 SOTF_flag = col_character(),
                                 NSH_tempC = col_double(),
                                 NSH_flag = col_character()))



#limit logger data to sampling years and remove 30 min data in 2009 so consistent with other years
dat1 <- dat %>%
  mutate(date = date(datetime)) %>%
  filter(year %in% 2009:2016) %>%
  mutate(min = minute(time)) %>%
  filter(min!=30) %>%
  select(-min) %>%
  select(datetime, date, year, dayofyr, time, HCS_tempC)

# check HCS_flag - all NA
sum(!is.na(dat1$HCS_flag))

# plot data to check - data look good
plot(HCS_tempC ~ datetime, data = dat1)

# Add full time series to data to be able to calculate daily averages but cut-off days with water temp for only part of day
full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dhours(1))

full_datetime.df <- as_tibble(full_datetime)
colnames(full_datetime.df) <- "datetime"

# Join full time series with water temp data
dat2 <- left_join(full_datetime.df, dat1, by = "datetime")

# Summarize hourly water temp to daily data ####

# open air function needs date column so renamed datetime
dat3 <- dat2 %>%
  select(-date) %>%
  rename(date = datetime)

# Set threshold to 75% of hourly data needs to be present to calculate the daily summary

hcs_watertemp_daily_mean <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
hcs_watertemp_daily_median <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "median", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
hcs_watertemp_daily_min <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "min", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
hcs_watertemp_daily_max <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "max", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
hcs_watertemp_daily_sd <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "sd", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")

#Bind summaries together
hcs_watertemp_daily_summary <- bind_cols(hcs_watertemp_daily_mean[,-4], hcs_watertemp_daily_median[,5], hcs_watertemp_daily_min[,5], hcs_watertemp_daily_max[,5], hcs_watertemp_daily_sd[,5])

# rename columns
dat4 <- hcs_watertemp_daily_summary %>%
  rename(HCS.tempC_mean = HCS_tempC, HCS.tempC_median = HCS_tempC1, HCS.tempC_min = HCS_tempC2, HCS.tempC_max = HCS_tempC3,HCS.tempC_sd = HCS_tempC4)


# Join with sampling dates ####
#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")
str(sampling_dates)

# Filter water temp data for sampling dates
dat5 <- dat4 %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates$date)

# Write data ####
write_csv(dat5, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary.csv")

# Gap fill with water temp for gloeo sample days within 1 or 2 days ####

dat4_subset <- dat4 %>%
  mutate(month = month(date)) %>%
  filter(month %in% 5:9)

dat4_subset1 <- dat4_subset[c(123, 277, 320, 430, 464, 588, 616, 739, 775, 892, 935, 1044, 1201),]

dat4_subset2 <- dat4_subset1 %>%
  mutate(date = date(date)) %>%
  filter(month == 9) %>%
  mutate(date2 = date + ddays(1))

dat4_subset2[2,10] <- ymd("2010-09-23") #2 days apart

dat4_subset3 <- dat4_subset1 %>%
  mutate(date = date(date)) %>%
  filter(month == 5|month == 6) %>%
  mutate(date2 = date - ddays(1))

# Bind new dates together
dat4_subset4 <- bind_rows(dat4_subset2, dat4_subset3) %>%
  select(-c(date, month)) %>%
  rename(date = date2) %>%
  select(date, year:HCS.tempC_sd)

# filter dat5 to remove NA dates
dat6 <- dat5 %>%
  filter(!date %in% dat4_subset4$date)

# full join for more complete water temp data
dat7 <- full_join(dat6, dat4_subset4) %>%
  arrange(date)

# Write data for 2nd dataset with water temp holes filled in ####
write_csv(dat7, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled.csv")

