# Script to download and wrangle Sunapee GLEON buoy wind data from EDI
# Last updated 2020 April 10 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive, openair)

# Download data ####
#download data file into appropriate local folder

# Sunapee GLEON buoy wind data
my_url <- "https://drive.google.com/file/d/1yQHeWihxC1C6wu4VR7boYo1bC9_GA123/view?usp=sharing"

drive_download(
  file = drive_get(my_url),
  path = "./00_Data_files/EDI_data_clones/2007-e2019_wind_L1.csv", overwrite = TRUE)

# Alternative way to get file ID
as_id(drive_find(pattern = "buoy.met/2007-e2019_wind_L1.csv")$id)

drive_download(file = as_id("1yQHeWihxC1C6wu4VR7boYo1bC9_GA123"),
               path = "./00_Data_files/EDI_data_clones/2007-e2019_wind_L1.csv", overwrite = TRUE)

# Load buoy wind data into R ####
buoy_wind <- read_csv("./00_Data_files/EDI_data_clones/2007-e2019_wind_L1.csv", col_types = cols(
  datetime = col_datetime(format = ""),
  location = col_character(),
  WindDir_deg = col_double(),
  WindSp_ms = col_double(),
  AveWindDir_deg = col_double(),
  AveWindSp_ms = col_double(),
  MaxWindSp_ms = col_double(),
  MaxWindDir_deg = col_double(),
  wind_flag = col_character()))

# Check data ####

buoy_wind1 <- buoy_wind %>%
  mutate(date = as_date(datetime)) %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  filter(year %in% 2009:2016) %>%
  filter(month %in% 5:10)

print(unique(buoy_wind1$wind_flag)) #NA, so no flags to worry about

# Use instantaneous data for 2011-2012 - no Avg wind speed data
buoy_wind_inst <- buoy_wind1 %>%
  filter(year %in% 2011:2012) %>%
  select(1,10:12,2:4) %>%
  rename(AveWindSp_ms = WindSp_ms, AveWindDir_deg = WindDir_deg) # rename to be able to join columns


plot(WindSp_ms ~ datetime, data = buoy_wind_inst)
points(AveWindSp_ms ~ datetime, data = buoy_wind_inst, col = "red")

sum(is.na(buoy_wind_inst$WindSp_ms))

# Use average wind data for the rest of the years - no instantaneous data for 2015-2016
buoy_wind_avg <- buoy_wind1 %>%
  filter(year %in% c(2009:2010, 2013:2016)) %>%
  select(1,10:12,2,5:6)

sum(is.na(buoy_wind_2015_2016$AveWindSp_ms))

# Join wind datasets
buoy_wind2 <- bind_rows(buoy_wind_inst, buoy_wind_avg) %>%
  select(-c(date, year, month))

# check data
plot(AveWindSp_ms ~ datetime, data = buoy_wind2)
which.max(buoy_wind2$AveWindSp_ms)

# Combine wind data with full time series ####

full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dminutes(10))
full_datetime.df <- as_tibble(full_datetime)
colnames(full_datetime.df) <- "datetime"

buoy_wind3 <- left_join(full_datetime.df, buoy_wind2, by = "datetime") %>%
  rename(date = datetime) # open air function needs date column so renamed datetime

# Calculate hourly mean of wind speed and wind direction data ####
windsp_hourly_mean <- timeAverage(buoy_wind3, avg.time = "hour", data.thresh = 75, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min", vector.ws = T)

# Add indicator variable for hourly wind direction ####
# If wind is blowing towards Herrick Cove 180-359°, use 1 if blowing away 0-179°, use 0
windsp_hourly_mean_filter <- windsp_hourly_mean %>%
  mutate(AveWindDir_cove = ifelse(AveWindDir_deg >= 180 & AveWindDir_deg < 360, 1, 0))

# Calculate daily summaries from hourly average data ####
windsp_daily_mean <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_median <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "median", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_min <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "min", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_max <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "max", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_sd <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "sd", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

#Bind summaries together
windsp_daily_summary <- bind_cols(windsp_daily_mean, windsp_daily_median[,3:4], windsp_daily_min[,3], windsp_daily_max[,3], windsp_daily_sd[,3])

# rename columns
windsp_daily_summary2 <- windsp_daily_summary %>%
  rename(AveWindSp_ms_mean = AveWindSp_ms, AveWindDir_cove_mean = AveWindDir_cove, AveWindSp_ms_median = AveWindSp_ms1, AveWindDir_cove_median = AveWindDir_cove1, AveWindSp_ms_min = AveWindSp_ms2, AveWindSp_ms_max = AveWindSp_ms3, AveWindSp_ms_sd = AveWindSp_ms4)

# Limit wind speed data to sampling dates ####

#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

windsp_daily_summary3 <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates$date)

# Write wind speed data ####
write_csv(windsp_daily_summary3, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary.csv")

# Create 1-3 day & 1 week lag ####

# Read in sampling dates lag
sampling_dates_lag <- read_csv("./00_Data_files/Covariate_analysis_data/sampling_dates_lag.csv")

# Join wind speed data with 1 DAY lag date
windsp_daily_summary2_lag_1day <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_1daylag = date)

windsp_daily_summary2_lag_1day_join <- left_join(sampling_dates_lag[,c(1,3)], windsp_daily_summary2_lag_1day, by = "date_1daylag")

colnames(windsp_daily_summary2_lag_1day_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_1day_join)[-c(1:2)], '_1daylag')

# Write wind speed 1 day lag data
write_csv(windsp_daily_summary2_lag_1day_join, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary_1daylag.csv")

# Join wind speed data with 2 DAY lag date
windsp_daily_summary2_lag_2day <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_2daylag = date)

windsp_daily_summary2_lag_2day_join <- left_join(sampling_dates_lag[,c(1,4)], windsp_daily_summary2_lag_2day, by = "date_2daylag")

colnames(windsp_daily_summary2_lag_2day_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_2day_join)[-c(1:2)], '_2daylag')

# Write wind speed 2 day lag data
write_csv(windsp_daily_summary2_lag_2day_join, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary_2daylag.csv")

# Join wind speed data with 3 DAY lag date
windsp_daily_summary2_lag_3day <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_3daylag = date)

windsp_daily_summary2_lag_3day_join <- left_join(sampling_dates_lag[,c(1,5)], windsp_daily_summary2_lag_3day, by = "date_3daylag")
colnames(windsp_daily_summary2_lag_3day_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_3day_join)[-c(1:2)], '_3daylag')

# Write wind speed 3 day lag data
write_csv(windsp_daily_summary2_lag_3day_join, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary_3daylag.csv")


# Join wind speed with 1 WEEK lag date
windsp_daily_summary2_lag_1week <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_1weeklag = date)

windsp_daily_summary2_lag_1week_join <- left_join(sampling_dates_lag[,c(1:2)], windsp_daily_summary2_lag_1week, by = "date_1weeklag")
colnames(windsp_daily_summary2_lag_1week_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_1week_join)[-c(1:2)], '_1weeklag')

# Write wind speed 1 week lag data
write_csv(windsp_daily_summary2_lag_1week_join, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary_1weeklag.csv")

# Calculate 1 week difference ####

windsp_daily_summary2_diff <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  mutate(month = month(date)) %>%
  filter(month %in% 5:10) #shorten dataset to sampling time period

# Make vectors for output
wind.speed_mean_diff <- vector("double", nrow(windsp_daily_summary2_diff))
wind.speed_median_diff <- vector("double", nrow(windsp_daily_summary2_diff))
wind.speed_min_diff <- vector("double", nrow(windsp_daily_summary2_diff))
wind.speed_max_diff <- vector("double", nrow(windsp_daily_summary2_diff))
wind.speed_sd_diff <- vector("double", nrow(windsp_daily_summary2_diff))

for (i in 1:nrow(windsp_daily_summary2_diff)) {
  wind.speed_mean_diff[i] <- windsp_daily_summary2_diff$AveWindSp_ms_mean[i+7] - windsp_daily_summary2_diff$AveWindSp_ms_mean[i]
  wind.speed_median_diff[i] <- windsp_daily_summary2_diff$AveWindSp_ms_median[i+7] - windsp_daily_summary2_diff$AveWindSp_ms_median[i]
  wind.speed_min_diff[i] <- windsp_daily_summary2_diff$AveWindSp_ms_min[i+7] - windsp_daily_summary2_diff$AveWindSp_ms_min[i]
  wind.speed_max_diff[i] <- windsp_daily_summary2_diff$AveWindSp_ms_max[i+7] - windsp_daily_summary2_diff$AveWindSp_ms_max[i]
  wind.speed_sd_diff[i] <- windsp_daily_summary2_diff$AveWindSp_ms_sd[i+7] - windsp_daily_summary2_diff$AveWindSp_ms_sd[i]

  wind.speed_diff_output <- data.frame(date = windsp_daily_summary2_diff$date + ddays(7), wind.speed_mean_diff, wind.speed_median_diff, wind.speed_min_diff, wind.speed_max_diff, wind.speed_sd_diff)
}

# Filter wind speed 1 week difference data for sampling dates
wind.speed_diff_output2 <- left_join(sampling_dates, wind.speed_diff_output, by = "date")
wind.speed_diff_output3 <- left_join( wind.speed_diff_output2, windsp_daily_summary2_diff[,c(1:2,4,6)], by = "date")

# Write water temp 1 week difference data
write_csv(wind.speed_diff_output3, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary_1weekdiff.csv")

# Cumulative sum 1-14 days ####

# Use hourly wind speed full time dataset

windsp_cum_sum <- windsp_hourly_mean_filter[-c(1:1656),] #start with complete day of water temp data

# Filter missing hours
# 2009-08-08 09:00:00
# 2009-08-11 13:00:00
# 2013-08-27 09:00:00
# 2013-09-09 10:00:00
# 2013-09-25 14:00:00
# 2015-06-11 08:00:00
# 2016-05-03 09:00:00
# 2016-05-03 10:00:00

# Includes all wind directions and calculates cumumlative sum regardless of wind direction
windsp_cum_sum1 <- windsp_cum_sum[-c(250,326,35770,36083,36471,51441,59290,59291),c(1,3)] %>%
  mutate(sum_1 = rollsum(AveWindSp_ms, k = 24, fill = NA, align = "right")) %>%
  mutate(sum_2 = rollsum(AveWindSp_ms, k = 48, fill = NA, align = "right")) %>%
  mutate(sum_3 = rollsum(AveWindSp_ms, k = 72, fill = NA, align = "right")) %>%
  mutate(sum_5 = rollsum(AveWindSp_ms, k = 120, fill = NA, align = "right")) %>%
  mutate(sum_7 = rollsum(AveWindSp_ms, k = 168, fill = NA, align = "right")) %>%
  mutate(sum_10 = rollsum(AveWindSp_ms, k = 240, fill = NA, align = "right")) %>%
  mutate(sum_14 = rollsum(AveWindSp_ms, k = 336, fill = NA, align = "right"))

# filter for value @ end of day
windsp_cum_sum2 <-  windsp_cum_sum1 %>%
  rename(datetime = date) %>%
  mutate(hour = hour(datetime)) %>%
  filter(hour == 23) %>%
  select(-hour)

# Find last day for 2016 - 22 instead of 23 hours
windsp_cum_sum2[2626,] <- windsp_cum_sum1[63015,]


# Filter wind speed cum sum for sampling dates
windsp_cum_sum3 <- windsp_cum_sum2 %>%
  mutate(date = date(datetime))

windsp_cum_sum4 <- left_join(sampling_dates, windsp_cum_sum3, by = "date") %>%
  select(date, sum_1:sum_14) %>%
  rename(windsp_cumsum_1day = sum_1, windsp_cumsum_2day = sum_2, windsp_cumsum_3day = sum_3, windsp_cumsum_5day = sum_5, windsp_cumsum_7day = sum_7, windsp_cumsum_10day = sum_10, windsp_cumsum_14day = sum_14)


# Filter for wind direction coming towards Herrick Cove and calculates cumulative sum just for wind speed blowing in
windsp_cum_sum_filter_in <- windsp_hourly_mean_filter[-c(1:1656),] %>%
  mutate(AveWindSp_ms_filter = ifelse(AveWindDir_cove==0, NA, AveWindSp_ms))

windsp_cum_sum_filter_in1 <- windsp_cum_sum_filter_in[-c(250,326,35770,36083,36471,51441,59290,59291),c(1,5)] %>%
  mutate(sum_1 = rollsum(AveWindSp_ms_filter, k = 24, fill = NA, align = "right")) %>%
  mutate(sum_2 = rollsum(AveWindSp_ms_filter, k = 48, fill = NA, align = "right")) %>%
  mutate(sum_3 = rollsum(AveWindSp_ms_filter, k = 72, fill = NA, align = "right")) %>%
  mutate(sum_5 = rollsum(AveWindSp_ms_filter, k = 120, fill = NA, align = "right")) %>%
  mutate(sum_7 = rollsum(AveWindSp_ms_filter, k = 168, fill = NA, align = "right")) %>%
  mutate(sum_10 = rollsum(AveWindSp_ms_filter, k = 240, fill = NA, align = "right")) %>%
  mutate(sum_14 = rollsum(AveWindSp_ms_filter, k = 336, fill = NA, align = "right"))

sum(!is.na(windsp_cum_sum_filter_in1$sum_5))

windsp_cum_sum_max_day_in <- windsp_cum_sum_filter_in1 %>%
  rename(datetime = date) %>%
  mutate(date = date(datetime)) %>%
  filter(sum_1 !="NA") %>%
  group_by(date) %>%
  distinct(max(datetime)) %>%
  rename(datetime = "max(datetime)")

windsp_cum_sum_filter_in2 <- windsp_cum_sum_filter_in1 %>%
  rename(datetime = date) %>%
  select(-c(sum_7:sum_14)) %>%
  filter(datetime %in% windsp_cum_sum_max_day_in$datetime) %>%
  mutate(date = date(datetime)) %>%
  select(date,sum_1:sum_3) %>% #no data for 5-14 days
  rename(windsp_cumsum_1day_in = sum_1, windsp_cumsum_2day_in = sum_2, windsp_cumsum_3day_in = sum_3)

# Join with sample dates
windsp_cum_sum_filter_in3 <- left_join(sampling_dates, windsp_cum_sum_filter_in2, by = "date")

sum(!is.na(windsp_cum_sum_filter_in3$sum_5))

# Filter for wind direction blowing away from Herrick Cove and calculates cumulative sum just for wind speed blowing out
windsp_cum_sum_filter_out <- windsp_hourly_mean_filter[-c(1:1656),] %>%
  mutate(AveWindSp_ms_filter = ifelse(AveWindDir_cove==0, AveWindSp_ms, NA))

windsp_cum_sum_filter_out1 <- windsp_cum_sum_filter_out[-c(250,326,35770,36083,36471,51441,59290,59291),c(1,5)] %>%
  mutate(sum_1 = rollsum(AveWindSp_ms_filter, k = 24, fill = NA, align = "right")) %>% # k = hours
  mutate(sum_2 = rollsum(AveWindSp_ms_filter, k = 48, fill = NA, align = "right")) %>%
  mutate(sum_3 = rollsum(AveWindSp_ms_filter, k = 72, fill = NA, align = "right")) %>%
  mutate(sum_5 = rollsum(AveWindSp_ms_filter, k = 120, fill = NA, align = "right")) %>%
  mutate(sum_7 = rollsum(AveWindSp_ms_filter, k = 168, fill = NA, align = "right")) %>%
  mutate(sum_10 = rollsum(AveWindSp_ms_filter, k = 240, fill = NA, align = "right")) %>%
  mutate(sum_14 = rollsum(AveWindSp_ms_filter, k = 336, fill = NA, align = "right"))

sum(!is.na(windsp_cum_sum_filter_out1$sum_14))

windsp_cum_sum_max_day_out <- windsp_cum_sum_filter_out1 %>%
  rename(datetime = date) %>%
  mutate(date = date(datetime)) %>%
  filter(sum_1 !="NA") %>%
  group_by(date) %>%
  distinct(max(datetime)) %>%
  rename(datetime = "max(datetime)")

windsp_cum_sum_filter_out2 <- windsp_cum_sum_filter_out1 %>%
  rename(datetime = date) %>%
  select(-c(sum_7:sum_14)) %>%
  filter(datetime %in% windsp_cum_sum_max_day_out$datetime) %>%
  mutate(date = date(datetime)) %>%
  select(date,sum_1:sum_2) %>% #no data for 5-14 days
  rename(windsp_cumsum_1day_out = sum_1, windsp_cumsum_2day_out = sum_2)

# Join with sample dates
windsp_cum_sum_filter_out3 <- left_join(sampling_dates, windsp_cum_sum_filter_out2, by = "date")


# Full join with cum sum in & out
wind_cum_sum_all <- full_join(windsp_cum_sum4, windsp_cum_sum_filter_in3, by = "date")
wind_cum_sum_all1 <- full_join(wind_cum_sum_all, windsp_cum_sum_filter_out3, by = "date")

write_csv(wind_cum_sum_all1, "./00_Data_files/Covariate_analysis_data/wind_speed_cummulative_sum.csv")


# Combine all wind speed data ####
windsp_all <- bind_cols(windsp_daily_summary3,windsp_daily_summary2_lag_1day_join[,-c(1:2)], windsp_daily_summary2_lag_2day_join[,-c(1:2)],windsp_daily_summary2_lag_3day_join[,-c(1:2)],windsp_daily_summary2_lag_1week_join[,-c(1:2)], wind_cum_sum_all1[,-1])

write_csv(windsp_all, "./00_Data_files/Covariate_analysis_data/wind_speed_all.csv")
