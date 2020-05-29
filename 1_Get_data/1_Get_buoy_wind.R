# Script to download and wrangle Sunapee GLEON buoy wind data from EDI
# Last updated 2020 May 20 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, openair, zoo)

# Download data from EDI to local folder ####

# High-Frequency Weather Data at Lake Sunapee, New Hampshire, USA, 2007-2019
# EDI Package ID: edi.234.3
# Citation: LSPA, K.C. Weathers, and B.G. Steele. 2020. High-Frequency Weather Data at Lake Sunapee, New Hampshire, USA, 2007-2019 ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/698e9ffb0cdcda81ecf7188bff54445e. Accessed 2020-05-21.

data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.3&entityid=a14e1f5bbeddc155d3fd8885557466ec"

destination <- "./00_Data_files/EDI_data_clones"

download.file(data, destfile = "./00_Data_files/EDI_data_clones/2007_e2019_wind_L1.csv", method='libcurl')

# Load buoy wind data into R ####
buoy_wind <- read_csv("./00_Data_files/EDI_data_clones/2007_e2019_wind_L1.csv", col_types = cols(
  datetime = col_datetime(format = ""),
  location = col_character(),
  WindDir_deg = col_double(),
  WindSp_ms = col_double(),
  AveWindDir_deg = col_double(),
  AveWindSp_ms = col_double(),
  MaxWindSp_ms = col_double(),
  MaxWindDir_deg = col_double(),
  wind_flag = col_character()))

# Check and filter data ####

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

# Use average wind data for the rest of the years - no instantaneous data for 2015-2016
buoy_wind_avg <- buoy_wind1 %>%
  filter(year %in% c(2009:2010, 2013:2016)) %>%
  select(1,10:12,2,5:6)

# Join wind datasets
buoy_wind2 <- bind_rows(buoy_wind_inst, buoy_wind_avg) %>%
  select(-c(date, year, month))

# check data
plot(AveWindSp_ms ~ datetime, data = buoy_wind2)
summary(buoy_wind2)

# Combine wind data with full time series ####

full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dminutes(10))
full_datetime.df <- as_tibble_col(full_datetime, column_name = "datetime")

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
windsp_daily_summary <- bind_cols(windsp_daily_mean, windsp_daily_median[,3], windsp_daily_min[,3], windsp_daily_max[,3], windsp_daily_sd[,3])

# rename columns
windsp_daily_summary2 <- windsp_daily_summary %>%
  rename(AveWindSp_ms_mean = AveWindSp_ms, AveWindDir_cove_mean = AveWindDir_cove, AveWindSp_ms_median = AveWindSp_ms1, AveWindSp_ms_min = AveWindSp_ms2, AveWindSp_ms_max = AveWindSp_ms3, AveWindSp_ms_sd = AveWindSp_ms4)

# Limit wind speed data to sampling dates ####

# Read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

windsp_daily_summary3 <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates$date)

# Create 1-3 day & 1 week lag ####

# Read in sampling dates lag
sampling_dates_lag <- read_csv("./00_Data_files/Covariate_analysis_data/sampling_dates_lag.csv")

# Join wind speed data with 1 DAY lag date
windsp_daily_summary2_lag_1day <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_1daylag = date)

windsp_daily_summary2_lag_1day_join <- left_join(sampling_dates_lag[,c(1,3)], windsp_daily_summary2_lag_1day, by = "date_1daylag")

colnames(windsp_daily_summary2_lag_1day_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_1day_join)[-c(1:2)], '_1daylag')

# Join wind speed data with 2 DAY lag date
windsp_daily_summary2_lag_2day <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_2daylag = date)

windsp_daily_summary2_lag_2day_join <- left_join(sampling_dates_lag[,c(1,4)], windsp_daily_summary2_lag_2day, by = "date_2daylag")

colnames(windsp_daily_summary2_lag_2day_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_2day_join)[-c(1:2)], '_2daylag')

# Join wind speed data with 3 DAY lag date
windsp_daily_summary2_lag_3day <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_3daylag = date)

windsp_daily_summary2_lag_3day_join <- left_join(sampling_dates_lag[,c(1,5)], windsp_daily_summary2_lag_3day, by = "date_3daylag")
colnames(windsp_daily_summary2_lag_3day_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_3day_join)[-c(1:2)], '_3daylag')

# Join wind speed with 1 WEEK lag date
windsp_daily_summary2_lag_1week <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  rename(date_1weeklag = date)

windsp_daily_summary2_lag_1week_join <- left_join(sampling_dates_lag[,c(1:2)], windsp_daily_summary2_lag_1week, by = "date_1weeklag")
colnames(windsp_daily_summary2_lag_1week_join)[-c(1:2)] = paste0(colnames(windsp_daily_summary2_lag_1week_join)[-c(1:2)], '_1weeklag')

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
wind.speed_diff_output3 <- left_join(wind.speed_diff_output2, windsp_daily_summary2_diff[,c(1:2,4,6)], by = "date")

# Write wind speed 1 week difference data
#write_csv(wind.speed_diff_output3, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary_1weekdiff.csv")

# Calculate cumulative sum 1-2 days ####

# Use hourly wind speed full time dataset

# Filter for wind direction coming towards Herrick Cove and calculates cumulative sum just for wind speed blowing in
windsp_cum_sum_filter_in <- windsp_hourly_mean_filter[-c(1:1656),] %>%
  mutate(AveWindSp_ms_filter = ifelse(AveWindDir_cove==0, 0, AveWindSp_ms))

# Filter missing hours
# 2009-08-08 09:00:00
# 2009-08-11 13:00:00
# 2013-08-27 09:00:00
# 2013-09-09 10:00:00
# 2013-09-25 14:00:00
# 2015-06-11 08:00:00
# 2016-05-03 09:00:00
# 2016-05-03 10:00:00

windsp_cum_sum_filter_in1 <- windsp_cum_sum_filter_in[-c(250,326,35770,36083,36471,51441,59290,59291),c(1,5)] %>%
  mutate(sum_1 = rollsum(AveWindSp_ms_filter, k = 24, fill = NA, align = "right")) %>%
  mutate(sum_2 = rollsum(AveWindSp_ms_filter, k = 48, fill = NA, align = "right"))

windsp_cum_sum_max_day_in <- windsp_cum_sum_filter_in1 %>%
  rename(datetime = date) %>%
  mutate(date = date(datetime)) %>%
  filter(sum_1 !="NA") %>%
  group_by(date) %>%
  distinct(max(datetime)) %>%
  rename(datetime = "max(datetime)")

windsp_cum_sum_filter_in2 <- windsp_cum_sum_filter_in1 %>%
  rename(datetime = date) %>%
  filter(datetime %in% windsp_cum_sum_max_day_in$datetime) %>%
  mutate(date = date(datetime)) %>%
  select(date,sum_1:sum_2) %>% #no data for 5-14 days
  rename(windsp_cumsum_1day_in = sum_1, windsp_cumsum_2day_in = sum_2)

# Join with sample dates
windsp_cum_sum_filter_in3 <- left_join(sampling_dates, windsp_cum_sum_filter_in2, by = "date")

# Combine all wind speed data ####
windsp_all <- bind_cols(windsp_daily_summary3,windsp_daily_summary2_lag_1day_join[,-c(1:2)], windsp_daily_summary2_lag_2day_join[,-c(1:2)],windsp_daily_summary2_lag_3day_join[,-c(1:2)],windsp_daily_summary2_lag_1week_join[,-c(1:2)], windsp_cum_sum_filter_in3[,-1])

# exclude raw wind direction data
wind_speed_data_no_windsp_filter <- windsp_all %>%
  select(-starts_with(c("AveWindDir_deg")))

# Write csv no wind sp filtered data
#write_csv(wind_speed_data_no_windsp_filter, "./00_Data_files/Covariate_analysis_data/wind_speed_all_no_windsp_filter.csv")

# Filter for wind direction blowing into cove and create lags ####
wind_speed_data_in <- wind_speed_data_no_windsp_filter %>%
  select(1:7) %>%
  mutate_at(vars(starts_with("AveWindSp_ms")),funs(ifelse(AveWindDir_cove_mean >=0.5,.,0))) %>%
  select(-starts_with("AveWindDir"))

lag_1day_wind_speed_data_in <- wind_speed_data_no_windsp_filter %>%
  select(ends_with("1daylag")) %>%
  mutate_at(vars(starts_with("AveWindSp_ms")),funs(ifelse(AveWindDir_cove_mean_1daylag >=0.5,.,0))) %>%
  select(-starts_with("AveWindDir"))

lag_2day_wind_speed_data_in <- wind_speed_data_no_windsp_filter %>%
  select(ends_with("2daylag")) %>%
  mutate_at(vars(starts_with("AveWindSp_ms")),funs(ifelse(AveWindDir_cove_mean_2daylag >=0.5,.,0))) %>%
  select(-starts_with("AveWindDir"))

lag_3day_wind_speed_data_in <- wind_speed_data_no_windsp_filter %>%
  select(ends_with("3daylag")) %>%
  mutate_at(vars(starts_with("AveWindSp_ms")),funs(ifelse(AveWindDir_cove_mean_3daylag >=0.5,.,0))) %>%
  select(-starts_with("AveWindDir"))

lag_1week_wind_speed_data_in <- wind_speed_data_no_windsp_filter %>%
  select(ends_with("1weeklag")) %>%
  mutate_at(vars(starts_with("AveWindSp_ms")),funs(ifelse(AveWindDir_cove_mean_1weeklag >=0.5,.,0))) %>%
  select(-starts_with("AveWindDir"))

# combine all wind speed filtered for directions blowing into cove together and cummulative sum wind speed data in
wind_speed_data_in_all <- bind_cols(wind_speed_data_in,lag_1day_wind_speed_data_in,lag_2day_wind_speed_data_in,lag_3day_wind_speed_data_in,lag_1week_wind_speed_data_in,wind_speed_data_no_windsp_filter[,32:33])

# Add 'in' to column names to differentiate
colnames(wind_speed_data_in_all)[-c(1,27:28)] = paste0(colnames(wind_speed_data_in_all)[-c(1,27:28)], '_in')

# combine all wind speed - wind speed filtered for blowing into cove and wind direction variables
wind_dir <- wind_speed_data_no_windsp_filter %>%
  select(starts_with("AveWindDir"))

wind_data_all <- bind_cols(wind_speed_data_in_all, wind_dir)

# Write final wind data ####

write_csv(wind_data_all, "./00_Data_files/Covariate_analysis_data/wind_data_all.csv")
