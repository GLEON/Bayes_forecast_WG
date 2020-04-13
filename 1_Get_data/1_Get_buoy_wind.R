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

# Use average wind data - no instantaneous data for 2015-2016
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

# Calculate hourly mean ####
windsp_hourly_mean <- timeAverage(buoy_wind3, avg.time = "hour", data.thresh = 75, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min", vector.ws = T)

# Filter hourly wind direction ####
windsp_hourly_mean_filter <- windsp_hourly_mean %>%
  mutate(AveWindDir_cove = ifelse(AveWindDir_deg >= 180 & AveWindDir_deg < 360, 1, 0))

# Calculate daily summaries from filtered wind direction and hourly average data ####
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

