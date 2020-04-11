# Script to download and wrangle Sunapee GLEON buoy wind data from EDI
# Last updated 2020 April 10 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive, rLakeAnalyzer, openair)

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

# Use instantaneous data for 2009-2014
buoy_wind_2009_2014 <- buoy_wind1 %>%
  filter(year %in% 2009:2014) %>%
  select(1,10:12,2:4)

sum(is.na(buoy_wind_2009_2014$WindSp_ms)) #Inst less missing data than Ave

# Use average wind data - no instantaneous data
buoy_wind_2015_2016 <- buoy_wind1 %>%
  filter(year %in% 2015:2016) %>%
  select(1,10:12,2,5:6) %>%
  rename(WindSp_ms = AveWindSp_ms, WindDir_deg = AveWindDir_deg)

sum(is.na(buoy_wind_2015_2016$AveWindSp_ms))

# Join wind datasets
buoy_wind2 <- bind_rows(buoy_wind_2009_2014, buoy_wind_2015_2016) %>%
  select(-c(date, year, month))

# check data
plot(WindSp_ms ~ datetime, data = buoy_wind2)

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
  filter(WindDir_deg >= 180 & WindDir_deg < 360)

# Calculate daily summaries from filtered wind direction and hourly average data ####
windsp_daily_mean <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_median <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "median", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_min <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "min", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_max <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "max", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

windsp_daily_sd <- timeAverage(windsp_hourly_mean_filter, avg.time = "day", data.thresh = 50, statistic = "sd", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "60 min", vector.ws = T)

#Bind summaries together
windsp_daily_summary <- bind_cols(windsp_daily_mean, windsp_daily_median[,3], windsp_daily_min[,3], windsp_daily_max[,3], windsp_daily_sd[,3])

# rename columns
windsp_daily_summary2 <- windsp_daily_summary %>%
  rename(windsp_mean = WindSp_ms, windsp_median = WindSp_ms1, windsp_min = WindSp_ms2, windsp_max = WindSp_ms3, windsp_sd = WindSp_ms4)

# Limit wind speed data to sampling dates ####

#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

windsp_daily_summary3 <- windsp_daily_summary2 %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates$date)

# Write wind speed data ####
write_csv(windsp_daily_summary3, "./00_Data_files/Covariate_analysis_data/wind_speed_daily_summary.csv")

