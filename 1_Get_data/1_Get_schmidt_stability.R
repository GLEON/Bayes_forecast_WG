# Script to download and wrangle Sunapee GLEON buoy water temp string thermistor data from EDI and calculate schmidt stability
# Last updated 2020 May 21 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, rLakeAnalyzer, openair)

# Download data from EDI to local folder ####

# Lake Sunapee Instrumented Buoy: High Frequency Water Temperature and Dissolved Oxygen Data – 2007-2019
# EDI Package ID: edi.499.1
# Citation: LSPA, K.C. Weathers, and B.G. Steele. 2020. Lake Sunapee Instrumented Buoy: High Frequency Water Temperature and Dissolved Oxygen Data – 2007-2019 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/70c41711d6199ac2758764ecfcb9815e. Accessed 2020-05-21.

buoy_data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.1&entityid=06da1cc93c1eaa69f819ce8610f5cd33"

hobo_data <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.1&entityid=14340fa6419a6b4bc82cfe4713c25ea6"

destination <- "./00_Data_files/EDI_data_clones"

# Buoy temp string data
download.file(buoy_data,destfile = "./00_Data_files/EDI_data_clones/2007_e2019_buoy_templine_v22April2020.csv", method='libcurl')

# HOBO data
download.file(hobo_data,destfile = "./00_Data_files/EDI_data_clones/2015_hobotempstring_L1.csv", method='libcurl')

# Load buoy data into R ####
buoy <- read_csv("./00_Data_files/EDI_data_clones/2007_e2019_buoy_templine_v22April2020.csv",
                 col_types = list(
                   datetime = col_datetime(format = ""),
                   location = col_character(),
                   TempC_0p5m = col_double(),
                   TempC_0p75m = col_double(),
                   TempC_0p85m = col_double(),
                   TempC_1m = col_double(),
                   TempC_1p5m = col_double(),
                   TempC_1p75m = col_double(),
                   TempC_1p85m = col_double(),
                   TempC_2m = col_double(),
                   TempC_2p5m = col_double(),
                   TempC_2p75m = col_double(),
                   TempC_2p85m = col_double(),
                   TempC_3m = col_double(),
                   TempC_3p5m = col_double(),
                   TempC_3p75m = col_double(),
                   TempC_3p85m = col_double(),
                   TempC_4p5m = col_double(),
                   TempC_4p75m = col_double(),
                   TempC_4p85m = col_double(),
                   TempC_5p5m = col_double(),
                   TempC_5p75m = col_double(),
                   TempC_5p85m = col_double(),
                   TempC_6p5m = col_double(),
                   TempC_6p75m = col_double(),
                   TempC_6p85m = col_double(),
                   TempC_7p5m = col_double(),
                   TempC_7p75m = col_double(),
                   TempC_7p85m = col_double(),
                   TempC_8p5m = col_double(),
                   TempC_8p75m = col_double(),
                   TempC_8p85m = col_double(),
                   TempC_9p5m = col_double(),
                   TempC_9p75m = col_double(),
                   TempC_9p85m = col_double(),
                   TempC_10p5m = col_double(),
                   TempC_11p5m = col_double(),
                   TempC_13p5m = col_double(),
                   temp_flag = col_character()
                 ))

# Load HOBO data from 2015 into R ####
hobo <- read_csv("./00_Data_files/EDI_data_clones/2015_hobotempstring_L1.csv", col_types = list(
    datetime = col_datetime(format = ""),
    TempC_0p5m = col_double(),
    TempC_1p5m = col_double(), #col_logical(), no data just NA
    TempC_2p5m = col_double(),
    TempC_3p5m = col_double(),
    TempC_4p5m = col_double(),
    TempC_5p5m = col_double(),
    TempC_6p5m = col_double(),
    TempC_7p5m = col_double(),
    TempC_8p5m = col_double()
  )
)

# Water temp data cleaning for buoy & hobo data ####

# BUOY
#eliminate data where bottom water thermistors may have been in sediment
#get in format for calculating Schmidt stability in rLakeAnalyzer
buoy1 <- buoy %>%
  mutate(date = date(datetime)) %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  filter(year %in% 2009:2016 & month %in% 5:10)

#set colnames for rLakeAnalyzer
colnames(buoy1)[3:38] <- c("wtr_0.5","wtr_0.75","wtr_0.85","wtr_1","wtr_1.5","wtr_1.75",
                           "wtr_1.85","wtr_2","wtr_2.5","wtr_2.75","wtr_2.85","wtr_3",
                           "wtr_3.5","wtr_3.75","wtr_3.85","wtr_4.5","wtr_4.75","wtr_4.85",
                           "wtr_5.5","wtr_5.75","wtr_5.85","wtr_6.5","wtr_6.75","wtr_6.85",
                           "wtr_7.5","wtr_7.75","wtr_7.85","wtr_8.5","wtr_8.75","wtr_8.85",
                           "wtr_9.5","wtr_9.75","wtr_9.85","wtr_10.5","wtr_11.5","wtr_13.5")


# To see data flags
print(unique(buoy1$temp_flag)) #  "11.5b, 13.5b" "i", "10.5dn"

buoy2 <- buoy1 %>%
  select(-ends_with(c(".75", ".85"))) %>%  # remove extra depths for data not part of time series
  mutate(wtr_10.5 = ifelse(temp_flag == "10.5dn",NA,wtr_10.5),
        wtr_11.5 = ifelse(temp_flag == "11.5b, 13.5b",NA,wtr_11.5),
         wtr_13.5 = ifelse(temp_flag == "11.5b, 13.5b",NA,wtr_13.5)) %>%
  filter(year!=2015) %>%    # no buoy data, use HOBO temp string data instead
  select(datetime,wtr_0.5:wtr_10.5) # remove 11.5 & 13.5 m - no data


# HOBO

#set colnames for rLakeAnalyzer
colnames(hobo)[2:10] <- c("wtr_0.5","wtr_1.5","wtr_2.5",
                           "wtr_3.5","wtr_4.5",
                           "wtr_5.5","wtr_6.5",
                           "wtr_7.5","wtr_8.5")


# Calculate Schimdt stability ####

# Load bathymetric data
bathy <- load.bathy("./00_Data_files/Sunapee.bth")

schmidt_buoy <- ts.schmidt.stability(buoy2, bathy, na.rm = TRUE)

schmidt_hobo <- ts.schmidt.stability(hobo[,-11], bathy, na.rm = TRUE)

# Schmidt Data cleaning  ####

# set negative values to 0 or remove if too early in year - cite Bruesewitz et al. 2015
plot(schmidt.stability ~ datetime, data = schmidt_buoy)
points(schmidt.stability ~ datetime, data = schmidt_hobo, col = 'red')

# 2009 Cleaning
# Remove values < 0 and < 50 before Sep 14
schmidt_buoy_2009_part1 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2009) %>%
  filter(date < "2009-09-14") %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,NA, schmidt.stability)) %>% # remove negative values
  filter(schmidt.stability > 50) # remove outliers in Aug/Sep

schmidt_buoy_2009_part2 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2009) %>%
  filter(date >= "2009-09-14") %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,0, schmidt.stability)) # set negative values to 0

# Join all 2009 data back together
schmidt_buoy_2009 <- bind_rows(schmidt_buoy_2009_part1, schmidt_buoy_2009_part2) %>%
  arrange(datetime)

#2010 Cleaning
schmidt_buoy_2010_part1 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2010) %>%
  filter(date <= "2010-06-14")

schmidt_buoy_2010_part2 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2010) %>%
  filter(date > "2010-06-14") %>%
  filter(date < "2010-07-05") %>%
  filter(schmidt.stability > 50)

schmidt_buoy_2010_part3 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2010) %>%
  filter(date >= "2010-07-05") %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,0, schmidt.stability)) # set negative values to 0

# Join all 2010 data back together
schmidt_buoy_2010 <- bind_rows(schmidt_buoy_2010_part1, schmidt_buoy_2010_part2, schmidt_buoy_2010_part3) %>%
  arrange(datetime)

# 2011 & 2012, 2016 Cleaning
schmidt_buoy_2011_2012_2016 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year %in% c(2011,2012,2016)) %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,0, schmidt.stability)) # set negative values to 0

# 2013 Cleaning
schmidt_buoy_2013_part1 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2013) %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,0, schmidt.stability)) # set negative values to 0

# Remove low outliers in August 2013-08-12 16:40:00, 2013-08-12 16:50:00
schmidt_buoy_2013 <- schmidt_buoy_2013_part1[-c(14933:14934),]

# 2014 Cleaning
schmidt_buoy_2014_part1 <- schmidt_buoy %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year == 2014)

# Remove low outliers in August 2014-08-13 16:40:00 - 2014-08-13 17:10:00
schmidt_buoy_2014_part2 <- schmidt_buoy_2014_part1[-c(15077:15080),]

schmidt_buoy_2014 <- schmidt_buoy_2014_part2 %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,0, schmidt.stability)) # set negative values to 0

#2015 Cleaning
schmidt_hobo_2015 <- schmidt_hobo %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  mutate(schmidt.stability = ifelse(schmidt.stability < 0,0, schmidt.stability)) # set negative values to 0

# Bind all rows for schmidt togeher

schmidt_buoy1 <- bind_rows(schmidt_buoy_2009, schmidt_buoy_2010, schmidt_buoy_2011_2012_2016, schmidt_buoy_2013, schmidt_buoy_2014,schmidt_hobo_2015) %>%
  arrange(datetime) %>%
  select(datetime, year, schmidt.stability)

# Summarize schmidt for daily summary ####

# Add full time series to data to be able to calculate daily averages but cut-off days with schmidt data for only part of day
full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dminutes(10))
full_datetime.df <- as_tibble(full_datetime) %>%
  mutate(year = year(value))
colnames(full_datetime.df) <- c("datetime", "year")

# Separate time series for 2015 since data were 15 minute intervals instead of 10
full_datetime_2015 <- seq(from = ymd_hms("2015-05-21 00:00:00"), to = ymd_hms("2015-10-02 23:00:00"), by = dminutes(15))
full_datetime_2015.df <- as_tibble(full_datetime_2015)
colnames(full_datetime_2015.df) <- "datetime"


# Join all years of time series except 2015 with schmidt stability data since 2015 data collected every 15 min
schmidt_buoy_no2015 <- schmidt_buoy1 %>%
  filter(year != 2015) %>%
  select(-year)

full_datetime.df_no2015 <- full_datetime.df %>%
  filter(year != 2015) %>%
  select(-year)

schmidt_buoy2 <- left_join(full_datetime.df_no2015, schmidt_buoy_no2015, by = "datetime") %>%
  rename(date = datetime) # open air function needs date column so renamed datetime

# 2015 join
schmidt_buoy_2015 <- schmidt_buoy1 %>%
  filter(year == 2015) %>%
  select(-year)

schmidt_buoy2015_fulltime <- left_join(full_datetime_2015.df, schmidt_buoy_2015, by = "datetime") %>%
  rename(date = datetime) # open air function needs date column so renamed datetime

# Set threshold to 75% of hourly data needs to be present to calculate the daily summary

# all years except 2015
schmidt_daily_mean <- timeAverage(schmidt_buoy2, avg.time = "day", data.thresh = 75, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

schmidt_daily_median <- timeAverage(schmidt_buoy2, avg.time = "day", data.thresh = 75, statistic = "median", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

schmidt_daily_min <- timeAverage(schmidt_buoy2, avg.time = "day", data.thresh = 75, statistic = "min", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

schmidt_daily_max <- timeAverage(schmidt_buoy2, avg.time = "day", data.thresh = 75, statistic = "max", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

schmidt_daily_sd <- timeAverage(schmidt_buoy2, avg.time = "day", data.thresh = 75, statistic = "sd", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

# just 2015
schmidt_daily_mean_2015 <- timeAverage(schmidt_buoy2015_fulltime, avg.time = "day", data.thresh = 75, statistic = "mean", start.date = "2015-05-21 00:00:00", end.date = "2015-10-02 23:00:00", interval = "15 min")

schmidt_daily_median_2015 <- timeAverage(schmidt_buoy2015_fulltime, avg.time = "day", data.thresh = 75, statistic = "median", start.date = "2015-05-21 00:00:00", end.date = "2015-10-02 23:00:00", interval = "15 min")

schmidt_daily_min_2015 <- timeAverage(schmidt_buoy2015_fulltime, avg.time = "day", data.thresh = 75, statistic = "min", start.date = "2015-05-21 00:00:00", end.date = "2015-10-02 23:00:00", interval = "15 min")

schmidt_daily_max_2015 <- timeAverage(schmidt_buoy2015_fulltime, avg.time = "day", data.thresh = 75, statistic = "max", start.date = "2015-05-21 00:00:00", end.date = "2015-10-02 23:00:00", interval = "15 min")

schmidt_daily_sd_2015 <- timeAverage(schmidt_buoy2015_fulltime, avg.time = "day", data.thresh = 75, statistic = "sd", start.date = "2015-05-21 00:00:00", end.date = "2015-10-02 23:00:00", interval = "15 min")

#Bind summaries together
schmidt_daily_summary1 <- bind_cols(schmidt_daily_mean, schmidt_daily_median[,2], schmidt_daily_min[,2], schmidt_daily_max[,2], schmidt_daily_sd[,2])

# rename columns
schmidt_daily_summary2 <- schmidt_daily_summary1 %>%
  rename(schmidt.stability_mean = schmidt.stability, schmidt.stability_median = schmidt.stability1, schmidt.stability_min = schmidt.stability2, schmidt.stability_max = schmidt.stability3,schmidt.stability_sd = schmidt.stability4) %>%
  mutate(date = date(date)) %>%
  mutate(year = year(date)) %>%
  filter(year !=2015) # remove 2015 to be able to bind separate data

#Bind 2015 summaries together
schmidt_daily_summary2015 <- bind_cols(schmidt_daily_mean_2015, schmidt_daily_median_2015[,2], schmidt_daily_min_2015[,2], schmidt_daily_max_2015[,2], schmidt_daily_sd_2015[,2])

# rename columns
schmidt_daily_summary2015_2 <- schmidt_daily_summary2015 %>%
  rename(schmidt.stability_mean = schmidt.stability, schmidt.stability_median = schmidt.stability1, schmidt.stability_min = schmidt.stability2, schmidt.stability_max = schmidt.stability3,schmidt.stability_sd = schmidt.stability4) %>%
  mutate(date = date(date)) %>%
  mutate(year = year(date))

# Bind all schmidt daily summaries
schmidt_daily_summary3 <- bind_rows(schmidt_daily_summary2, schmidt_daily_summary2015_2) %>%
  arrange(date) %>%
  select(-year)

# Check data with plots
plot(schmidt.stability_mean ~ date, data = schmidt_daily_summary3)
lines(schmidt.stability_median ~ date, data = schmidt_daily_summary3, col = "blue")
lines(schmidt.stability_min ~ date, data = schmidt_daily_summary3, col = "gray")
lines(schmidt.stability_max ~ date, data = schmidt_daily_summary3, col = "red")


# Limit schmidt data to sampling dates ####

#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

schmidt_daily_summary4 <- schmidt_daily_summary3 %>%
  filter(date %in% sampling_dates$date)

# write_csv(schmidt_daily_summary4, "./00_Data_files/Covariate_analysis_data/schmidt_stability_daily_summary.csv")

# Add in gap filled data for schmidt values 1-2 days apart from missing data ####

schmidt_daily_summary3_subset <- schmidt_daily_summary3[c(1547, 1848, 1892),]

schmidt_daily_summary3_subset[1,1] <- ymd("2013-08-15") #1 day apart
schmidt_daily_summary3_subset[2,1] <- ymd("2014-06-12") #1 day apart
schmidt_daily_summary3_subset[3,1] <- ymd("2014-07-23") #2 days apart

# filter dat5 to remove NA dates
schmidt_daily_summary5 <- schmidt_daily_summary4 %>%
  filter(!date %in% schmidt_daily_summary3_subset$date)

# full join for more complete schmidt data
schmidt_daily_summary6 <- full_join(schmidt_daily_summary5, schmidt_daily_summary3_subset) %>%
  arrange(date)

# Write data for 2nd dataset with water temp holes filled in
# write_csv(schmidt_daily_summary6, "./00_Data_files/Covariate_analysis_data/schmidt_stability_daily_summary_gap_filled.csv")

# Create 1 week lag ####

# Add in holes to full dataset
schmidt_daily_summary3_fill <- schmidt_daily_summary3 %>%
  filter(!date %in% schmidt_daily_summary3_subset$date)

# full join for more complete schmidt data
schmidt_daily_summary3_fill2 <- full_join(schmidt_daily_summary3_fill, schmidt_daily_summary3_subset) %>%
  arrange(date)

# Read in sampling dates lag
sampling_dates_lag <- read_csv("./00_Data_files/Covariate_analysis_data/sampling_dates_lag.csv")

#Join schmidt with 1 week lag date
schmidt_weeklag <- schmidt_daily_summary3_fill2 %>%
  rename(date_1weeklag = date)

schmidt_weeklag1 <- left_join(sampling_dates_lag[,1:2], schmidt_weeklag, by = "date_1weeklag")

colnames(schmidt_weeklag1)[-c(1:2)] = paste0(colnames(schmidt_weeklag1)[-c(1:2)], '_lag')

# Write schmidt 1 week lag data
# write_csv(schmidt_weeklag1, "./00_Data_files/Covariate_analysis_data/schmidt_stability_daily_summary_gap_filled_1weeklag.csv")

# Calculate 1 week difference ####

schmidt_daily_summary3_fill3 <- schmidt_daily_summary3_fill2 %>%
  mutate(month = month(date)) %>%
  filter(month %in% 5:10) #shorten dataset to sampling time period

# Make vectors for output
schmidt.stability_mean_diff <- vector("double", nrow(schmidt_daily_summary3_fill3))
schmidt.stability_median_diff <- vector("double", nrow(schmidt_daily_summary3_fill3))
schmidt.stability_min_diff <- vector("double", nrow(schmidt_daily_summary3_fill3))
schmidt.stability_max_diff <- vector("double", nrow(schmidt_daily_summary3_fill3))
schmidt.stability_sd_diff <- vector("double", nrow(schmidt_daily_summary3_fill3))

for (i in 1:nrow(schmidt_daily_summary3_fill3)) {
  schmidt.stability_mean_diff[i] <- schmidt_daily_summary3_fill3$schmidt.stability_mean[i+7] - schmidt_daily_summary3_fill3$schmidt.stability_mean[i]
  schmidt.stability_median_diff[i] <- schmidt_daily_summary3_fill3$schmidt.stability_median[i+7] - schmidt_daily_summary3_fill3$schmidt.stability_median[i]
  schmidt.stability_min_diff[i] <- schmidt_daily_summary3_fill3$schmidt.stability_min[i+7] - schmidt_daily_summary3_fill3$schmidt.stability_min[i]
  schmidt.stability_max_diff[i] <- schmidt_daily_summary3_fill3$schmidt.stability_max[i+7] - schmidt_daily_summary3_fill3$schmidt.stability_max[i]
  schmidt.stability_sd_diff[i] <- schmidt_daily_summary3_fill3$schmidt.stability_sd[i+7] - schmidt_daily_summary3_fill3$schmidt.stability_sd[i]

  schmidt_diff_output <- data.frame(date = schmidt_daily_summary3_fill3$date + ddays(7), schmidt.stability_mean_diff, schmidt.stability_median_diff, schmidt.stability_min_diff, schmidt.stability_max_diff, schmidt.stability_sd_diff)
}

# Filter water temp 1 week difference data for sampling dates
schmidt_diff_output2 <- left_join(sampling_dates, schmidt_diff_output, by = "date")

# Write water temp 1 week difference data
# write_csv(schmidt_diff_output2, "./00_Data_files/Covariate_analysis_data/schmidt_stability_daily_summary_gap_filled_1weekdiff.csv")

# Combine all schmidt data ####
schmidt_all <- bind_cols(schmidt_daily_summary6, schmidt_weeklag1[,-c(1:2)], schmidt_diff_output2[,-1])

write_csv(schmidt_all, "./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")
