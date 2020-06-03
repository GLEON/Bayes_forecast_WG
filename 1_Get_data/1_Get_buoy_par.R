# Script to download and wrangle Sunapee GLEON buoy PAR data from EDI
# Last updated 2020 May 20 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, openair)

# Download data from EDI to local folder ####

# High-Frequency Weather Data at Lake Sunapee, New Hampshire, USA, 2007-2019
# EDI Package ID: edi.234.3
# Citation: LSPA, K.C. Weathers, and B.G. Steele. 2020. High-Frequency Weather Data at Lake Sunapee, New Hampshire, USA, 2007-2019 ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/698e9ffb0cdcda81ecf7188bff54445e. Accessed 2020-05-21.

data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.3&entityid=e7e5f1961a054ef9b4146e82a8f77aa4"
destination <- "./00_Data_files/EDI_data_clones"

download.file(data,destfile = "./00_Data_files/EDI_data_clones/2007-e2019_PAR_L1.csv", method='libcurl')

# Load buoy PAR data into R ####
par <- read_csv("./00_Data_files/EDI_data_clones/2007-e2019_PAR_L1.csv",
                col_types = list(datetime = col_datetime(format = ""),
                                 location = col_character(),
                                 PAR_umolm2s = col_double(),
                                 PAR_flag = col_character()))

par1 <- par %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  filter(year %in% 2009:2016) %>%
  filter(month %in% 5:10)

# Check data
plot(PAR_umolm2s ~ datetime, data = par1) #no PAR data in 2014?

sum(is.na(par1$PAR_umolm2s)) # missing data

print(unique(par1$PAR_flag)) #  z

min(par1$PAR_umolm2s, na.rm = T) # no negative values

# Combine PAR data with full time series ####

full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dminutes(10))
full_datetime.df <- as_tibble_col(full_datetime, column_name = "datetime")

par2 <- left_join(full_datetime.df, par1, by = "datetime") %>%
  rename(date = datetime) %>%  # open air function needs date column so renamed datetime
  select(-c(year,month))

# filter for > 0 for daily min value not 0
par2_min <- par1 %>%
  filter(PAR_umolm2s > 0)

par3_min <- left_join(full_datetime.df, par2_min, by = "datetime") %>%
  rename(date = datetime) %>%  # open air function needs date column so renamed datetime
  select(-c(year,month))

# Calculate daily summaries ####
par_daily_mean <- timeAverage(par2, avg.time = "day", data.thresh = 75, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

par_daily_median <- timeAverage(par2, avg.time = "day", data.thresh = 75, statistic = "median", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

par_daily_min <- timeAverage(par3_min, avg.time = "day", data.thresh = 1, statistic = "min", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

par_daily_max <- timeAverage(par2, avg.time = "day", data.thresh = 75, statistic = "max", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

par_daily_sd <- timeAverage(par2, avg.time = "day", data.thresh = 75, statistic = "sd", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "10 min")

# Daily sum summary
par_daily_sum <- par2 %>%
  rename(datetime = date) %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(daily_sum = sum(PAR_umolm2s, na.rm = T), daily_count = sum(!is.na(PAR_umolm2s))) %>%
  mutate(daily_sum2 = ifelse(daily_count < 108, NA, daily_sum))

#Bind summaries together
par_daily_summary <- bind_cols(par_daily_mean, par_daily_median[,2], par_daily_min[,2], par_daily_max[,2], par_daily_sd[,2], par_daily_sum[,4])

# rename columns
par_daily_summary2 <- par_daily_summary %>%
  rename(par_mean = PAR_umolm2s, par_median = PAR_umolm2s1, par_min = PAR_umolm2s2, par_max = PAR_umolm2s3, par_sd = PAR_umolm2s4, par_sum = daily_sum2)

# Limit PAR data to sampling dates ####

#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

par_daily_summary3 <- par_daily_summary2 %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates$date)

# Write PAR data ####
write_csv(par_daily_summary3, "./00_Data_files/Covariate_analysis_data/par_daily_summary.csv")
