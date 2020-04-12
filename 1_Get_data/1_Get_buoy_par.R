# Script to download and wrangle Sunapee GLEON buoy PAR data from EDI
# Last updated 2020 April 11 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive, openair)

# Download data ####
#download data file into appropriate local folder

# Sunapee GLEON buoy PAR data
my_url <- "https://drive.google.com/file/d/1_Og4EBRCskBIkirpaCf8nKAiZUGeBZa6/view?usp=sharing"

drive_download(
  file = drive_get(my_url),
  path = "./00_Data_files/EDI_data_clones/2007-e2019_PAR_L1.csv", overwrite = TRUE)

# Alternative way to get file ID
as_id(drive_find(pattern = "buoy.met/2007-e2019_PAR_L1.csv")$id)

drive_download(file = as_id("1_Og4EBRCskBIkirpaCf8nKAiZUGeBZa6"),
               path = "./00_Data_files/EDI_data_clones/2007-e2019_PAR_L1.csv", overwrite = TRUE)


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

# Combine par data with full time series ####

full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dminutes(10))
full_datetime.df <- as_tibble(full_datetime)
colnames(full_datetime.df) <- "datetime"

par2 <- left_join(full_datetime.df, par1, by = "datetime") %>%
  rename(date = datetime) %>%  # open air function needs date column so renamed datetime
  select(-c(year,month))

# filter for > 0 for daily min value
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




