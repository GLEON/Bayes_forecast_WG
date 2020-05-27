# Script to download and wrangle PRISM precip data from EDI
# Last updated 2020 April 11 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

# Download data ####
my_url <- "https://drive.google.com/file/d/1EmgNrxctUfbjn_oDc79710CJkt2U8R61/view?usp=sharing"

drive_download(
  file = drive_get(my_url),
  path = "./00_Data_files/EDI_data_clones/PRISM_met_1981_2017_midge.csv", overwrite = TRUE)

# Alternative way to get file ID
drive_download(file = as_id("1EmgNrxctUfbjn_oDc79710CJkt2U8R61"),
               path = "./00_Data_files/EDI_data_clones/PRISM_met_1981_2017_midge.csv", overwrite = TRUE)

# Load data into R ####
precip <- read_csv("./00_Data_files/EDI_data_clones/PRISM_met_1981_2017_midge.csv",skip = 10)

# Rename columns
colnames(precip) <- c("date", "precip_mm", "air.tempC_min", "air.tempC_mean", "air.tempC_max")

# Limit data to sampling years
precip1 <- precip %>%
  select(date, precip_mm) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  filter(year %in% 2009:2016) %>%
  filter(month %in% 5:10)

# plot data to check - data look good
plot(precip_mm ~ date, data = precip1)

# Join with sampling dates ####
#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

# Filter precip data for sampling dates
precip2 <- precip1 %>%
  filter(date %in% sampling_dates$date) %>%
  select(-c(year, month))

# Write data ####
write_csv(precip2, "./00_Data_files/Covariate_analysis_data/PRISM_precip_daily_sum.csv")


# Create 1 day & 1 week lag precip data ####

# Read in sampling dates lag
sampling_dates_lag <- read_csv("./00_Data_files/Covariate_analysis_data/sampling_dates_lag.csv")

#Join precip with 1 day lag date
precip_lag_day <- precip1 %>%
  rename(date_1daylag = date) %>%
  select(date_1daylag, precip_mm)

precip_lag_day1 <- left_join(sampling_dates_lag[,c(1,3)], precip_lag_day, by = "date_1daylag") %>%
  rename(precip_mm_1daylag = precip_mm)

# Write precip 1 day lag data
write_csv(precip_lag_day1, "./00_Data_files/Covariate_analysis_data/PRISM_precip_daily_sum_1daylag.csv")

#Join precip with 1 week lag date
precip_lag_week <- precip1 %>%
  rename(date_1weeklag = date) %>%
  select(date_1weeklag, precip_mm)

precip_lag_week1 <- left_join(sampling_dates_lag[,1:2], precip_lag_week, by = "date_1weeklag") %>%
  rename(precip_mm_1weeklag = precip_mm)

# Write precip 1 week lag data
write_csv(precip_lag_week1, "./00_Data_files/Covariate_analysis_data/PRISM_precip_daily_sum_1weeklag.csv")

# Combine all precip data ####
precip_all <- bind_cols(precip2,precip_lag_day1[,3], precip_lag_week1[,3])

write_csv(precip_all, "./00_Data_files/Covariate_analysis_data/PRISM_precipitation_2009-2016.csv")
