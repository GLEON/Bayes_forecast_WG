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
write_csv(precip2, "./00_Data_files/Covariate_analysis_data/PRISM_precip_daily_summary.csv")
