# Script to download and wrangle Sunapee NLDAS data
# *Will need to update once NLDAS data are published on EDI - stored on GitHub for now
# Last updated 2020 May 22 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

# load other packages
pacman::p_load(tidyverse, lubridate)

# Load NLDAS data into R ####

nldas <- read_csv("./00_Data_files/NLDAS_SunapeeMet_1979_2016.csv")

# Filter for just short wave radiation and time period

solar_rad <- nldas %>%
  select(datetime,ShortWaveRad_Wperm2) %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  mutate(date = date(datetime)) %>%
  filter(year %in% 2009:2016) %>%
  filter(month %in% 5:10)

# Check data
plot(ShortWaveRad_Wperm2 ~ datetime, data = solar_rad)

sum(is.na(solar_rad$ShortWaveRad_Wperm2)) #no missing data

# Summarize data into daily summary ####
solar_rad1 <- solar_rad %>%
  group_by(date) %>%
  select(-c(datetime, month)) %>%
  summarize_all(lst(mean, median, max, sd, sum)) %>%
  select(date, starts_with("ShortWaveRad"))

# Daily summary for min excluding 0 values
solar_rad_min <- solar_rad %>%
  filter(ShortWaveRad_Wperm2 > 0) %>%
  group_by(date) %>%
  select(-c(datetime, month, year)) %>%
  summarize(ShortWaveRad_Wperm2_min = min(ShortWaveRad_Wperm2))

# Bind cols and re-arrange to match other datasets
solar_rad2 <- full_join(solar_rad1, solar_rad_min, by = "date") %>%
  select(1:3,7,4:6)

# Limit solar radiation data to sampling dates ####

# Read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

solar_rad3 <- solar_rad2 %>%
  filter(date %in% sampling_dates$date)

# Write NLDAS solar radiation data ####
write_csv(solar_rad3, "./00_Data_files/Covariate_analysis_data/NLDAS_solar_radiation_2009-2016.csv")


