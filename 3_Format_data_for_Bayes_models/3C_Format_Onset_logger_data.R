#Title: 3C Format_Onset_logger_data
#Author: Mary Lofton
#Date: 08APR20

#Load packages

#run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

#####################################################################################################################################
#TEMPORARY: get signed into google drive package; once you are signed in, return to R

#download data file into appropriate local folder
drive_download("~/GLEON_Bayesian_WG/EDI.data.clones/onset.temp/temp_2006-2018_L1_v07April2020.csv",
               path = "./00_Data_files/EDI_data_clones/temp_2006-2018_L1_v07April2020.csv", overwrite = TRUE)

#######METHOD THAT WORKS FOR JENNIE
# Alternative way to get file ID
as_id(drive_find(pattern = "2007-e2019_buoy_tempstring_v07April2020.csv")$id)

# alternative way combining both steps of finding and downloading file
drive_download(
  file = as_id(drive_find(pattern = "2007-e2019_buoy_tempstring_v07April2020.csv")$id),
  path = "./00_Data_files/EDI_data_clones/2007-e2019_buoy_tempstring_v07April2020.csv", overwrite = TRUE)
#####################################################################################################################################

#load data into R
dat <- read_csv("./00_Data_files/EDI_data_clones/temp_2006-2018_L1_v07April2020.csv",
                col_types = list(datetime = col_datetime(),
                                 year = col_double(),
                                 dayofyr = col_double(),
                                 time = col_time(),
                                 HCS_tempC = col_double(),
                                 HCS_flag = col_character(),
                                 NBRY_tempC = col_double(),
                                 NBRY_flag = col_character(),
                                 SOTF_tempC = col_double(),
                                 SOTF_flag = col_character(),
                                 NSH_tempC = col_double(),
                                 NSH_flag = col_character()))

#create column for date in sampling_dates.csv format
dat1 <- dat %>% mutate(date = date(datetime))

#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

#limit logger data to sampling dates
dat2 <- dat1 %>% filter(date %in% sampling_dates$date)

#reformat to create "site" column
dat3 <- dat2 %>%
  select(date, HCS_tempC, NBRY_tempC, SOTF_tempC, NSH_tempC) %>%
  gather(HCS_tempC:NSH_tempC, key = "site",value = "tempC") %>%
  mutate(site = ifelse(site == "HCS_tempC","HCS",
                       ifelse(site == "NBRY_tempC","NBRY",
                              ifelse(site == "SOTF_tempC","SOTF",
                                     ifelse(site == "NSH_tempC","NSH",site)))))

#calculate water temp summary statistics
dat4 <- dat3 %>%
  group_by(site, date) %>%
  summarise(wtrtemp_mean = mean(tempC, na.rm = TRUE),
            wtrtemp_max = min(tempC, na.rm = TRUE),
            wtrtemp_min = max(tempC, na.rm = TRUE),
            wtrtemp_sd = sd(tempC, na.rm = TRUE))

dat4[dat4 == Inf] <- NA
dat4[dat4 == -Inf] <- NA
dat4[is.na(dat4)] <- NA

#get in wide format (year by week) for seasonal for-loop in JAGS models

#gap fill for missing dates
date <- rep(as.Date(sampling_dates$date), times = 4)
site <- rep(c("HCS","NBRY","SOTF","NSH"),each = 160)

obs <- as_tibble(data.frame(site,date))%>%
  mutate(site = as.character(site))

dat5 <- left_join(obs, dat4)

#Site 1 (focal site for analysis)
dat6 <- dat5 %>%
  mutate(season_week = rep(c(1:20),times = 8*4),
         year = year(date)) %>%
  filter(site == "HCS") %>%
  select(year, season_week, wtrtemp_min) %>%
  spread(key = season_week, value = wtrtemp_min) %>%
  select(-year)

colnames(dat6) <- paste("wk", colnames(dat6), sep = "_")

write.csv(dat6, "./00_Data_files/Bayesian_model_input_data/wtrtemp_mean_Site1.csv", row.names = FALSE)

#Site 2 (focal site for analysis)
dat7 <- dat5 %>%
  mutate(season_week = rep(c(1:20),times = 8*4),
         year = year(date)) %>%
  filter(site == "SOTF") %>%
  select(year, season_week, wtrtemp_min) %>%
  spread(key = season_week, value = wtrtemp_min) %>%
  select(-year)

colnames(dat7) <- paste("wk", colnames(dat7), sep = "_")

write.csv(dat7, "./00_Data_files/Bayesian_model_input_data/wtrtemp_mean_Site2.csv", row.names = FALSE)


