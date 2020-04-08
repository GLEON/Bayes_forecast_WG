#Title: 3B Format_GLEON_buoy_tempstring_data
#Author: Mary Lofton
#Date: 08APR20

#Load packages

#run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive, rLakeAnalyzer)

#####################################################################################################################################
#TEMPORARY: get signed into google drive package; once you are signed in, return to R

#download data file into appropriate local folder
drive_download("~/GLEON_Bayesian_WG/EDI.data.clones/buoy.tempstring/2007-e2019_buoy_tempstring_v07April2020.csv",
               path = "./00_Data_files/EDI_data_clones/2007-e2019_buoy_tempstring_v07April2020.csv", overwrite = TRUE)

drive_download("~/GLEON_Bayesian_WG/EDI.data.clones/buoy.tempstring/2015_hobotempstring_L1.csv",
               path = "./00_Data_files/EDI_data_clones/2015_hobotempstring_L1.csv", overwrite = TRUE)

#######METHOD THAT WORKS FOR JENNIE
# Alternative way to get file ID
as_id(drive_find(pattern = "2007-e2019_buoy_tempstring_v07April2020.csv")$id)

# alternative way combining both steps of finding and downloading file
drive_download(
  file = as_id(drive_find(pattern = "2007-e2019_buoy_tempstring_v07April2020.csv")$id),
  path = "./00_Data_files/EDI_data_clones/2007-e2019_buoy_tempstring_v07April2020.csv", overwrite = TRUE)
#####################################################################################################################################

#load buoy data into R
buoy <- read_csv("./00_Data_files/EDI_data_clones/2007-e2019_buoy_tempstring_v07April2020.csv")

#create column for date in sampling_dates.csv format
buoy1 <- buoy %>% mutate(date = date(datetime))

#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")

#limit buoy data to sampling dates
buoy2 <- buoy1 %>% filter(date %in% sampling_dates$date)

#eliminate data where bottom water thermistors may have been in sediment
#get in format for calculating Schmidt stability in rLakeAnalyzer
buoy3 <- buoy2 %>%
  mutate(TempC_11p5m = ifelse(temp_flag == "11.5b, 13.5b",NA,TempC_11p5m),
         TempC_13p5m = ifelse(temp_flag == "11.5b, 13.5b",NA,TempC_13p5m)) %>%
  select(-date, -location, -temp_flag)

#set colnames for rLakeAnalyzer
colnames(buoy3)[2:37] <- c("wtr_0.5","wtr_0.75","wtr_0.85","wtr_1","wtr_1.5","wtr_1.75",
                          "wtr_1.85","wtr_2","wtr_2.5","wtr_2.75","wtr_2.85","wtr_3",
                          "wtr_3.5","wtr_3.75","wtr_3.85","wtr_4.5","wtr_4.75","wtr_4.85",
                          "wtr_5.5","wtr_5.75","wtr_5.85","wtr_6.5","wtr_6.75","wtr_6.85",
                          "wtr_7.5","wtr_7.75","wtr_7.85","wtr_8.5","wtr_8.75","wtr_8.85",
                          "wtr_9.5","wtr_9.75","wtr_9.85","wtr_10.5","wtr_11.5","wtr_13.5")

#load HOBO data from 2015 into R
hobo <- read_csv("./00_Data_files/EDI_data_clones/2015_hobotempstring_L1.csv")

#create column for date in sampling_dates.csv format
hobo1 <- hobo %>% mutate(date = date(datetime))

#limit buoy data to sampling dates
hobo2 <- hobo1 %>% filter(date %in% sampling_dates$date) %>%
  select(-date)

#set colnames for rLakeAnalyzer
colnames(hobo2)[2:10] <- c("wtr_0.5","wtr_1.5","wtr_2.5",
                           "wtr_3.5","wtr_4.5",
                           "wtr_5.5","wtr_6.5",
                           "wtr_7.5","wtr_8.5")

#calculate Schimdt stability
bathy <- load.bathy("./00_Data_files/Sunapee.bth")

schmidt_buoy <- ts.schmidt.stability(buoy3, bathy, na.rm = TRUE)

schmidt_hobo <- ts.schmidt.stability(hobo2, bathy, na.rm = TRUE)

schmidt <- bind_rows(schmidt_buoy, schmidt_hobo) %>%
  arrange(datetime)

#get summary statistics of Schmidt stability
schmidt1 <- schmidt %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarise(schmidt_mean = mean(schmidt.stability, na.rm = TRUE),
            schmidt_max = min(schmidt.stability, na.rm = TRUE),
            schmidt_min = max(schmidt.stability, na.rm = TRUE),
            schmidt_sd = sd(schmidt.stability, na.rm = TRUE))

#add column with differences in schmidt stability week to week
diff <- c(NA,diff(schmidt1$schmidt_mean))
schmidt1$schmidt_diff <- diff

schmidt1[schmidt1 == Inf] <- NA
schmidt1[schmidt1 == -Inf] <- NA
schmidt1[is.na(schmidt1)] <- NA


