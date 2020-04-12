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
sampling_dates <- c(sampling_dates$date)

#limit logger data to sampling dates
#limit buoy data to sampling dates
dat2 <- dat1 %>%
  filter(date >= "2009-05-22" & date <= "2009-10-05")
dat3 <- dat1 %>%
  filter(date >= "2010-05-18" & date <= "2010-10-07")
dat4 <- dat1 %>%
  filter(date >= "2011-05-19" & date <= "2011-10-06")
dat5 <- dat1 %>%
  filter(date >= "2012-05-17" & date <= "2012-10-05")
dat6 <- dat1 %>%
  filter(date >= "2013-05-16" & date <= "2013-10-02")
dat7 <- dat1 %>%
  filter(date >= "2014-05-22" & date <= "2014-10-10")
dat8 <- dat1 %>%
  filter(date >= "2015-05-14" & date <= "2015-10-02")
dat9 <- dat1 %>%
  filter(date >= "2016-05-19" & date <= "2016-10-05")

dat10 <- bind_rows(dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9)

#reformat to create "site" column
dat11 <- dat10 %>%
  select(date, datetime, HCS_tempC, NBRY_tempC, SOTF_tempC, NSH_tempC) %>%
  gather(HCS_tempC:NSH_tempC, key = "site",value = "tempC") %>%
  mutate(site = ifelse(site == "HCS_tempC","HCS",
                       ifelse(site == "NBRY_tempC","NBRY",
                              ifelse(site == "SOTF_tempC","SOTF",
                                     ifelse(site == "NSH_tempC","NSH",site)))))

#calculate water temp summary statistics
dat12 <- dat11 %>%
  filter(date %in% sampling_dates) %>%
  group_by(site, date) %>%
  summarise(wtrtemp_mean = mean(tempC, na.rm = TRUE),
            wtrtemp_max = min(tempC, na.rm = TRUE),
            wtrtemp_min = max(tempC, na.rm = TRUE),
            wtrtemp_sd = sd(tempC, na.rm = TRUE))

dat12[dat12 == Inf] <- NA
dat12[dat12 == -Inf] <- NA
dat12[is.na(dat12)] <- NA


#get summary statistics of water temp. in week prior to sampling
dat14 <- dat11
weekly_summary <- data.frame(matrix(NA, 1,6))
colnames(weekly_summary) <- c("site","weekly_wtrtemp_mean","weekly_wtrtemp_max","weekly_wtrtemp_min","weekly_wtrtemp_sd","date")

for (i in 1:length(sampling_dates)){
  week <- dat14 %>%
    filter(date <= sampling_dates[i])
  week1 <- week %>%
    group_by(site) %>%
    summarise(weekly_wtrtemp_mean = mean(tempC, na.rm = TRUE),
              weekly_wtrtemp_max = min(tempC, na.rm = TRUE),
              weekly_wtrtemp_min = max(tempC, na.rm = TRUE),
              weekly_wtrtemp_sd = sd(tempC, na.rm = TRUE))
  if(length(week1$site >0)){
  week1$date <- sampling_dates[i]
  weekly_summary <- rbind(weekly_summary, week1)}
  dat14 <- dat14 %>% filter(!date %in% week$date)
}

weekly_summary[weekly_summary == Inf] <- NA
weekly_summary[weekly_summary == -Inf] <- NA
weekly_summary[is.na(weekly_summary)] <- NA
weekly_summary <- weekly_summary[-1,]
weekly_summary$date <- as.Date(weekly_summary$date, origin = "1970-01-01")
weekly_summary <- as.tibble(weekly_summary)

#gap fill for missing dates
date <- rep(as.Date(sampling_dates), times = 4)
site <- rep(c("HCS","NBRY","SOTF","NSH"),each = 160)

obs <- as_tibble(data.frame(site,date))%>%
  mutate(site = as.character(site))

daily <- left_join(obs, dat12)
weekly <- left_join(obs, weekly_summary, by = c("site","date"))

#separate out sites to calculate weekly diff
HCSweek <- weekly %>% filter(site == "HCS")
diff <- c(NA,diff(HCSweek$weekly_wtrtemp_mean))
HCSweek$weekly_wtrtemp_diff <- diff

NBRYweek <- weekly %>% filter(site == "NBRY")
diff <- c(NA,diff(NBRYweek$weekly_wtrtemp_mean))
NBRYweek$weekly_wtrtemp_diff <- diff

NSHweek <- weekly %>% filter(site == "NSH")
diff <- c(NA,diff(NSHweek$weekly_wtrtemp_mean))
NSHweek$weekly_wtrtemp_diff <- diff

SOTFweek <- weekly %>% filter(site == "SOTF")
diff <- c(NA,diff(SOTFweek$weekly_wtrtemp_mean))
SOTFweek$weekly_wtrtemp_diff <- diff

#daily diff
HCSday <- daily %>% filter(site == "HCS")
diff <- c(NA,diff(HCSday$wtrtemp_mean))
HCSday$wtrtemp_diff <- diff

NBRYday <- daily %>% filter(site == "NBRY")
diff <- c(NA,diff(NBRYday$wtrtemp_mean))
NBRYday$wtrtemp_diff <- diff

NSHday <- daily %>% filter(site == "NSH")
diff <- c(NA,diff(NSHday$wtrtemp_mean))
NSHday$wtrtemp_diff <- diff

SOTFday <- daily %>% filter(site == "SOTF")
diff <- c(NA,diff(SOTFday$wtrtemp_mean))
SOTFday$wtrtemp_diff <- diff

onset_summary <- left_join(HCSday, HCSweek, by = c("site","date"))
write.csv(onset_summary, "./00_Data_files/Correlation_analysis_input_data/Onset_water_temp_summary.csv",row.names = FALSE)

#Site 1 (focal site for analysis)
dat6 <- dat5 %>%
  mutate(season_week = rep(c(1:20),times = 8*4),
         year = year(date)) %>%
  filter(site == "HCS") %>%
  select(year, season_week, wtrtemp_min) %>%
  spread(key = season_week, value = wtrtemp_min) %>%
  select(-year)

colnames(dat6) <- paste("wk", colnames(dat6), sep = "_")

write.csv(dat6, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site1.csv", row.names = FALSE)

#Site 2 (used to create temp. dataset for gap-filling)
dat7 <- dat5 %>%
  mutate(season_week = rep(c(1:20),times = 8*4),
         year = year(date)) %>%
  filter(site == "SOTF") %>%
  select(year, season_week, wtrtemp_min) %>%
  spread(key = season_week, value = wtrtemp_min) %>%
  select(-year)

colnames(dat7) <- paste("wk", colnames(dat7), sep = "_")

write.csv(dat7, "./00_Data_files/Bayesian_model_input_data/wtrtemp_min_Site2.csv", row.names = FALSE)


