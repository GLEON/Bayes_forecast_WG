#Title: 3A Format G. echinulata data for Bayesian models
#Author: Mary Lofton
#Date: 23MAR20

#Load packages

#run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

#####################################################################################################################################
#TEMPORARY: get signed into google drive package; once you are signed in, return to R
#only necessary to run once
#drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")

# Alternative way to get file ID
#as_id(drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")$id)

#download data file into appropriate local folder
drive_download("~/GLEON_Bayesian_WG/EDI.data.clones/gloeo.counts/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv",
                      path = "./00_Data_files/EDI_data_clones/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv", overwrite = TRUE)

###############METHOD THAT WORKS FOR JENNIE
# Alternative way to get file ID
#as_id(drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")$id)

# alternative way combining both steps of finding and downloading file
drive_download(
 file = as_id(drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")$id),
               path = "./00_Data_files/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv", overwrite = TRUE)
#####################################################################################################################################

#load data into R
dat <- read_csv("./00_Data_files/EDI_data_clones/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")

#create tibble with rows for missing observations in 2009-2016
date <- c(rep(c("2009-06-11","2009-09-28","2015-06-25","2016-06-09","2016-08-10"),times = 1,each = 4),"2014-07-17","2015-06-11","2015-07-23","2015-08-20","2016-08-04")
site <- c(rep(c("NorthSunapeeHarbor","SouthoftheFells","HerrickCoveSouth","Newbury"),times = 5),"Newbury","SouthoftheFells","HerrickCoveSouth","SouthoftheFells","NorthSunapeeHarbor")

odd_obs <- as_tibble(cbind(site,date)) %>%
  mutate(date = as.Date(date))
odd_obs$year <- year(odd_obs$date)
odd_obs$dayofyr <- yday(odd_obs$date)
odd_obs$coloniesperL <- NA
odd_obs$filbundperL <- NA
odd_obs$totalperL <- NA

#isolate and average duplicate samples on same day
dup_days <- c("2010-06-10","2016-08-15")

for (i in 1:length(dup_days)){
  dup_sampling_day <- dat %>%
    filter(date == dup_days[i]) %>%
    group_by(site, date, year, dayofyr)%>%
    summarize(coloniesperL = mean(coloniesperL, na.rm = TRUE),
              filbundperL = mean(filbundperL, na.rm = TRUE),
              totalperL = mean(totalperL, na.rm = TRUE))

  odd_obs <- bind_rows(odd_obs, dup_sampling_day)

}

#eliminate duplicate samples and extra sampling days from original data file
dat1 <- dat %>%
  filter(!date %in% as.Date(c("2009-07-11","2010-06-10","2015-09-20","2015-10-10","2016-08-15")))#get rid of extra sampling days

#combine missing and averaged duplicate observations with original data file
#assign "sampling season weeks" numbering 1-20 each year to dates
dat2 <- bind_rows(dat1, odd_obs) %>%
  mutate(week = week(date)) %>%
  filter(year %in% 2009:2016 & (week %in% 21:40 | dayofyr == 283))%>% #limit to study period
  arrange(date, site) %>%
  select(date, site, coloniesperL, filbundperL, totalperL, year) %>%
  mutate(season_week = rep(c(1:20),times = 8, each = 4))

#write dates to file for reference in other data wrangling scripts
dates <- dat2 %>%
  filter(site == "HerrickCoveSouth") %>%
  select(date)

write.csv(dates, "./00_Data_files/Bayesian_model_input_data/sampling_dates.csv",row.names = FALSE)

#get in wide format (year by week) for seasonal for-loop in JAGS models

#Site 1 (focal site for analysis)
dat3 <- dat2 %>%
  filter(site == "HerrickCoveSouth") %>%
  select(year, season_week, totalperL) %>%
  spread(key = season_week, value = totalperL) %>%
  select(-year)

colnames(dat3) <- paste("wk", colnames(dat3), sep = "_")

write.csv(dat3, "./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv", row.names = FALSE)

