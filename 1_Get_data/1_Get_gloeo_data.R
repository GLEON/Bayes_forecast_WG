# Script to download and wrangle Gloeo data from EDI
# Last updated 2020 April 9 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

# Load gloeo data ####

#TEMPORARY: get signed into google drive package; once you are signed in, return to R
#only necessary to run once
#drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")

# Alternative way to get file ID
#as_id(drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")$id)

#download data file into appropriate local folder
drive_download("~/GLEON_Bayesian_WG/EDI.data.clones/gloeo.counts/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv",
               path = "./00_Data_files/EDI_data_clones/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv", overwrite = TRUE)

drive_download(file = as_id("1zjPFCU8Lf-OExyNADGPFGlTMcJOkhsz0"),
               path = "./00_Data_files/EDI_data_clones/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv", overwrite = TRUE)

# alternative waying combining both steps of finding and downloading file
drive_download(
  file = as_id(drive_find(pattern = "weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")$id),
  path = "./00_Data_files/EDI_data_clones/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv", overwrite = TRUE)

#load data into R
dat <- read_csv("./00_Data_files/EDI_data_clones/weekly.surface.gloeo_4sites_2005-2016_v31Mar2020.csv")

# Create tibble with rows for missing observations in 2009-2016 ####
date <- c(rep(c("2009-06-11","2009-09-28","2015-06-25","2016-06-09","2016-08-10"),times = 1,each = 4),"2014-07-17","2015-06-11","2015-07-23","2015-08-20","2016-08-04")
site <- c(rep(c("NorthSunapeeHarbor","SouthoftheFells","HerrickCoveSouth","Newbury"),times = 5),"Newbury","SouthoftheFells","HerrickCoveSouth","SouthoftheFells","NorthSunapeeHarbor")

odd_obs <- as_tibble(cbind(site,date)) %>%
  mutate(date = as.Date(date))
odd_obs$year <- year(odd_obs$date)
odd_obs$dayofyr <- yday(odd_obs$date)
odd_obs$coloniesperL <- NA
odd_obs$filbundperL <- NA
odd_obs$totalperL <- NA

#isolate and average duplicate samples on same day -
dup_days <- c("2010-06-10", "2012-02-16", "2016-08-15")

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
  filter(!date %in% as.Date(c("2009-07-11","2010-06-10","2012-02-16", "2015-09-20","2015-10-10","2016-08-15")))#get rid of extra sampling days

#combine missing and averaged duplicate observations with original data file
#assign "sampling season weeks" numbering 1-20 each year to dates
dat2 <- bind_rows(dat1, odd_obs) %>%
  mutate(week = week(date)) %>%
  filter(year %in% 2009:2016 & (week %in% 21:40 | dayofyr == 283))%>% #limit to study period
  arrange(date, site) %>%
  select(date, site, coloniesperL, filbundperL, totalperL, year, dayofyr) %>% #keep day of year too
  mutate(season_week = rep(c(1:20),times = 8, each = 4))

# Filter for Herrick Cove South site, add month and ln of gloeo column ####
# Time period = 2009-2016, weeks 21-40 (last week of May to 1st week of Oct)

hc_gloeo_data <- dat2 %>%
  filter(site == "HerrickCoveSouth") %>%
  mutate(month = month(date)) %>%
  select(date,year,month,dayofyr,season_week,site,coloniesperL,filbundperL,totalperL) %>% # rearrange columns to have date items together
  mutate(ln_totalperL = log(totalperL + (1/141.4)))# add ln of gloeo, Total per L is Volume of 2, ~1 m net tows = 141.4 L
# convert to natural log using  log + detection limit = 1/141.4

write_csv(hc_gloeo_data, "./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv")

