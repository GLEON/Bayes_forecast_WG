# Script to download and wrangle Gloeo data from EDI
# Last updated 2020 May 22 - JB
# New Update 2022 Jan 20 - JB
# Download Newbury data to test on HC Models

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate)

# Download data from EDI to local folder ####

# Gloeotrichia echinulata density at four nearshore sites in Lake Sunapee, NH, USA from 2005-2016
# EDI Package ID: edi.497.2
# Citation: Cottingham, K.L., C.C. Carey, and K.C. Weathers. 2020. Gloeotrichia echinulata density at four nearshore sites in Lake Sunapee, NH, USA from 2005-2016 ver 2. Environmental Data Initiative. https://doi.org/10.6073/pasta/b6f418436088b14666a02467797ff1ad. Accessed 2020-05-21.

data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.497.2&entityid=9d9fbe6e3a69067569085435051d562e"

destination <- "./00_Data_files/EDI_data_clones/weekly_surface_gloeo_4sites_2005_2016_v10April2020.csv"

download.file(data, destfile = destination, method='libcurl')


# Load gloeo data into R ####
dat <- read_csv("00_Data_files/EDI_data_clones/weekly_surface_gloeo_4sites_2005_2016_v10April2020.csv",
      col_types = list(
      date = col_date(format = ""),
      site = col_character(),
      year = col_double(),
      dayofyr = col_double(),
      n_sample = col_double(),
      coloniesperL = col_double(),
      filbundperL = col_double(),
      totalperL = col_double()
    )
  )

# Create tibble with rows for missing observations in 2009-2016 ####
date <- c(rep(c("2009-06-11","2009-09-28","2015-06-25","2016-06-09","2016-08-10"),times = 1,each = 4),"2014-07-17","2015-06-11","2015-07-23","2015-08-20","2016-08-04")
site <- c(rep(c("NorthSunapeeHarbor","SouthOfTheFells","HerrickCoveSouth","Newbury"),times = 5),"Newbury","SouthOfTheFells","HerrickCoveSouth","SouthOfTheFells","NorthSunapeeHarbor")

odd_obs <- as_tibble(cbind(site,date)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date), dayofyr = yday(date)) %>%
  mutate(n_sample = NA, coloniesperL = NA, filbundperL = NA, totalperL = NA)


# eliminate extra sampling day from original data file
dat1 <- dat %>%
  filter(!date %in% as.Date(c("2009-07-11")))#get rid of extra sampling days

#combine missing and averaged duplicate observations with original data file
#assign "sampling season weeks" numbering 1-20 each year to dates
dat2 <- bind_rows(dat1, odd_obs) %>%
  mutate(week = week(date)) %>%
  filter(year %in% 2009:2016 & (week %in% 21:40 | dayofyr == 283))%>% #limit to study period
  arrange(date, site) %>%
  select(date, site, coloniesperL, filbundperL, totalperL, year, dayofyr) %>% #keep day of year too
  mutate(season_week = rep(c(1:20),times = 8, each = 4))

# Filter for Newbury site, add month and ln of gloeo column ####
# Time period = 2009-2016, weeks 21-40 (last week of May to 1st week of Oct)

nb_gloeo_data <- dat2 %>%
  filter(site == "Newbury") %>%
  mutate(month = month(date)) %>%
  select(date,year,month,dayofyr,season_week,site,coloniesperL,filbundperL,totalperL) %>% # rearrange columns to have date items together
  mutate(ln_totalperL = log(totalperL + (1/141.4)))# add ln of gloeo, Total per L is Volume of 2, ~1 m net tows = 141.4 L
# convert to natural log using  log + detection limit = 1/141.4

# Write Newbury gloeo data as a csv ####
write_csv(nb_gloeo_data, "./00_Data_files/Covariate_analysis_data/NB_Gechinulata_long.csv")

# # Write sampling dates as a csv - only needs done once, as this file will stay in repo
# sampling_dates <- tibble(hc_gloeo_data$date)
# colnames(sampling_dates) <- "date"
# write_csv(sampling_dates, "./00_Data_files/sampling_dates.csv")

# Check sampling dates for Newbury vs. Midge same for Fichter, Coffin also off by 1 same as Newbury

sampling_dates_hcs <- read_csv("00_Data_files/sampling_dates.csv")

# compare to Midge sampling dates 1 difference for 2010-09-22 (NB/Coffin) instead of 2010-09-23 (Midge/Fichter)
sampling_dates_NB_check <- nb_gloeo_data %>%
  filter(!date %in% sampling_dates_hcs$date)

# Write new sampling dates for Newbury
sampling_dates_NB <- nb_gloeo_data %>%
  select(date) %>%
  #rows_insert(tibble(date = ymd("2010-09-22"))) %>%
  arrange(date)

#write.csv(sampling_dates_NB, "00_Data_files/sampling_dates_NB.csv", row.names = F)

