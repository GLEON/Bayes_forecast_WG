# Script to download and wrangle Onset water temp data for Site 2 (South of the Fells) from EDI
# Last updated 2020 May 21 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, openair, zoo)

# Download data from EDI to local folder ####

# High-frequency temperature data from four near-shore sites, Lake Sunapee, NH, USA, 2006-2018
# EDI Package ID: edi.498.1
# Citation: Cottingham, K.L., C.C. Carey, and K.C. Weathers. 2020. High-frequency temperature data from four near-shore sites, Lake Sunapee, NH, USA, 2006-2018 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/3e325757f0e981d91cd297f257f05f55. Accessed 2020-05-21.

data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.498.1&entityid=b4f60789ceb87db613924ca43a2f71ed"

destination <- "./00_Data_files/EDI_data_clones"

download.file(data,destfile = "./00_Data_files/EDI_data_clones/temp_2006-2018_QAQC_vert_09May2020.csv", method='libcurl')

# Load onset water temp data into R ####
dat <- read_csv("./00_Data_files/EDI_data_clones/temp_2006-2018_QAQC_vert_09May2020.csv",
                col_types = list(year = col_double(),
                                 dayofyr = col_double(),
                                 time = col_time(),
                                 datetime = col_datetime(),
                                 site = col_character(),
                                 temp_degC = col_double()))


# SAME DATA WRANGLING BUT FOR SITE 2 (South of the Fells) ####
#limit logger data to sampling years and remove 30 min data in 2009 so consistent with other years
dat1 <- dat %>%
  mutate(date = date(datetime)) %>%
  filter(year %in% 2009:2016) %>%
  filter(site=="SouthOfTheFells") %>%
  mutate(min = minute(time)) %>%
  filter(min!=30) %>%
  select(-min) %>%
  select(datetime, date, year, dayofyr, time, site, temp_degC)

# plot data to check
plot(temp_degC ~ datetime, data = dat1)

# Low data at end of 2014 when logger was removed from lake, so changed to NA for 2014-09-17 10:00:00 - 2014-09-18 08:00:00

dat1[17138:17160, 7] <- NA

# Add full time series to data to be able to calculate daily averages but cut-off days with water temp for only part of day
full_datetime <- seq(from = ymd_hms("2009-05-21 00:00:00"), to = ymd_hms("2016-10-05 23:00:00"), by = dhours(1))

full_datetime.df <- as_tibble_col(full_datetime, column_name = "datetime")


# Join full time series with water temp data
dat2 <- left_join(full_datetime.df, dat1, by = "datetime")

# Summarize hourly water temp to daily data ####

# open air function needs date column so renamed datetime
dat3 <- dat2 %>%
  select(-date) %>%
  rename(date = datetime)

# Set threshold to 75% of hourly data needs to be present to calculate the daily summary

sotf_watertemp_daily_mean <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "mean", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
sotf_watertemp_daily_median <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "median", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
sotf_watertemp_daily_min <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "min", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
sotf_watertemp_daily_max <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "max", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")
sotf_watertemp_daily_sd <- timeAverage(dat3, avg.time = "day", data.thresh = 75, statistic = "sd", start.date = "2009-05-21 00:00:00", end.date = "2016-10-05 23:00:00", interval = "hour")

#Bind summaries together
sotf_watertemp_daily_summary <- bind_cols(sotf_watertemp_daily_mean[,-4], sotf_watertemp_daily_median[,5], sotf_watertemp_daily_min[,5], sotf_watertemp_daily_max[,5], sotf_watertemp_daily_sd[,5])

# rename columns
dat4 <- sotf_watertemp_daily_summary %>%
  rename(SOTF.tempC_mean = temp_degC, SOTF.tempC_median = temp_degC1, SOTF.tempC_min = temp_degC2, SOTF.tempC_max = temp_degC3,SOTF.tempC_sd = temp_degC4)


# Join with sampling dates ####
#read in sampling dates
sampling_dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")
str(sampling_dates)

# Filter water temp data for sampling dates
dat5 <- dat4 %>%
  mutate(date = date(date)) %>%
  filter(date %in% sampling_dates$date)

# Write water temp daily summary data
# write_csv(dat5, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_SOTF.csv")

# Gap fill with water temp for gloeo sample days within 1 or 2 days ####

dat4_subset <- dat4 %>%
  mutate(month = month(date)) %>%
  filter(month %in% 5:9)

dat4_subset1 <- dat4_subset[c(123, 277, 320, 430, 464, 588, 616, 739, 775, 892, 935, 1044, 1201),]

dat4_subset2 <- dat4_subset1 %>%
  mutate(date = date(date)) %>%
  filter(month == 9) %>%
  mutate(date2 = date + ddays(1))

dat4_subset2[2,10] <- ymd("2010-09-23") #2 days apart

dat4_subset3 <- dat4_subset1 %>%
  mutate(date = date(date)) %>%
  filter(month == 5|month == 6) %>%
  mutate(date2 = date - ddays(1))

# Bind new dates together
dat4_subset4 <- bind_rows(dat4_subset2, dat4_subset3) %>%
  select(-c(date, month)) %>%
  rename(date = date2) %>%
  select(date, year:SOTF.tempC_sd)

# filter dat5 to remove NA dates
dat6 <- dat5 %>%
  filter(!date %in% dat4_subset4$date)

# full join for more complete water temp data
dat7 <- full_join(dat6, dat4_subset4) %>%
  arrange(date)

# Write data for 2nd dataset with water temp holes filled in
write_csv(dat7, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled_SOTF.csv")

saveRDS(dat7, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled_SOTF.rds")

# Create 1 week lag water temp dataset ####

# Create sampling dates lag for 1 week, 1-3 days
sampling_dates_lag <- sampling_dates %>%
  mutate(date_1weeklag = date - dweeks(1)) %>%
  mutate(date_1daylag = date - ddays(1)) %>%
  mutate(date_2daylag = date - ddays(2)) %>%
  mutate(date_3daylag = date - ddays(3))

# Fill full time series data for water temp holes @ start & end gloeo data
dat4_fill <- dat4 %>%
  mutate(date = date(date)) %>%
  filter(!date %in% dat4_subset4$date)

# full join for more complete water temp data
dat4_fill2 <- full_join(dat4_fill, dat4_subset4) %>%
  arrange(date)

#Join water temp with 1 week lag date
dat4_lag <- dat4_fill2 %>%
  mutate(date_1weeklag = date(date)) %>%
  select(-c(date,year,dayofyr))


dat4_lag1 <- left_join(sampling_dates_lag[,2], dat4_lag, by = "date_1weeklag") %>%
  rename(SOTF.tempC_mean_lag = SOTF.tempC_mean, SOTF.tempC_median_lag = SOTF.tempC_median, SOTF.tempC_min_lag = SOTF.tempC_min, SOTF.tempC_max_lag = SOTF.tempC_max,SOTF.tempC_sd_lag = SOTF.tempC_sd)

# Write water temp lag data
# write_csv(dat4_lag1, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled_1weeklag_SOTF.csv")

# Calculate 1 week difference in water temp ####

dat4_fill3 <- dat4_fill2 %>%
  mutate(month = month(date)) %>%
  filter(month %in% 5:10) #shorten dataset to sampling time period

# Make vectors for output
wtr_mean_diff <- vector("double", nrow(dat4_fill3))
wtr_median_diff <- vector("double", nrow(dat4_fill3))
wtr_min_diff <- vector("double", nrow(dat4_fill3))
wtr_max_diff <- vector("double", nrow(dat4_fill3))
wtr_sd_diff <- vector("double", nrow(dat4_fill3))

for (i in 1:nrow(dat4_fill3)) {
  wtr_mean_diff[i] <- dat4_fill3$SOTF.tempC_mean[i+7] - dat4_fill3$SOTF.tempC_mean[i]
  wtr_median_diff[i] <- dat4_fill3$SOTF.tempC_median[i+7] - dat4_fill3$SOTF.tempC_median[i]
  wtr_min_diff[i] <- dat4_fill3$SOTF.tempC_min[i+7] - dat4_fill3$SOTF.tempC_min[i]
  wtr_max_diff[i] <- dat4_fill3$SOTF.tempC_max[i+7] - dat4_fill3$SOTF.tempC_max[i]
  wtr_sd_diff[i] <- dat4_fill3$SOTF.tempC_sd[i+7] - dat4_fill3$SOTF.tempC_sd[i]

  wtr_diff_output <- data.frame(date = dat4_fill3$date + ddays(7), wtr_mean_diff, wtr_median_diff, wtr_max_diff, wtr_min_diff, wtr_sd_diff)
}

# Filter water temp 1 week difference data for sampling dates
wtr_diff_output2 <- wtr_diff_output %>%
  filter(date %in% sampling_dates$date)

# Write water temp 1 week difference data
# write_csv(wtr_diff_output2, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_gap_filled_1weekdiff_SOTF.csv")

# Water Temp Moving Avg ####
# Use hourly water temp full time dataset

dat3_ma <- dat3[-c(1:24),] #start with complete day of water temp data

dat3_ma1 <- dat3_ma %>%
  mutate(ma_3 = rollmean(temp_degC, k = 72, fill = NA, align = "right")) %>% #k = hours; 72 hours = 3 days
  mutate(ma_5 = rollmean(temp_degC, k = 120, fill = NA, align = "right")) %>%
  mutate(ma_7 = rollmean(temp_degC, k = 168, fill = NA, align = "right")) %>%
  mutate(ma_10 = rollmean(temp_degC, k = 240, fill = NA, align = "right")) %>%
  mutate(ma_14 = rollmean(temp_degC, k = 336, fill = NA, align = "right"))

# filter for value @ end of day
dat3_ma2 <-  dat3_ma1 %>%
  mutate(hour = hour(time)) %>%
  filter(hour == 23) %>%
  select(-hour)

# Find end periods of moving avg
dat3_ma3 <- dat3_ma1 %>%
  #mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(date = max(date)) %>%
  filter(year!="NA")

str(dat3_ma3)

dat3_ma4 <- dat3_ma1 %>%
  filter(date %in% dat3_ma3$date)

# Bind datasets together

dat3_ma5 <- bind_rows(dat3_ma2, dat3_ma4) %>%
  arrange(date)

# Filter water temp moving avg data for sampling dates
dat3_ma6 <- dat3_ma5 %>%
  mutate(date = date(date))

dat3_ma7 <- left_join(sampling_dates, dat3_ma6) %>%
  select(date, ma_3:ma_14)

# Write water temp moving average data
# write_csv(dat3_ma7, "./00_Data_files/Covariate_analysis_data/onset_watertemp_daily_summary_movingavg_SOTF.csv")


# Growing Degree Days ####

# set base temp to 4°C - water temp limit for growth of gloeo
base_temp <- 4

gdd1 <- dat4_fill3 %>% # use daily water temp data summary for May-Sep
  mutate(gdd = ((SOTF.tempC_max + SOTF.tempC_min)/2) - base_temp) %>%
  mutate(dayofyr = yday(date)) %>%
  select(year,dayofyr,gdd) %>%
  filter(gdd!="NA") %>%
  spread(key = year, value = gdd) # make wide to do each year separately

# Calculate gdd as column sum of daily data - separate each year since different number of missing points
# Note could also try rollsum
gdd_sum1 <- as_tibble(cumsum(na.omit(gdd1$`2009`)), column_name = "gdd_sum09")
gdd_sum2 <- as_tibble(cumsum(na.omit(gdd1$`2010`)), column_name = "gdd_sum10")
gdd_sum3 <- as_tibble(cumsum(na.omit(gdd1$`2011`)), column_name = "gdd_sum11")
gdd_sum4 <- as_tibble(cumsum(na.omit(gdd1$`2012`)), column_name = "gdd_sum12")
gdd_sum5 <- as_tibble(cumsum(na.omit(gdd1$`2013`)), column_name = "gdd_sum13")
gdd_sum6 <- as_tibble(cumsum(na.omit(gdd1$`2014`)), column_name = "gdd_sum14")
gdd_sum7 <- as_tibble(cumsum(na.omit(gdd1$`2015`)), column_name = "gdd_sum15")
gdd_sum8 <- as_tibble(cumsum(na.omit(gdd1$`2016`)), column_name = "gdd_sum16")

y1 <- gdd1[1:123,1]
y2 <- gdd1[7:124,1]
y3 <- gdd1[12:124,1]
y4 <- gdd1[4:130,1]
y5 <- gdd1[2:119,1]
y6 <- gdd1[16:118,1]
y7 <- gdd1[15:126,1]
y8 <- gdd1[12:131,1]

gdd_sum09 <- bind_cols(y1, gdd_sum1)
gdd_sum10 <- bind_cols(y2, gdd_sum2)
gdd_sum11 <- bind_cols(y3, gdd_sum3)
gdd_sum12 <- bind_cols(y4, gdd_sum4)
gdd_sum13 <- bind_cols(y5, gdd_sum5)
gdd_sum14 <- bind_cols(y6, gdd_sum6)
gdd_sum15 <- bind_cols(y7, gdd_sum7)
gdd_sum16 <- bind_cols(y8, gdd_sum8)

# Left join with origina data
gdd2 <- left_join(gdd1,gdd_sum09,by = "dayofyr")
gdd3 <- left_join(gdd2,gdd_sum10,by = "dayofyr")
gdd4 <- left_join(gdd3,gdd_sum11,by = "dayofyr")
gdd5 <- left_join(gdd4,gdd_sum12,by = "dayofyr")
gdd6 <- left_join(gdd5,gdd_sum13,by = "dayofyr")
gdd7 <- left_join(gdd6,gdd_sum14,by = "dayofyr")
gdd8 <- left_join(gdd7,gdd_sum15,by = "dayofyr")
gdd_sum_all <- left_join(gdd8,gdd_sum16,by = "dayofyr")


# Fix 2010
gdd_sum_all[124,11] <- NA
gdd_sum_all[125,11] <- 2149.890

# Convert back to long
gdd_all2 <- gdd_sum_all %>%
  select(1:9) %>%
  gather(key = "year", value = "gdd", -dayofyr)

gdd_all3 <- gdd_sum_all %>%
  select(1,10:17) %>%
  gather(key = "year", value = "gdd_sum", -dayofyr)

gdd_all4 <- bind_cols(gdd_all2,gdd_all3[,3]) %>%
  mutate(year = as.numeric(year))

# Join with original water temp data to get dates back

gdd_all5 <- dat4_fill3 %>%
  mutate(dayofyr = yday(date)) %>%
  mutate(year = year(date)) %>%
  select(-month)

gdd_all6 <- left_join(gdd_all5, gdd_all4, by = c("dayofyr", "year")) %>%
  select(date, gdd, gdd_sum)

# Filter water temp 1 week difference data for sampling dates
gdd_all7 <- gdd_all6 %>%
  filter(date %in% sampling_dates$date)

# Write growing degree days data
# write_csv(gdd_all7, "./00_Data_files/Covariate_analysis_data/growing_degree_days_SOTF.csv")


# Combine all water temp files ####

watertemp_all <- bind_cols(dat7[,c(1,4:8)], dat4_lag1[,-1], wtr_diff_output2[,-1], dat3_ma7[,-1], gdd_all7[,3])

# Write all water temp data
write_csv(watertemp_all, "./00_Data_files/Covariate_analysis_data/onset_watertemp_all_SOTF.csv")

