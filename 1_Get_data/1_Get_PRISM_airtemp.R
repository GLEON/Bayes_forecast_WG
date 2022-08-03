# Script to wrangle PRISM precip data
# **will need to update once PRISM data published on EDI
# Last updated 2021 Jan. 12 - MEL

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

# Download data from EDI to local folder ####

# Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016
# EDI staging environment Package ID: edi.18.4
# Citation: Lofton, M.E., J.A. Brentrup, W.S. Beck, J.A. Zwart, R. Bhattacharya, L.S. Brighenti, S.H. Burnet, I.M. McCullough, B.G. Steele, C.C. Carey, K.L. Cottingham, M.C. Dietze, H.A. Ewing, K.C. Weathers, and S.L. LaDeau. 2020. Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016 ver 4. Environmental Data Initiative. https://doi.org/DOI_PLACE_HOLDER (Accessed 2021-01-12).

data  <- "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.18.5&entityid=e916fc13e0163fd93b32dafd8615ad72"

destination <- "./00_Data_files/EDI_data_clones"

download.file(data, destfile = "./00_Data_files/Covariate_analysis_data/PRISM_precipitation_2009-2016.csv", method='libcurl')

# DO NOT RUN CODE BELOW THIS POINT
# It is the data wrangling code used to generate the file published on EDI.
# It is not necessary to run this code to recreate the analysis.

# Load PRISM data into R ####
airtemp <- read_csv("./00_Data_files/PRISM_met_1981_2017_sites_combined.csv",
                   col_types = list(
                     Date = col_date(format = ""),
                     coffin_ppt_mm = col_double(),
                     coffin_tmin_degreesC = col_double(),
                     coffin_tmean_degreesC = col_double(),
                     coffin_tmax_degreesC = col_double(),
                     fichter_ppt_mm = col_double(),
                     fichter_tmin_degreesC = col_double(),
                     fichter_tmean_degreesC = col_double(),
                     fichter_tmax_degreesC = col_double(),
                     midge_ppt_mm = col_double(),
                     midge_tmin_degreesC = col_double(),
                     midge_tmean_degreesC = col_double(),
                     midge_tmax_degreesC = col_double(),
                     newbury_ppt_mm = col_double(),
                     newbury_tmin_degreesC = col_double(),
                     newbury_tmean_degreesC = col_double(),
                     newbury_tmax_degreesC = col_double()
                   ))


# Limit data to sampling years and drop precip
airtemp1 <- airtemp %>%
  select(-ends_with("ppt_mm")) %>%
  mutate(year = year(Date)) %>%
  mutate(month = month(Date)) %>%
  filter(year %in% 2009:2016) %>%
  filter(month %in% 5:10)


# plot data to check - data look good
plot(newbury_tmin_degreesC ~ Date, data = airtemp1)
points(midge_tmin_degreesC ~ Date, data = airtemp1, col = "blue")

# Join with sampling dates ####
#read in sampling dates for both NB/NSH and HC/SOTF
sampling_dates_NB <- read_csv("./00_Data_files/sampling_dates_NB.csv")
sampling_dates_HC <- read_csv("./00_Data_files/sampling_dates.csv")

sampling_dates <- sampling_dates_HC %>%
  mutate(dayofyr = yday(date)) %>%
  mutate(year = year(date))


# Filter precip data for sampling dates in HC
airtemp2a <- airtemp1 %>%
  select(Date, starts_with(c("midge", "fichter"))) %>%
  filter(Date %in% sampling_dates_HC$date)

# Filter precip data for sampling dates in NB
airtemp2b <- airtemp1 %>%
  select(Date, starts_with(c("coffin", "newbury"))) %>%
  filter(Date %in% sampling_dates_NB$date)

# Join back together
airtemp2 <- full_join(airtemp2a, airtemp2b, by = "Date") %>%
  arrange(Date)

# Growing Degree Days ####

# set base temp to 4°C - water temp limit for growth of gloeo
base_temp <- 4

airtemp3 <- airtemp2 %>%
  pivot_longer(-Date, values_to = "airtemp", names_to = c("site","type"), names_sep = "_") %>%
  pivot_wider(names_from = type, values_from = airtemp) %>%
  mutate(gdd = ((tmax + tmin)/2) - base_temp) %>%
  drop_na()

airtemp3_wide <- airtemp3 %>%
  mutate(dayofyr = yday(Date)) %>%
  mutate(year = year(Date)) %>%
  pivot_wider(c(year,site,gdd,dayofyr),names_from = year, values_from = gdd) %>% # make wide to do each year separately
  pivot_wider(names_from = site, values_from = `2009`:`2016`) %>%
  arrange(dayofyr)

# Calculate cumulative gdd ####

sum(is.na(airtemp3$`2011_coffin`))

chunk1 <- rep(NA, 87)

airtemp4 <- airtemp3_wide %>%
  mutate(gdd_hc_sum09 = c(cumsum(na.omit(`2009_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum09 = c(cumsum(na.omit(`2009_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum09 = c(cumsum(na.omit(`2009_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum09 = c(cumsum(na.omit(`2009_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum10 = c(cumsum(na.omit(`2010_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum10 = c(cumsum(na.omit(`2010_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum10 = c(cumsum(na.omit(`2010_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum10 = c(cumsum(na.omit(`2010_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum11 = c(cumsum(na.omit(`2011_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum11 = c(cumsum(na.omit(`2011_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum11 = c(cumsum(na.omit(`2011_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum11 = c(cumsum(na.omit(`2011_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum12 = c(cumsum(na.omit(`2012_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum12 = c(cumsum(na.omit(`2012_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum12 = c(cumsum(na.omit(`2012_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum12 = c(cumsum(na.omit(`2012_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum13 = c(cumsum(na.omit(`2013_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum13 = c(cumsum(na.omit(`2013_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum13 = c(cumsum(na.omit(`2013_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum13 = c(cumsum(na.omit(`2013_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum14 = c(cumsum(na.omit(`2014_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum14 = c(cumsum(na.omit(`2014_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum14 = c(cumsum(na.omit(`2014_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum14 = c(cumsum(na.omit(`2014_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum15 = c(cumsum(na.omit(`2015_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum15 = c(cumsum(na.omit(`2015_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum15 = c(cumsum(na.omit(`2015_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum15 = c(cumsum(na.omit(`2015_newbury`)),chunk1)) %>%
  mutate(gdd_hc_sum16 = c(cumsum(na.omit(`2016_midge`)),chunk1)) %>%
  mutate(gdd_sotf_sum16 = c(cumsum(na.omit(`2016_fichter`)),chunk1)) %>%
  mutate(gdd_nsh_sum16 = c(cumsum(na.omit(`2016_coffin`)),chunk1)) %>%
  mutate(gdd_nb_sum16 = c(cumsum(na.omit(`2016_newbury`)),chunk1))

# Combine with correct day of year for specific year ####

airtemp_09 <- airtemp4 %>%
  select(dayofyr, contains("09")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_09_gdd <- bind_cols(airtemp_09,airtemp4[1:20,34:37]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2009)%>%
  rename(gdd_hc_sum = gdd_hc_sum09, gdd_sotf_sum = gdd_sotf_sum09, gdd_nsh_sum = gdd_nsh_sum09, gdd_nb_sum = gdd_nb_sum09)

airtemp_09_gdd2 <- full_join(airtemp_09_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2009)

airtemp_10 <- airtemp4 %>%
  select(dayofyr, contains("10")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

# add in 2 dates for Sep. 22 & 23 2010
airtemp_10 <- rbind(airtemp_10,266) %>%
  arrange(dayofyr)

airtemp_10_gdd <- bind_cols(airtemp_10,airtemp4[1:20,38:41]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2010) %>%
  rename(gdd_hc_sum = gdd_hc_sum10, gdd_sotf_sum = gdd_sotf_sum10, gdd_nsh_sum = gdd_nsh_sum10, gdd_nb_sum = gdd_nb_sum10)

airtemp_10_gdd2 <- full_join(airtemp_10_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2010)

airtemp_11 <- airtemp4 %>%
  select(dayofyr, contains("11")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_11_gdd <- bind_cols(airtemp_11,airtemp4[1:20,42:45]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2011)%>%
  rename(gdd_hc_sum = gdd_hc_sum11, gdd_sotf_sum = gdd_sotf_sum11, gdd_nsh_sum = gdd_nsh_sum11, gdd_nb_sum = gdd_nb_sum11)

airtemp_11_gdd2 <- full_join(airtemp_11_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2011)

airtemp_12 <- airtemp4 %>%
  select(dayofyr, contains("12")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_12_gdd <- bind_cols(airtemp_12,airtemp4[1:20,46:49]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2012)%>%
  rename(gdd_hc_sum = gdd_hc_sum12, gdd_sotf_sum = gdd_sotf_sum12, gdd_nsh_sum = gdd_nsh_sum12, gdd_nb_sum = gdd_nb_sum12)

airtemp_12_gdd2 <- full_join(airtemp_12_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2012)

airtemp_13 <- airtemp4 %>%
  select(dayofyr, contains("13")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_13_gdd <- bind_cols(airtemp_13,airtemp4[1:20,50:53]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2013)%>%
  rename(gdd_hc_sum = gdd_hc_sum13, gdd_sotf_sum = gdd_sotf_sum13, gdd_nsh_sum = gdd_nsh_sum13, gdd_nb_sum = gdd_nb_sum13)

airtemp_13_gdd2 <- full_join(airtemp_13_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2013)

airtemp_14 <- airtemp4 %>%
  select(dayofyr, contains("14")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_14_gdd <- bind_cols(airtemp_14,airtemp4[1:20,54:57]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2014)%>%
  rename(gdd_hc_sum = gdd_hc_sum14, gdd_sotf_sum = gdd_sotf_sum14, gdd_nsh_sum = gdd_nsh_sum14, gdd_nb_sum = gdd_nb_sum14)

airtemp_14_gdd2 <- full_join(airtemp_14_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2014)

airtemp_15 <- airtemp4 %>%
  select(dayofyr, contains("15")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_15_gdd <- bind_cols(airtemp_15,airtemp4[1:20,58:61]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2015)%>%
  rename(gdd_hc_sum = gdd_hc_sum15, gdd_sotf_sum = gdd_sotf_sum15, gdd_nsh_sum = gdd_nsh_sum15, gdd_nb_sum = gdd_nb_sum15)

airtemp_15_gdd2 <- full_join(airtemp_15_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2015)

airtemp_16 <- airtemp4 %>%
  select(dayofyr, contains("16")) %>%
  drop_na(2:5) %>%
  select(dayofyr)

airtemp_16_gdd <- bind_cols(airtemp_16,airtemp4[1:20,62:65]) %>%
  #pivot_longer(-dayofyr, names_to = c("year","site"), names_sep = "_", values_to = "gdd_sum") %>%
  mutate(year = 2016)%>%
  rename(gdd_hc_sum = gdd_hc_sum16, gdd_sotf_sum = gdd_sotf_sum16, gdd_nsh_sum = gdd_nsh_sum16, gdd_nb_sum = gdd_nb_sum16)

airtemp_16_gdd2 <- full_join(airtemp_16_gdd, sampling_dates, by = c("dayofyr", "year")) %>%
  filter(year == 2016)


# Combine all air temp gdd data ####
airtemp_gdd_all <- bind_rows(airtemp_09_gdd2, airtemp_10_gdd2, airtemp_11_gdd2, airtemp_12_gdd2, airtemp_13_gdd2, airtemp_14_gdd2, airtemp_15_gdd2, airtemp_16_gdd2) %>%
  select(date, year, gdd_hc_sum, gdd_nb_sum, gdd_nsh_sum, gdd_sotf_sum)

write_csv(airtemp_gdd_all, "./00_Data_files/Bayesian_model_input_data/PRISM_airtemp_GDD_2009-2016.csv")
