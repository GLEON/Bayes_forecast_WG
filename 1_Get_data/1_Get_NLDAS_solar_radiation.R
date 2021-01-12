# Script to download and wrangle Sunapee NLDAS data
# Last updated 2021 Jan. 12 - MEL

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

# load other packages
pacman::p_load(tidyverse, lubridate)

# Download data from EDI to local folder ####

# Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016
# EDI staging environment Package ID: edi.18.4
# Citation: Lofton, M.E., J.A. Brentrup, W.S. Beck, J.A. Zwart, R. Bhattacharya, L.S. Brighenti, S.H. Burnet, I.M. McCullough, B.G. Steele, C.C. Carey, K.L. Cottingham, M.C. Dietze, H.A. Ewing, K.C. Weathers, and S.L. LaDeau. 2020. Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016 ver 4. Environmental Data Initiative. https://doi.org/DOI_PLACE_HOLDER (Accessed 2021-01-12).

data  <- "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.18.4&entityid=1ebc16e71c753eb0514a13b31eab4877"

destination <- "./00_Data_files/EDI_data_clones"

download.file(data, destfile = "./00_Data_files/Covariate_analysis_data/NLDAS_solar_radiation_2009-2016.csv", method='libcurl')

# DO NOT RUN CODE BELOW THIS POINT
# It is the data wrangling code used to generate the file published on EDI.
# It is not necessary to run this code to recreate the analysis.


# # Load NLDAS data into R ####
#
# nldas <- read_csv("./00_Data_files/NLDAS_SunapeeMet_1979_2016.csv")
#
# # Filter for just short wave radiation and time period
#
# solar_rad <- nldas %>%
#   select(datetime,ShortWaveRad_Wperm2) %>%
#   mutate(year = year(datetime)) %>%
#   mutate(month = month(datetime)) %>%
#   mutate(date = date(datetime)) %>%
#   filter(year %in% 2009:2016) %>%
#   filter(month %in% 5:10)
#
# # Check data
# plot(ShortWaveRad_Wperm2 ~ datetime, data = solar_rad)
#
# sum(is.na(solar_rad$ShortWaveRad_Wperm2)) #no missing data
#
# # Summarize data into daily summary ####
# solar_rad1 <- solar_rad %>%
#   group_by(date) %>%
#   select(-c(datetime, month)) %>%
#   summarize_all(lst(mean, median, max, sd, sum)) %>%
#   select(date, starts_with("ShortWaveRad"))
#
# # Daily summary for min excluding 0 values
# solar_rad_min <- solar_rad %>%
#   filter(ShortWaveRad_Wperm2 > 0) %>%
#   group_by(date) %>%
#   select(-c(datetime, month, year)) %>%
#   summarize(ShortWaveRad_Wperm2_min = min(ShortWaveRad_Wperm2))
#
# # Bind cols and re-arrange to match other datasets
# solar_rad2 <- full_join(solar_rad1, solar_rad_min, by = "date") %>%
#   select(1:3,7,4:6)
#
# # Limit solar radiation data to sampling dates ####
#
# # Read in sampling dates
# sampling_dates <- read_csv("./00_Data_files/sampling_dates.csv")
#
# solar_rad3 <- solar_rad2 %>%
#   filter(date %in% sampling_dates$date)
#
# # Write NLDAS solar radiation data ####
# write_csv(solar_rad3, "./00_Data_files/Covariate_analysis_data/NLDAS_solar_radiation_2009-2016.csv")

