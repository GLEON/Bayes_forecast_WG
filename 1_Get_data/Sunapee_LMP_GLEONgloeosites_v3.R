#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   Sunapee_LMP_GLEONgloeosites_v3.r                     *
#* AUTHOR:  Bethel Steele                                        *
#* SYSTEM:  Lenovo ThinkCentre, Win 10, R 4.0.4, RStudio 1.4.1103*
#* DATE:    30Oct2017                                            *
#* PROJECT: Lake Sunapee Data Cleaning - GLEON                   *
#* PURPOSE: subset the LSPA LMP files for associated data at     *
#*          Midge (site110) and Newbury (site90)                 *
#* UPDATED: 25Feb2019                                            *
#* UPDATES: Summarize Secchi and TP data                         *
#* UPDAtED: 31March2021                                          *
#* UPDATES: call in DOI master file                              *
#*****************************************************************

library(tidyverse)
library(readr)

sunapee_master_http <- 'https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv'

sunapee_master <- read_csv(sunapee_master_http,
                           col_types = cols(.default = col_character()))

#subset for sites 110 and 90, secchi and TP, 2009-2016, May-Oct
sunapee_filter <- sunapee_master %>%
  #filter(station == 110) %>%
  filter(parameter == 'secchidepth_m' | parameter == 'TP_mgl') %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(date),
         month = as.numeric(format(date, '%m')),
         value = as.numeric(value)) %>%
  filter(date >= as.Date('2009-01-01') & date < as.Date('2017-01-01') & month >= 5 & month < 11)

# convert TP to ugl
sunapee_filter <- sunapee_filter %>%
  mutate(value = case_when(parameter == 'TP_mgl' ~ value * 1000,
                           TRUE ~ value),
         parameter = case_when(parameter == 'TP_mgl' ~ 'TP_ugl',
                               TRUE ~ parameter))

sunapee_summary <- sunapee_filter %>%
  group_by(parameter)%>%
  summarise(mean_value = round(mean(value), digits = 2),
            SD_value = round(sd(value), digits = 2),
            n_value = length(value))

# to save csv, enter file path
# write_csv(sunapee_summary, '')

