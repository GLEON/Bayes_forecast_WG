#Sub-figures for hindcasting workflow
#Author: Mary Lofton
#Date 13NOV20

#load packages
library(tidyverse)
library(lubridate)

#x_ic=-5,tau_ic = 100
fauxdata <- data.frame(rnorm(1000,-5,1/sqrt(100)))
colnames(fauxdata) <- "fauxdata"
ggplot(data = fauxdata, aes(x = fauxdata))+
  geom_density(size=1.3)+
  theme_classic()+
  xlab("log(Initial G. echinulata density)")+
  theme(axis.title = element_text(size = 16),axis.text = element_text(size = 14))

# onset water temp data
water_temp_data <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_SOTF.csv") %>%
  filter(year(date) %in% c(2009:2014)) %>%
  mutate(season_week  = rep(c(1:20),times = 6),
         Year = as.factor(year(date)))

ggplot(water_temp_data, aes(x = season_week, y = SOTF.tempC_mean, group = Year, color = Year))+
  geom_line(size = 1.3)+
  scale_color_brewer(palette = "Blues")+
  theme_classic()+
  xlab("Week of sampling season")+
  ylab("Water temp. (degrees C)")+
  theme(axis.title = element_text(size = 16),axis.text = element_text(size = 14),legend.title = element_text(size = 16),legend.text = element_text(size = 14))
