#Title: 8D: Gloeo_and_environmental_covariate_timeseries
#History: created by MEL on 25APR20

#load packages
pacman::p_load(tidyverse, lubridate, lemon, grid, gridExtra, cowplot)

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Gloeo_env_covariate_timeseries/"

# Load Gloeo. data and covariates ####

# gloeo data
hc_gloeo_data <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv")

# onset water temp data
water_temp_data <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all.csv")

# schmidt stability data
schmidt_stability_data <- read_csv("./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")

# precip data
precip_data <- read_csv("./00_Data_files/Covariate_analysis_data/PRISM_precip_all.csv")

# gdd
gdd <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days.csv")

# sw radiation
swrad <- read_csv("./00_Data_files/Covariate_analysis_data/solar_radiation_daily_summary.csv")

# par
par <- read_csv("./00_Data_files/Covariate_analysis_data/par_daily_summary.csv")

# wind speed data - a little data wrangling to fill filtered variables with 0 when
# wind blowing away from cove
wind_data <- read_csv("./00_Data_files/Covariate_analysis_data/wind_data.csv")

#join all covariate data with gloeo
covariates_all <- bind_cols(hc_gloeo_data[,c(1:10)], water_temp_data[,-1], schmidt_stability_data[,-1], precip_data[,-1], gdd[,3], swrad[,-1], par[,-1], wind_data[-1]) %>%
  select(date, year, totalperL, HCS.tempC_min, HCS.tempC_min_lag, ma_7, schmidt.stability_median_diff,
         schmidt.stability_max_lag, gdd_sum, precip_mm, AveWindDir_cove_mean_2daylag)

#write plot theme
mytheme1 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=16), axis.text.y = element_text(size = 14),
                 panel.border = element_rect(colour = "black", fill = NA),
                 strip.text.x = element_text(face = "bold"),
                 strip.background.x = element_blank(),
                 axis.text.x = element_blank(),
                 plot.margin = unit(c(0, 1, 0, 0), "cm"),
                 panel.spacing = unit(-1, "lines"))

first4 <- covariates_all %>%
  filter(year %in% c(2009:2012))

gloeo1 <- ggplot(data = first4, aes(x = date, y = totalperL))+
  facet_rep_wrap(vars(year), nrow = 1, ncol = 4, scales = "free_x")+
  ylab("")+
  xlab("")+
  ylim(0,82)+
  mytheme1+
  geom_point(colour = "chartreuse3")+
  geom_line(size = 1, colour = "chartreuse3")
gloeo1

mytheme2 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  legend.key = element_blank(),legend.background = element_blank(),
                  text = element_text(size=16), axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 14),
                  panel.border = element_rect(colour = "black", fill = NA),
                  strip.text.x = element_blank(),
                  strip.background.x = element_blank(),
                  plot.margin = unit(c(0, 1, 0, 0), "cm"),
                  panel.spacing = unit(-1, "lines"))

gloeo2 <- ggplot(data = first4, aes(x = date, y = totalperL))+
  facet_rep_wrap(vars(year), nrow = 1, ncol = 4, scales = "free_x")+
  #ylab(my_y_title)+
  xlab("")+
  ylim(0,5)+
  ylab("")+
  mytheme2+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")
gloeo2

second4 <- covariates_all %>%
  filter(year %in% c(2013:2016))

gloeo3 <- ggplot(data = second4, aes(x = date, y = totalperL))+
  facet_rep_wrap(vars(year), nrow = 1, ncol = 4, scales = "free_x")+
  ylab("")+
  xlab("")+
  ylim(0,82)+
  mytheme1+
  geom_point(colour = "chartreuse3")+
  geom_line(size = 1, colour = "chartreuse3")
gloeo3

gloeo4 <- ggplot(data = second4, aes(x = date, y = totalperL))+
  facet_rep_wrap(vars(year), nrow = 1, ncol = 4, scales = "free_x")+
  #ylab(my_y_title)+
  xlab("")+
  ylim(0,5)+
  ylab("")+
  mytheme2+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")
gloeo4

plot<-plot_grid(gloeo1,gloeo2,gloeo3,gloeo4, align='v', vjust=1, scale = 1,
                nrow = 4, ncol = 1,
                rel_heights = c(1.0, 0.7, 1.0, 0.7),
                labels = c("a","b","c","d"))
my_y_title <- expression(paste(italic("G. echinulata"), " total colonies",~~L^-1))

y.grob <- textGrob(my_y_title,
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

#add to plot

final_plot <- grid.arrange(arrangeGrob(plot, left = y.grob))


ggsave(final_plot, filename = file.path(paste(my_directory,"gloeo_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.5)

#mintemp
mintemp <- ggplot(data = covariates_all, aes(x = date, y = HCS.tempC_min))+
  geom_point(colour = "darkblue")+
  geom_line(size = 1, colour = "darkblue")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression("Water temperature " ( degree*C)))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
mintemp
ggsave(mintemp, filename = file.path(paste(my_directory,"mintemp_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#mintemp_lag
mintemp_lag <- ggplot(data = covariates_all, aes(x = date, y = HCS.tempC_min_lag))+
  geom_point(colour = "darkblue")+
  geom_line(size = 1, colour = "darkblue")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression("Water temperature " ( degree*C)))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
mintemp_lag
ggsave(mintemp_lag, filename = file.path(paste(my_directory,"mintemp_lag_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#wtrtemp_MA
wtrtemp_MA <- ggplot(data = covariates_all, aes(x = date, y = ma_7))+
  geom_point(colour = "darkblue")+
  geom_line(size = 1, colour = "darkblue")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression("Water temperature " ( degree*C)))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
wtrtemp_MA
ggsave(wtrtemp_MA, filename = file.path(paste(my_directory,"wtrtemp_MA7_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#GDD
gdd <- ggplot(data = covariates_all, aes(x = date, y = gdd_sum))+
  geom_point(colour = "darkblue")+
  geom_line(size = 1, colour = "darkblue")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("Growing degree days")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
gdd
ggsave(gdd, filename = file.path(paste(my_directory,"GDD_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#schmidt_med_diff
smd <- ggplot(data = covariates_all, aes(x = date, y = schmidt.stability_median_diff))+
  geom_point(colour = "darkorange")+
  geom_line(size = 1, colour = "darkorange")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression(paste("Schmidt stability  ","(",J~m^-2,")")))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
smd
ggsave(smd, filename = file.path(paste(my_directory,"schmidt_med_diff_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#schmidt_max_lag
sml <- ggplot(data = covariates_all, aes(x = date, y = schmidt.stability_max_lag))+
  geom_point(colour = "darkorange")+
  geom_line(size = 1, colour = "darkorange")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression(paste("Schmidt stability  ","(",J~m^-2,")")))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
sml
ggsave(sml, filename = file.path(paste(my_directory,"schmidt_max_lag_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#precip
precip <- ggplot(data = covariates_all, aes(x = date, y = precip_mm))+
  geom_point(colour = "darkgray")+
  geom_line(size = 1, colour = "darkgray")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("millimeters")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
precip
ggsave(precip, filename = file.path(paste(my_directory,"precip_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)

#winddir
winddir <- ggplot(data = covariates_all, aes(x = date, y = AveWindDir_cove_mean_2daylag))+
  geom_point(colour = "darkgray")+
  geom_line(size = 1, colour = "darkgray")+
  facet_rep_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("Proportion of wind measurements")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
winddir
ggsave(winddir, filename = file.path(paste(my_directory,"winddir_timeseries.tif"),sep = ""),
       device = "tiff",height = 4, width = 8, units = "in", scale = 1.1)
