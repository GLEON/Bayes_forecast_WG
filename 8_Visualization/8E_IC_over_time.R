#Title: 8E_IC_over_time
#History: created by MEL 26APR20

#load packages
pacman::p_load(tidyverse, lubridate)

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs_v2/"

#load data

#example varRelative dataframe from WindDir model
varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varRelative_1.csv")

#Gloeo data to find missing data time points
gloeo <- read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")
#ok. forecast wks 6, 10, 23, and 32 are missing data
png(filename = file.path(my_directory,"IC_over_time.png"),
                         width = 5, height = 4, units = "in", res = 300)
par(mgp = c(2.5,1,0))
ic <- plot(c(1:20),varRel[1,1:20], type = "l", col = "red",
           ylab = "Proportion of IC Uncertainty",
           xlab = "Forecast week in 2015", lwd = 3, cex.axis = 1.3, cex.title = 1.3, cex.lab = 1.5)
abline(v = 6, lwd = 2)
abline(v = 10, lwd = 2)
dev.off()
