#Title: 8E_IC_over_time
#History: created by MEL 26APR20

#load packages
pacman::p_load(tidyverse, lubridate)

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs_v2/"

#load data

#Gloeo data to find missing data time points
gloeo <- read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")
#ok. forecast wks 6, 10, 23, and 32 are missing data

#example varRelative dataframe from WindDir model
varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varRelative_1.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varMat_1.csv")


#Gloeo data to find missing data time points
gloeo <- read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")
#ok. forecast wks 6, 10, 23, and 32 are missing data

#1wk
#example varRelative dataframe from WindDir model
varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varRelative_1.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varMat_1.csv")

png(filename = file.path(my_directory,"IC_over_time_1wk.png"),
                         width = 8, height = 6, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
ic <- plot(c(1:20),varRel[1,1:20], type = "l", col = "red",
           ylab = "Proportion of IC Uncertainty",
           xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20), varMat[4,1:20], type = "l", col = "blue",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, expression(paste("Hindcast variance (colonies",~~L^-1,")",phantom(x)^2)), cex = 1.3)
abline(v = 6, lwd = 2)
abline(v = 10, lwd = 2)
legend("topleft",legend = c("IC uncertainty","variance"),col = c("red","blue"),lwd = c(2,2),bty = "n")
dev.off()

#4wk
#example varRelative dataframe from WindDir model
varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wnd_dir_2day_lag_varMat_4.csv")

png(filename = file.path(my_directory,"IC_over_time_4wk.png"),
    width = 8, height = 6, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
ic <- plot(c(4:20),varRel[1,1:17], type = "l", col = "red",
           ylab = "Proportion of IC Uncertainty",
           xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(4:20), varMat[4,1:17], type = "l", col = "blue",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, expression(paste("Hindcast variance (colonies",~~L^-1,")",phantom(x)^2)), cex = 1.3)
abline(v = 6, lwd = 2)
abline(v = 10, lwd = 2)
legend("topright",legend = c("IC uncertainty","variance"),col = c("red","blue"),lwd = c(2,2),bty = "n")
dev.off()
