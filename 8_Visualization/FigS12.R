#Title: Process error and IC error over time
#History: created by MEL 26APR20

#load packages
pacman::p_load(tidyverse, lubridate)

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs_v2/"

###########INITIAL CONDITIONS ERROR OVER TIME
#load data

#Gloeo data to find missing data time points
gloeo <- read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")
#ok. forecast wks 6, 10, 23, and 32 are missing data

#1wk
#example varRelative dataframe from AR model
varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ base_DLM_varRelative_1.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ base_DLM_varMat_1.csv")

png(filename = file.path(my_directory,"base_DLM_IC_over_time_1wk_2015.png"),
    width = 8, height = 6, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[1,1:20]), type = "l", col = "red",
           ylab = "Proportion of IC Uncertainty",
           xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20), varMat[3,1:20], type = "l", col = "blue",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, expression(paste("Hindcast variance ln(colonies",~~L^-1,")",phantom(x)^2)), cex = 1.3)
abline(v = 6, lwd = 2)
abline(v = 10, lwd = 2)
legend("topright",legend = c("IC uncertainty","variance"),col = c("red","blue"),lwd = c(2,2),bty = "n")
dev.off()

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ base_DLM_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ base_DLM_varMat_4.csv")

png(filename = file.path(my_directory,"base_DLM_IC_over_time_4wk_2015.png"),
    width = 8, height = 6, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[1,1:20]), type = "l", col = "red",
             ylab = "Proportion of IC Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20), varMat[3,1:20], type = "l", col = "blue",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, expression(paste("Hindcast variance ln(colonies",~~L^-1,")",phantom(x)^2)), cex = 1.3)
abline(v = 6, lwd = 2)
abline(v = 10, lwd = 2)
legend("topright",legend = c("IC uncertainty","variance"),col = c("red","blue"),lwd = c(2,2),bty = "n")
dev.off()





#example tradeoff in driver/process uncertainty

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wtrtemp_min_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wtrtemp_min_varMat_4.csv")

png(filename = file.path(my_directory,"wtrtemp_min_Driv_vs_Proc_4wk_2015.png"),
    width = 6, height = 5, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[3,c(18:20,1:17)] - varRel[2,c(18:20,1:17)]), type = "l", col = "chartreuse4",
             ylab = "Proportion of Driver Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20),(varRel[4,c(18:20,1:17)] - varRel[3,c(18:20,1:17)]), type = "l", col = "gray",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, "Proportion of Process Uncertainty", cex = 1.3)
legend("bottomleft",legend = c("driver uncertainty","process uncertainty"),col = c("chartreuse4","gray"),lwd = c(2,2),bty = "n")
legend("topleft",legend = "a. MinWaterTemp",bty = "n")
dev.off()

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wtrtemp_min_lag_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wtrtemp_min_lag_varMat_4.csv")

png(filename = file.path(my_directory,"wtrtemp_min_lag_Driv_vs_Proc_4wk_2015.png"),
    width = 6, height = 5, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[3,c(18:20,1:17)] - varRel[2,c(18:20,1:17)]), type = "l", col = "chartreuse4",
             ylab = "Proportion of Driver Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20),(varRel[4,c(18:20,1:17)] - varRel[3,c(18:20,1:17)]), type = "l", col = "gray",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, "Proportion of Process Uncertainty", cex = 1.3)
legend("bottomleft",legend = c("driver uncertainty","process uncertainty"),col = c("chartreuse4","gray"),lwd = c(2,2),bty = "n")
legend("topleft",legend = "b. MinWaterTempLag",bty = "n")
dev.off()

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wtrtemp_MA7_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wtrtemp_MA7_varMat_4.csv")

png(filename = file.path(my_directory,"wtrtemp_MA7_Driv_vs_Proc_4wk_2015.png"),
    width = 6, height = 5, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[3,c(18:20,1:17)] - varRel[2,c(18:20,1:17)]), type = "l", col = "chartreuse4",
             ylab = "Proportion of Driver Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20),(varRel[4,c(18:20,1:17)] - varRel[3,c(18:20,1:17)]), type = "l", col = "gray",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, "Proportion of Process Uncertainty", cex = 1.3)
legend("bottomleft",legend = c("driver uncertainty","process uncertainty"),col = c("chartreuse4","gray"),lwd = c(2,2),bty = "n")
legend("topleft",legend = "c. WaterTempMA",bty = "n")
dev.off()

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ GDD_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ GDD_varMat_4.csv")

png(filename = file.path(my_directory,"GDD_Driv_vs_Proc_4wk_2015.png"),
    width = 6, height = 5, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[3,c(18:20,1:17)] - varRel[2,c(18:20,1:17)]), type = "l", col = "chartreuse4",
             ylab = "Proportion of Driver Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20),(varRel[4,c(18:20,1:17)] - varRel[3,c(18:20,1:17)]), type = "l", col = "gray",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, "Proportion of Process Uncertainty", cex = 1.3)
legend("bottomleft",legend = c("driver uncertainty","process uncertainty"),col = c("chartreuse4","gray"),lwd = c(2,2),bty = "n")
legend("topleft",legend = "d. GDD",bty = "n")
dev.off()

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ temp_and_wind_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ temp_and_wind_varMat_4.csv")

png(filename = file.path(my_directory,"temp_and_wind_Driv_vs_Proc_4wk_2015.png"),
    width = 6, height = 5, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[3,c(18:20,1:17)] - varRel[2,c(18:20,1:17)]), type = "l", col = "chartreuse4",
             ylab = "Proportion of Driver Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20),(varRel[4,c(18:20,1:17)] - varRel[3,c(18:20,1:17)]), type = "l", col = "gray",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, "Proportion of Process Uncertainty", cex = 1.3)
legend("bottomleft",legend = c("driver uncertainty","process uncertainty"),col = c("chartreuse4","gray"),lwd = c(2,2),bty = "n")
legend("topleft",legend = "e. Temp+Wind",bty = "n")
dev.off()

varRel <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wind_and_GDD_varRelative_4.csv")
varMat <- read_csv("./5_Model_output/5.3_Uncertainty_partitioning/ wind_and_GDD_varMat_4.csv")

png(filename = file.path(my_directory,"wind_and_GDD_Driv_vs_Proc_4wk_2015.png"),
    width = 6, height = 5, units = "in", res = 300)
par(mar = c(5,5,2,5),mgp = c(2.5,1,0))
proc <- plot(c(1:20),(varRel[3,c(18:20,1:17)] - varRel[2,c(18:20,1:17)]), type = "l", col = "chartreuse4",
             ylab = "Proportion of Driver Uncertainty",
             xlab = "Forecast week in 2015", lwd = 2, cex.axis = 1.3, cex.lab = 1.5)
par(new = T)
plot(c(1:20),(varRel[4,c(18:20,1:17)] - varRel[3,c(18:20,1:17)]), type = "l", col = "gray",axes=F, xlab=NA, ylab=NA, lwd = 2)
axis(side = 4, cex.axis = 1.3, cex.title = 1.3)
mtext(side = 4, line = 3, "Proportion of Process Uncertainty", cex = 1.3)
legend("bottomleft",legend = c("driver uncertainty","process uncertainty"),col = c("chartreuse4","gray"),lwd = c(2,2),bty = "n")
legend("topleft",legend = "f. Wind+GDD",bty = "n")
dev.off()

