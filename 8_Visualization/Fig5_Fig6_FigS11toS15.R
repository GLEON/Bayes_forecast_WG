# Title: 8A_Hindcast_output_plots
# History:
# created by MEL 18APR20
# Corresponds to Fig. XX in manuscript

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, lubridate)

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Bayes_model_analysis_output/"

#setting up counters and vectors for for-loop
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","wnd_dir_2day_lag","GDD","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD")
model_labels <- c("a. RW","b. BiasRW","c. AC","d. BaseDLM","MinWaterTemp","MinWaterTempLag","WaterTempMA","WindDir","GDD","SchmidtLag","Schmidt+Wind","e. Temp+Wind","Wind+GDD")

forecast_weeks <- c(1,4)

########################MAKE PLOTS#####################################
for (n in 1:length(forecast_weeks)){

  #read in observed data
  obs_log <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))
  obs_log <- obs_log[7:8,]

  obs_not_log <- exp(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")))-0.0035
  obs_not_log <- obs_not_log[7:8,]

  #read in sampling dates
  dates <- read_csv("./00_Data_files/sampling_dates.csv")
  dates <- dates$date[121:160]

  #subset observed data according to forecast week
  if(forecast_weeks[n] == 4){
    obs_log <- obs_log[,c(4:20)]
    obs_not_log <- obs_not_log[,c(4:20)]
    dates2015 <- dates[4:20]
    dates2016 <- dates[24:40]
  }
  if(forecast_weeks[n] == 3){
    obs_log <- obs_log[,c(3:20)]
    obs_not_log <- obs_not_log[,c(3:20)]
    dates2015 <- dates[3:20]
    dates2016 <- dates[23:40]
  }
  if(forecast_weeks[n] == 2){
    obs_log <- obs_log[,c(2:20)]
    obs_not_log <- obs_not_log[,c(2:20)]
    dates2015 <- dates[2:20]
    dates2016 <- dates[22:40]
  }
  if(forecast_weeks[n] == 1){
    dates2015 <- dates[1:20]
    dates2016 <- dates[21:40]
  }

  for (i in 1:length(model_names)){

    #read in appropriate hindcast summary files

    #confidence intervals
    if(model_names[i] %in% c("RW","RW_obs")){
      vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.P_',forecast_weeks[n],'.csv')))))}
    else if(model_names[i] %in% c("RW_bias","AC","base_DLM")){
      vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.P_',forecast_weeks[n],'.csv')))))}
    else{vardat <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.Pa.D.P_',forecast_weeks[n],'.csv')))))}

    #predictive intervals
    PI <- as.matrix(read_csv(file=file.path(paste("./6_Output_analysis/6.1_Predictive_intervals/",paste0(model_names[i],'_PI_',forecast_weeks[n],'.csv')))))

    #subset vardat and varMat according to forecast week
    if(forecast_weeks[n] == 4){
      vardat2015 <- vardat[,1:17]
      vardat2016 <- vardat[,21:37]
      PI2015 <- PI[,1:17]
      PI2016 <- PI[,21:37]
    }
    if(forecast_weeks[n] == 3){
      vardat2015 <- vardat[,1:18]
      vardat2016 <- vardat[,21:38]
      PI2015 <- PI[,1:18]
      PI2016 <- PI[,21:38]
    }
    if(forecast_weeks[n] == 2){
      vardat2015 <- vardat[,1:19]
      vardat2016 <- vardat[,21:39]
      PI2015 <- PI[,1:19]
      PI2016 <- PI[,21:39]
    }
    if(forecast_weeks[n] == 1){
      vardat2015 <- vardat[,1:20]
      vardat2016 <- vardat[,21:40]
      PI2015 <- PI[,1:20]
      PI2016 <- PI[,21:40]
    }

    #calculate mean predicted values in log space
    ci2015_log <- apply(vardat2015,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    ci2016_log <- apply(vardat2016,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

    pi2015_log <- apply(PI2015,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    pi2016_log <- apply(PI2016,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

    mean_pred_log_2015 <- colMeans(vardat2015)
    mean_pred_log_2016 <- colMeans(vardat2016)

    ci2015_not_log <- apply(exp(vardat2015)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    ci2016_not_log <- apply(exp(vardat2016)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

    pi2015_not_log <- apply(exp(PI2015)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    pi2016_not_log <- apply(exp(PI2016)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

    mean_pred_not_log_2015 <- colMeans(exp(vardat2015) - 0.0035)
    mean_pred_not_log_2016 <- colMeans(exp(vardat2016) - 0.0035)

    #plot timeseries of pred and obs on log scale
    tiff(file = file.path(paste(my_directory,paste0(model_names[i],"_timeseries_pred_and_obs_log_",forecast_weeks[n],".tif"),sep = "")),
         width = 7, height = 2.5, units = "in", res = 300)
    par(mfrow = c(1,2),mgp = c(2.5,1,0), mar = c(3,4,2,0)+0.1)

    if(forecast_weeks[n]==1){
    plot(dates2015,ci2015_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-8,5))}
    else{plot(dates2015,ci2015_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-12,8))}
    if(model_names[i]=="schmidt_med_diff"){
      title(expression(paste("e. ",Delta,"Schmidt", sep = "")),adj=0)
    } else {
    title(model_labels[i], adj = 0, font.main = 1)}
    arrows(dates2015, (pi2015_log[2,]-(pi2015_log[2,]-pi2015_log[1,])), dates2015, (pi2015_log[2,]+(pi2015_log[3,]-pi2015_log[2,])), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
    arrows(dates2015, (ci2015_log[2,]-(ci2015_log[2,]-ci2015_log[1,])), dates2015, (ci2015_log[2,]+(ci2015_log[3,]-ci2015_log[2,])), length=0.05, angle=90, code=3, lwd = 1.3)
    points(dates2015,obs_log[1,],pch = 17, col = "chartreuse3")
    # if(model_names[i] == "RW_obs" & forecast_weeks[n]==1){
    # legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n")}
    # if(model_names[i] == "RW_obs" & forecast_weeks[n]==4){
    #   legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n",cex = 0.7)}
    #legend("bottomright",legend = as.expression(bquote(bold("2015"))),bty = "n")

    if(forecast_weeks[n]==1){
      plot(dates2016,ci2016_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-8,5))}
    else{plot(dates2016,ci2016_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-12,8))}
    if(model_names[i]=="schmidt_med_diff"){
      title(expression(paste("e. ",Delta,"Schmidt", sep = "")),adj=0, font = 2)
    } else {
      title(model_labels[i], adj = 0, font.main = 1)}
    arrows(dates2016, pi2016_log[2,]-(pi2016_log[2,]-pi2016_log[1,]), dates2016, pi2016_log[2,]+(pi2016_log[3,]-pi2016_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
    arrows(dates2016, ci2016_log[2,]-(ci2016_log[2,]-ci2016_log[1,]), dates2016, ci2016_log[2,]+(ci2016_log[3,]-ci2016_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3)
    points(dates2016,obs_log[2,],pch = 17, col = "chartreuse3")
    #legend("bottomright",legend = as.expression(bquote(bold("2016"))),bty = "n")

    dev.off()

    # #plot timeseries of pred and obs on not log scale
    # tiff(file = file.path(paste(my_directory,paste0(model_names[i],"_timeseries_pred_and_obs_not_log_",forecast_weeks[n],".tif"),sep = "")),
    #      width = 5, height = 6, units = "in", res = 300)
    # par(mfrow = c(2,1),mgp = c(2.5,1,0), mar = c(3,4,0,0)+0.1)
    #
    # plot(dates2015,pi2015_not_log[2,],pch = 16,ylim = c(min(pi2015_not_log[1,])-0.1,max(pi2015_not_log[3,])+0.1),xlab = "", las = 1,ylab = expression(paste("G. echinulata (colonies",~~L^-1, ")")))
    # arrows(dates, pi2015_not_log[2,]-(pi2015_not_log[2,]-pi2015_not_log[1,]), dates, pi2015_not_log[2,]+(pi2015_not_log[3,]-pi2015_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
    # arrows(dates, ci2015_not_log[2,]-(ci2015_not_log[2,]-ci2015_not_log[1,]), dates, ci2015_not_log[2,]+(ci2015_not_log[3,]-ci2015_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3)
    # points(dates2015,obs_not_log[1,],pch = 17, col = "red")
    # legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n")
    # legend("topright",legend = "2015",bty = "n")
    #
    # plot(dates2016,pi2016_not_log[2,],pch = 16,ylim = c(min(pi2016_not_log[1,])-0.1,max(pi2016_not_log[3,])+0.1),xlab = "", las = 1,ylab = expression(paste("G. echinulata (colonies",~~L^-1, ")")))
    # arrows(dates, pi2016_not_log[2,]-(pi2016_not_log[2,]-pi2016_not_log[1,]), dates, pi2016_not_log[2,]+(pi2016_not_log[3,]-pi2016_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
    # arrows(dates, ci2016_not_log[2,]-(ci2016_not_log[2,]-ci2016_not_log[1,]), dates, ci2016_not_log[2,]+(ci2016_not_log[3,]-ci2016_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3)
    # points(dates2016,obs_not_log[2,],pch = 17, col = "red")
    # legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n")
    # legend("topright",legend = "2016",bty = "n")
    #
    # dev.off()

    ##The next bracket is the end of the model loop

  }


  ##The next bracket is the end of the week loop

}

rm(list = ls())

##################MAKING SAME PLOTS FOR MODEL ENSEMBLE###############################
#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Bayes_model_analysis_output/"

#set counters and vectors for for-loop
forecast_weeks <- c(1,4)
model_name = "ensemble"
model_label = "Ensemble"

#for-loop
for (n in 1:length(forecast_weeks)){

  #read in observed data
  obs_log <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))
  obs_log <- obs_log[7:8,]

  obs_not_log <- exp(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")))-0.0035
  obs_not_log <- obs_not_log[7:8,]

  #read in sampling dates
  dates <- read_csv("./00_Data_files/sampling_dates.csv")
  dates <- dates$date[121:160]

  #subset observed data according to forecast week
  if(forecast_weeks[n] == 4){
    obs_log <- obs_log[,c(4:20)]
    obs_not_log <- obs_not_log[,c(4:20)]
    dates2015 <- dates[4:20]
    dates2016 <- dates[24:40]
  }
  if(forecast_weeks[n] == 3){
    obs_log <- obs_log[,c(3:20)]
    obs_not_log <- obs_not_log[,c(3:20)]
    dates2015 <- dates[3:20]
    dates2016 <- dates[23:40]
  }
  if(forecast_weeks[n] == 2){
    obs_log <- obs_log[,c(2:20)]
    obs_not_log <- obs_not_log[,c(2:20)]
    dates2015 <- dates[2:20]
    dates2016 <- dates[22:40]
  }
  if(forecast_weeks[n] == 1){
    dates2015 <- dates[1:20]
    dates2016 <- dates[21:40]
  }

#read in appropriate hindcast summary files
vardat <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble.vardat.IC.Pa.D.P_',forecast_weeks[n],'.csv')))))
PI <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble_PI_',forecast_weeks[n],'.csv')))))

#subset vardat and varMat according to forecast week
if(forecast_weeks[n] == 4){
  vardat2015 <- vardat[,1:17]
  vardat2016 <- vardat[,21:37]
  PI2015 <- PI[,1:17]
  PI2016 <- PI[,21:37]
}
if(forecast_weeks[n] == 3){
  vardat2015 <- vardat[,1:18]
  vardat2016 <- vardat[,21:38]
  PI2015 <- PI[,1:18]
  PI2016 <- PI[,21:38]
}
if(forecast_weeks[n] == 2){
  vardat2015 <- vardat[,1:19]
  vardat2016 <- vardat[,21:39]
  PI2015 <- PI[,1:19]
  PI2016 <- PI[,21:39]
}
if(forecast_weeks[n] == 1){
  vardat2015 <- vardat[,1:20]
  vardat2016 <- vardat[,21:40]
  PI2015 <- PI[,1:20]
  PI2016 <- PI[,21:40]
}

#calculate mean predicted values in log space
ci2015_log <- apply(vardat2015,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
ci2016_log <- apply(vardat2016,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

pi2015_log <- apply(PI2015,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
pi2016_log <- apply(PI2016,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

mean_pred_log_2015 <- colMeans(vardat2015)
mean_pred_log_2016 <- colMeans(vardat2016)

ci2015_not_log <- apply(exp(vardat2015)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
ci2016_not_log <- apply(exp(vardat2016)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

pi2015_not_log <- apply(exp(PI2015)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
pi2016_not_log <- apply(exp(PI2016)-0.0035,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

mean_pred_not_log_2015 <- colMeans(exp(vardat2015) - 0.0035)
mean_pred_not_log_2016 <- colMeans(exp(vardat2016) - 0.0035)

#plot timeseries of pred and obs on log scale
tiff(file = file.path(paste(my_directory,paste0(model_name,"_timeseries_pred_and_obs_log_",forecast_weeks[n],".tif"),sep = "")),
     width = 7, height = 2.5, units = "in", res = 300)
par(mfrow = c(1,2),mgp = c(2.5,1,0), mar = c(3,4,2,0)+0.1)

if(forecast_weeks[n]==1){
  plot(dates2015,ci2015_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-8,5))}
else{plot(dates2015,ci2015_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-12,8))}
title(model_label, adj = 0, font.main = 1)
arrows(dates2015, (pi2015_log[2,]-(pi2015_log[2,]-pi2015_log[1,])), dates2015, (pi2015_log[2,]+(pi2015_log[3,]-pi2015_log[2,])), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
arrows(dates2015, (ci2015_log[2,]-(ci2015_log[2,]-ci2015_log[1,])), dates2015, (ci2015_log[2,]+(ci2015_log[3,]-ci2015_log[2,])), length=0.05, angle=90, code=3, lwd = 1.3)
points(dates2015,obs_log[1,],pch = 17, col = "chartreuse3")
# if(model_name == "RW_obs" & forecast_weeks[n] == 1){
#   legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n")}
# legend("bottomright",legend = "2015",bty = "n")

if(forecast_weeks[n]==1){
  plot(dates2016,ci2016_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-8,5))}
else{plot(dates2016,ci2016_log[2,],pch = 16,xlab = "", las = 1,ylab = expression(paste("log total colonies",~~L^-1)),ylim = c(-12,8))}
title(model_label, adj = 0, font.main = 1)
arrows(dates2016, pi2016_log[2,]-(pi2016_log[2,]-pi2016_log[1,]), dates2016, pi2016_log[2,]+(pi2016_log[3,]-pi2016_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
arrows(dates2016, ci2016_log[2,]-(ci2016_log[2,]-ci2016_log[1,]), dates2016, ci2016_log[2,]+(ci2016_log[3,]-ci2016_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3)
points(dates2016,obs_log[2,],pch = 17, col = "chartreuse3")
#legend("bottomright",legend = "2016",bty = "n")

dev.off()


############plot that just has legend to include in final figures
tiff(file = file.path(paste(my_directory,paste0("Fig5_Fig6_FigSX_legend.tif"),sep = "")),
     width = 4, height = 5, units = "in", res = 300)
plot.new()
legend("topleft",legend = c("median predicted","observed","95% credible interval","95% predictive interval"),pch = c(16,17,NA,NA),
       col = c("black","chartreuse3", "black","gray"),bty = "n",lty = c(NA,NA,1,1),
       lwd = c(NA,NA,2,2), cex = 1.2)

dev.off()






# #plot timeseries of pred and obs on not log scale
# tiff(file = file.path(paste(my_directory,paste0(model_name,"_timeseries_pred_and_obs_not_log_",forecast_weeks[n],".tif"),sep = "")),
#      width = 5, height = 6, units = "in", res = 300)
# par(mfrow = c(2,1),mgp = c(2.5,1,0), mar = c(3,4,0,0)+0.1)
#
# plot(dates2015,pi2015_not_log[2,],pch = 16,ylim = c(min(pi2015_not_log[1,])-0.1,max(pi2015_not_log[3,])+0.1),xlab = "", las = 1,ylab = expression(paste("G. echinulata (colonies",~~L^-1, ")")))
# arrows(dates, pi2015_not_log[2,]-(pi2015_not_log[2,]-pi2015_not_log[1,]), dates, pi2015_not_log[2,]+(pi2015_not_log[3,]-pi2015_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
# arrows(dates, ci2015_not_log[2,]-(ci2015_not_log[2,]-ci2015_not_log[1,]), dates, ci2015_not_log[2,]+(ci2015_not_log[3,]-ci2015_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3)
# points(dates2015,obs_not_log[1,],pch = 17, col = "red")
# legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n")
# legend("topright",legend = "2015",bty = "n")
#
# plot(dates2016,pi2016_not_log[2,],pch = 16,ylim = c(min(pi2016_not_log[1,])-0.1,max(pi2016_not_log[3,])+0.1),xlab = "", las = 1,ylab = expression(paste("G. echinulata (colonies",~~L^-1, ")")))
# arrows(dates, pi2016_not_log[2,]-(pi2016_not_log[2,]-pi2016_not_log[1,]), dates, pi2016_not_log[2,]+(pi2016_not_log[3,]-pi2016_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3, col = "gray")
# arrows(dates, ci2016_not_log[2,]-(ci2016_not_log[2,]-ci2016_not_log[1,]), dates, ci2016_not_log[2,]+(ci2016_not_log[3,]-ci2016_not_log[2,]), length=0.05, angle=90, code=3, lwd = 1.3)
# points(dates2016,obs_not_log[2,],pch = 17, col = "red")
# legend("topleft",legend = c("median predicted","observed"),pch = c(16,17),col = c("black","red"),bty = "n")
# legend("topright",legend = "2016",bty = "n")
#
# dev.off()

}


