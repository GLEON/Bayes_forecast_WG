# Title: 8B_Uncertainty_partitioning_output_plots
# History:
# created by MEL 19APR20
# Corresponds to Fig. XX in manuscript

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, lubridate,cowplot)

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Uncertainty_partitioning_analysis_output/"

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","GDD_test","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")
forecast_weeks <- c(1,4)

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...)
}

###############################FIGURE xx##########################################

for (n in 1:length(forecast_weeks)){

  #read in observed data
  obs_log <- as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv"))
  obs_log <- obs_log[7:8,]

  obs_not_log <- exp(as.matrix(read_csv("./00_Data_files/Bayesian_model_input_data/Gechinulata_Site1.csv")))-0.0035
  obs_not_log <- obs_not_log[7:8,]

  #read in sampling dates
  dates <- read_csv("./00_Data_files/Bayesian_model_input_data/sampling_dates.csv")
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
      vardat.IC <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC_',forecast_weeks[n],'.csv')))))
      forecast.ci.IC <- apply(vardat.IC,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
      vardat.IC.P <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.P_',forecast_weeks[n],'.csv')))))
      forecast.ci.IC.P <- apply(vardat.IC.P,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    if(!model_names[i] %in% c("RW","RW_obs")){
      vardat.IC.P.Pa <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.P.Pa_',forecast_weeks[n],'.csv')))))
      forecast.ci.IC.P.Pa <- apply(vardat.IC.P.Pa,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    }
    if(!model_names[i] %in% c("RW","RW_obs","AR")){
      vardat.IC.P.Pa.D <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_vardat.IC.P.Pa.D_',forecast_weeks[n],'.csv')))))
      forecast.ci.IC.P.Pa.D <- apply(vardat.IC.P.Pa.D,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    }

    #subset vardat and varMat according to forecast week
    if(forecast_weeks[n] == 4){
      forecast.ci.IC.2015 <- forecast.ci.IC[,1:17]
      forecast.ci.IC.2016 <- forecast.ci.IC[,21:37]
      forecast.ci.IC.P.2015 <- forecast.ci.IC.P[,1:17]
      forecast.ci.IC.P.2016 <- forecast.ci.IC.P[,21:37]
      if(!model_names[i] %in% c("RW","RW_obs")){
      forecast.ci.IC.P.Pa.2015 <- forecast.ci.IC.P.Pa[,1:17]
      forecast.ci.IC.P.Pa.2016 <- forecast.ci.IC.P.Pa[,21:37]}
      if(!model_names[i] %in% c("RW","RW_obs","AR")){
      forecast.ci.IC.P.Pa.D.2015 <- forecast.ci.IC.P.Pa.D[,1:17]
      forecast.ci.IC.P.Pa.D.2016 <- forecast.ci.IC.P.Pa.D[,21:37]}
    }
    if(forecast_weeks[n] == 3){
      forecast.ci.IC.2015 <- forecast.ci.IC[,1:18]
      forecast.ci.IC.2016 <- forecast.ci.IC[,21:38]
      forecast.ci.IC.P.2015 <- forecast.ci.IC.P[,1:18]
      forecast.ci.IC.P.2016 <- forecast.ci.IC.P[,21:38]
      if(!model_names[i] %in% c("RW","RW_obs")){
      forecast.ci.IC.P.Pa.2015 <- forecast.ci.IC.P.Pa[,1:18]
      forecast.ci.IC.P.Pa.2016 <- forecast.ci.IC.P.Pa[,21:38]}
      if(!model_names[i] %in% c("RW","RW_obs","AR")){
      forecast.ci.IC.P.Pa.D.2015 <- forecast.ci.IC.P.Pa.D[,1:18]
      forecast.ci.IC.P.Pa.D.2016 <- forecast.ci.IC.P.Pa.D[,21:38]}
    }
    if(forecast_weeks[n] == 2){
      forecast.ci.IC.2015 <- forecast.ci.IC[,1:19]
      forecast.ci.IC.2016 <- forecast.ci.IC[,21:39]
      forecast.ci.IC.P.2015 <- forecast.ci.IC.P[,1:19]
      forecast.ci.IC.P.2016 <- forecast.ci.IC.P[,21:39]
      if(!model_names[i] %in% c("RW","RW_obs")){
      forecast.ci.IC.P.Pa.2015 <- forecast.ci.IC.P.Pa[,1:19]
      forecast.ci.IC.P.Pa.2016 <- forecast.ci.IC.P.Pa[,21:39]}
      if(!model_names[i] %in% c("RW","RW_obs","AR")){
      forecast.ci.IC.P.Pa.D.2015 <- forecast.ci.IC.P.Pa.D[,1:19]
      forecast.ci.IC.P.Pa.D.2016 <- forecast.ci.IC.P.Pa.D[,21:39]}
    }
    if(forecast_weeks[n] == 1){
      forecast.ci.IC.2015 <- forecast.ci.IC[,1:20]
      forecast.ci.IC.2016 <- forecast.ci.IC[,21:40]
      forecast.ci.IC.P.2015 <- forecast.ci.IC.P[,1:20]
      forecast.ci.IC.P.2016 <- forecast.ci.IC.P[,21:40]
      if(!model_names[i] %in% c("RW","RW_obs")){
      forecast.ci.IC.P.Pa.2015 <- forecast.ci.IC.P.Pa[,1:20]
      forecast.ci.IC.P.Pa.2016 <- forecast.ci.IC.P.Pa[,21:40]}
      if(!model_names[i] %in% c("RW","RW_obs","AR")){
      forecast.ci.IC.P.Pa.D.2015 <- forecast.ci.IC.P.Pa.D[,1:20]
      forecast.ci.IC.P.Pa.D.2016 <- forecast.ci.IC.P.Pa.D[,21:40]}
    }

    if(model_names[i] %in% c("RW","RW_obs")){
      #ic + p
      lims <- c(min(forecast.ci.IC.P.2015[1,])-0.2, max(forecast.ci.IC.P.2015[3,])+0.2)
      tiff(file = file.path(my_directory,paste0(model_names[i],"_ci_log_IC.P_",forecast_weeks[n],".tif")),
           width = 6, height = 10, units = "in", res = 300)
      par(mfrow = c(2,1),mgp = c(2.5,1,0), mar = c(3,5,4,0)+0.1)
      plot(dates2015, obs_log[1,], type = "n",
           xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
           ylim = lims, main = "1 week ahead hindcasts - 2015")
      ciEnvelope(dates2015,forecast.ci.IC.P.2015[1,],forecast.ci.IC.P.2015[3,],col="lightgray")
      ciEnvelope(dates2015,forecast.ci.IC.2015[1,],forecast.ci.IC.2015[3,],col="coral")
      lines(dates2015,forecast.ci.IC.P.2015[2,], lwd = 2)
      points(dates2015,obs_log[1,],pch = 16)

      lims <- c(min(forecast.ci.IC.P.2016[1,])-0.2, max(forecast.ci.IC.P.2016[3,])+0.2)
      plot(dates2016, obs_log[2,], type = "n",
           xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
           ylim = lims, main = "1 week ahead hindcasts - 2016")
      ciEnvelope(dates2016,forecast.ci.IC.P.2016[1,],forecast.ci.IC.P.2016[3,],col="lightgray")
      ciEnvelope(dates2016,forecast.ci.IC.2016[1,],forecast.ci.IC.2016[3,],col="coral")
      lines(dates2016,forecast.ci.IC.P.2016[2,], lwd = 2)
      points(dates2016,obs_log[2,],pch = 16)
      dev.off()

    } else if(model_names[i] == "AR"){

      #ic + p + pa
      lims <- c(min(forecast.ci.IC.P.Pa.2015[1,])-0.2, max(forecast.ci.IC.P.Pa.2015[3,])+0.2)
      tiff(file = file.path(my_directory,paste0(model_names[i],"_ci_log_IC.P.Pa_",forecast_weeks[n],".tif")),
           width = 6, height = 10, units = "in", res = 300)
      par(mfrow = c(2,1),mgp = c(2.5,1,0), mar = c(3,5,4,0)+0.1)
      plot(dates2015, obs_log[1,], type = "n",
           xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
           ylim = lims, main = "1 week ahead hindcasts - 2015")
      ciEnvelope(dates2015,forecast.ci.IC.P.Pa.2015[1,],forecast.ci.IC.P.Pa.2015[3,],col="deepskyblue4")
      ciEnvelope(dates2015,forecast.ci.IC.P.2015[1,],forecast.ci.IC.P.2015[3,],col="lightgray")
      ciEnvelope(dates2015,forecast.ci.IC.2015[1,],forecast.ci.IC.2015[3,],col="coral")
      lines(dates2015,forecast.ci.IC.P.Pa.2015[2,], lwd = 2)
      points(dates2015,obs_log[1,],pch = 16)

      lims <- c(min(forecast.ci.IC.P.Pa.2016[1,])-0.2, max(forecast.ci.IC.P.Pa.2016[3,])+0.2)
      plot(dates2016, obs_log[2,], type = "n",
           xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
           ylim = lims, main = "1 week ahead hindcasts - 2016")
      ciEnvelope(dates2016,forecast.ci.IC.P.Pa.2016[1,],forecast.ci.IC.P.Pa.2016[3,],col="deepskyblue4")
      ciEnvelope(dates2016,forecast.ci.IC.P.2016[1,],forecast.ci.IC.P.2016[3,],col="lightgray")
      ciEnvelope(dates2016,forecast.ci.IC.2016[1,],forecast.ci.IC.2016[3,],col="coral")
      lines(dates2016,forecast.ci.IC.P.Pa.2016[2,], lwd = 2)
      points(dates2016,obs_log[2,],pch = 16)
      dev.off()
    } else {
      #ic + p + pa + d
      lims <- c(min(forecast.ci.IC.P.Pa.D.2015[1,])-0.2, max(forecast.ci.IC.P.Pa.D.2015[3,])+0.2)
      tiff(file = file.path(my_directory,paste0(model_names[i],"_ci_log_IC.P.Pa.D._",forecast_weeks[n],".tif")),
           width = 6, height = 10, units = "in", res = 300)
      par(mfrow = c(2,1),mgp = c(2.5,1,0), mar = c(3,5,4,0)+0.1)
      plot(dates2015, obs_log[1,], type = "n",
           xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
           ylim = lims, main = "1 week ahead hindcasts - 2015")
      ciEnvelope(dates2015,forecast.ci.IC.P.Pa.D.2015[1,],forecast.ci.IC.P.Pa.D.2015[3,],col="darkseagreen3")
      ciEnvelope(dates2015,forecast.ci.IC.P.Pa.2015[1,],forecast.ci.IC.P.Pa.2015[3,],col="deepskyblue4")
      ciEnvelope(dates2015,forecast.ci.IC.P.2015[1,],forecast.ci.IC.P.2015[3,],col="lightgray")
      ciEnvelope(dates2015,forecast.ci.IC.2015[1,],forecast.ci.IC.2015[3,],col="coral")
      lines(dates2015,forecast.ci.IC.P.Pa.D.2015[2,], lwd = 2)
      points(dates2015,obs_log[1,],pch = 16)

      lims <- c(min(forecast.ci.IC.P.Pa.D.2016[1,])-0.2, max(forecast.ci.IC.P.Pa.D.2016[3,])+0.2)
      plot(dates2016, obs_log[2,], type = "n",
           xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
           ylim = lims, main = "1 week ahead hindcasts - 2016")
      ciEnvelope(dates2016,forecast.ci.IC.P.Pa.D.2016[1,],forecast.ci.IC.P.Pa.D.2016[3,],col="darkseagreen3")
      ciEnvelope(dates2016,forecast.ci.IC.P.Pa.2016[1,],forecast.ci.IC.P.Pa.2016[3,],col="deepskyblue4")
      ciEnvelope(dates2016,forecast.ci.IC.P.2016[1,],forecast.ci.IC.P.2016[3,],col="lightgray")
      ciEnvelope(dates2016,forecast.ci.IC.2016[1,],forecast.ci.IC.2016[3,],col="coral")
      lines(dates2016,forecast.ci.IC.P.Pa.D.2016[2,], lwd = 2)
      points(dates2016,obs_log[2,],pch = 16)
      dev.off()
    }
  }
}
rm(list = ls())

###############################FIGURE YY##########################################
#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Uncertainty_partitioning_analysis_output/"

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")
forecast_weeks <- c(1:4)

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...)
}


##make plots
for(i in 1:length(model_names)){

  if(model_names[i] %in% c("RW","RW_obs")){
    plotdata <- matrix(NA,2,4)
  }
  if(model_names[i] == "AR"){
    plotdata <- matrix(NA,3,4)
  }
  if(!model_names[i] %in% c("RW","RW_obs","AR")){
    plotdata <- matrix(NA,4,4)
  }



for(n in 1:length(forecast_weeks)){

  varpart <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv'),sep = " "))))
  plotdata[1,n] <- mean(varpart[1,], na.rm = TRUE)
  plotdata[2,n] <- mean(varpart[2,], na.rm = TRUE)

  if(!model_names[i] %in% c("RW","RW_obs")){
    plotdata[3,n] <- mean(varpart[3,], na.rm = TRUE)
  }
  if(!model_names[i] %in% c("RW","RW_obs","AR")){
    plotdata[4,n] <- mean(varpart[4,], na.rm = TRUE)
  }

}

  if(model_names[i] %in% c("RW","RW_obs")){
  tiff(file = file.path(my_directory,paste0(model_names[i],"_V.pred.rel.tif")),
       width = 8, height = 6, units = "in", res = 300)
  layout(rbind(1,2), heights=c(7,1))
  par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
  plot(forecast_weeks, plotdata[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1,xaxt = "n")
  axis(side = 1, at=1:4,cex.axis = 1.5, labels = c("1wk","2wk","3wk","4wk"))
  ciEnvelope(forecast_weeks, rep(0,ncol(plotdata)), plotdata[1,], col = "coral")
  ciEnvelope(forecast_weeks, plotdata[1,], rep(1,ncol(plotdata)), col = "lightgray")
  axis(side = 2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis = 1.5, labels = c("","","","","",""))
  par(mar=c(0, 2.1, 0, 0))
  plot.new()
  legend("center", legend=c("Initial Cond","Process"), col=c("coral","lightgray"), lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 2, bty = "n", seg.len = 0.5)
  dev.off()}

  if(model_names[i] == "AR"){
  tiff(file = file.path(my_directory,paste0(model_names[i],"_V.pred.rel.tif")),
       width = 8, height = 6, units = "in", res = 300)
  layout(rbind(1,2), heights=c(7,1))
  par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
  plot(forecast_weeks, plotdata[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1,xaxt = "n")
  axis(side = 1, at=1:4,cex.axis = 1.5, labels = c("1wk","2wk","3wk","4wk"))
  ciEnvelope(forecast_weeks, rep(0,ncol(plotdata)), plotdata[1,], col = "coral")
  ciEnvelope(forecast_weeks, plotdata[1,], plotdata[2,], col = "lightgray")
  ciEnvelope(forecast_weeks, plotdata[2,], rep(1,ncol(plotdata)), col = "deepskyblue4")
  axis(side = 2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis = 1.5, labels = c("","","","","",""))
  par(mar=c(0, 2.1, 0, 0))
  plot.new()
  legend("center", legend=c("Initial Cond","Process","Parameter"), col=c("coral","lightgray","deepskyblue4"), lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 3, bty = "n", seg.len = 0.5)
  dev.off()}

  if(!model_names[i] %in% c("RW","RW_obs","AR")){
  tiff(file = file.path(my_directory,paste0(model_names[i],"_V.pred.rel.tif")),
       width = 8, height = 6, units = "in", res = 300)
  layout(rbind(1,2), heights=c(7,1))
  par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
  plot(forecast_weeks, plotdata[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1,xaxt = "n")
  axis(side = 1, at=1:4,cex.axis = 1.5, labels = c("1wk","2wk","3wk","4wk"))
  ciEnvelope(forecast_weeks, rep(0,ncol(plotdata)), plotdata[1,], col = "coral")
  ciEnvelope(forecast_weeks, plotdata[1,], plotdata[2,], col = "lightgray")
  ciEnvelope(forecast_weeks, plotdata[2,], plotdata[3,], col = "deepskyblue4")
  ciEnvelope(forecast_weeks, plotdata[3,], rep(1,ncol(plotdata)), col = "darkseagreen3")
  axis(side = 2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis = 1.5, labels = c("","","","","",""))
  par(mar=c(0, 2.1, 0, 0))
  plot.new()
  legend("center", legend=c("Initial Cond","Process","Parameter","Driver"), col=c("coral","lightgray","deepskyblue4","darkseagreen3"), lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 4, bty = "n", seg.len = 0.5)
  dev.off()}

}

rm(list = ls())

###############################FIGURE ZZ##########################################

#set local directory for writing plots
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Uncertainty_partitioning_analysis_output/"

#setting up counters and vectors for for-loop
model_names <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_and_wnd","schmidt_diff_and_max","wnd_dir_and_speed")
letters <- c("A","B","C","D","E","F","G","H","I","J","K","L")
forecast_weeks <- c(1:4)
plotdata <- NULL

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...)
}


##make plots
for(n in 1:length(forecast_weeks)){

  plotdata0 <- matrix(NA,length(model_names),7)

for(i in 1:length(model_names)){

    plotdata0[i,1] <- model_names[i]
    plotdata0[i,2] <- letters[i]

    varpart <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv'),sep = " "))))
    plotdata0[i,3] <- mean(varpart[1,], na.rm = TRUE)
    plotdata0[i,4] <- mean(varpart[2,]-varpart[1,], na.rm = TRUE)

    if(!model_names[i] %in% c("RW","RW_obs")){
      plotdata0[i,5] <- mean(varpart[3,]-varpart[2,], na.rm = TRUE)
    }
    if(!model_names[i] %in% c("RW","RW_obs","AR")){
      plotdata0[i,6] <- mean(varpart[4,]-varpart[3,], na.rm = TRUE)
    }

    plotdata0[i,7] <- forecast_weeks[n]

  }

  plotdata <- rbind(plotdata, plotdata0)

}

#data wrangling to get in shape for plotting
plotdata <- as_tibble(plotdata) %>%
  mutate(V3 = as.double(V3),
         V4 = as.double(V4),
         V5 = as.double(V5),
         V6 = as.double(V6),
         V7 = as.double(V7))
colnames(plotdata) <- c("model_name","mod_code","Initial Cond.","Process","Parameter","Driver","forecast_week")

plotdata1 <- plotdata %>%
  gather(`Initial Cond.`:Driver, key = "var_type",value = "mean_perc_var") %>%
  mutate(var_type = factor(var_type, levels = rev(c("Initial Cond.","Process","Parameter","Driver"))))

#make plot
allvar1 <- ggplot(data = subset(plotdata1, plotdata1$forecast_week == 1), aes(x = mod_code,y = mean_perc_var,color = var_type,fill = var_type))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 14), legend.text = element_text(size = 13),
        legend.position = "top")+
  labs(fill = "Variance type:",color = "Variance type:")+
  scale_fill_manual(values = c("darkseagreen3","deepskyblue4","lightgray","coral"))+
  scale_color_manual(values = c("darkseagreen3","deepskyblue4","lightgray","coral"))+
  xlab("")+
  ylab("Proportion of forecast variance")
allvar1

allvar4 <- ggplot(data = subset(plotdata1, plotdata1$forecast_week == 4), aes(x = mod_code,y = mean_perc_var,color = var_type,fill = var_type))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 14), legend.text = element_text(size = 13),
        legend.position = "top")+
  labs(fill = "Variance type:",color = "Variance type:")+
  scale_fill_manual(values = c("darkseagreen3","deepskyblue4","lightgray","coral"))+
  scale_color_manual(values = c("darkseagreen3","deepskyblue4","lightgray","coral"))+
  xlab("")+
  ylab("Proportion of forecast variance")
allvar4

final_plot <- plot_grid(allvar1, allvar4, align = "hv", nrow = 1, ncol = 2,
                        labels=c('a','b'), label_size = 26)

ggsave(final_plot, filename = file.path(my_directory,paste0("V.pred.rel.all.models.tif")),device = "tiff",
       height = 4, width = 12, units = "in", scale = 1.2)


rm(list = ls())
