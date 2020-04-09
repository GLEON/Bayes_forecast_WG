##############CALCULATING TOTAL AND RELATIVE VARIANCE FROM FORECASTS
model_names <- c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error",
                 "Seasonal_AR","Seasonal_AR_Mintemp",
                 "Seasonal_DayLength_Quad","Seasonal_DayLength_Quad_Mintemp")
forecast_week = 4

for (j in 1:length(model_names)){
  my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Data_assim"
  site = "Midge"
  N_weeks <- c(1:40)

  vardat.IC <- matrix(NA, 5000, 40)
  vardat.IC <- data.frame(vardat.IC)


  vardat.IC.P <- matrix(NA, 5000, 40)
  vardat.IC.P <- data.frame(vardat.IC.P)


  vardat.IC.P.O <- matrix(NA, 5000, 40)
  vardat.IC.P.O <- data.frame(vardat.IC.P.O)

  vardat.IC.P.O.Pa <- matrix(NA, 5000, 40)
  vardat.IC.P.O.Pa <- data.frame(vardat.IC.P.O.Pa)


  vardat.IC.P.O.Pa.D <- matrix(NA, 5000, 40)
  vardat.IC.P.O.Pa.D <- data.frame(vardat.IC.P.O.Pa.D)


  for (i in 1:40){

    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC[,N_weeks[i]] <- dat[,forecast_week]

    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC.P[,i] <- dat[,forecast_week]

    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC.P.O[,i] <- dat[,forecast_week]

    if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
      dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O.Pa_',N_weeks[i],'.csv'),sep = '_')))
      vardat.IC.P.O.Pa[,i] <- dat[,forecast_week]}

    if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){
      dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O.Pa.D_',N_weeks[i],'.csv'),sep = '_')))
      vardat.IC.P.O.Pa.D[,i] <- dat[,forecast_week]}

  }

  vardat.IC <- vardat.IC[,c(1:16,21:36)]
  vardat.IC.P <- vardat.IC.P[,c(1:16,21:36)]
  vardat.IC.P.O <- vardat.IC.P.O[,c(1:16,21:36)]
  vardat.IC.P.O.Pa <- vardat.IC.P.O.Pa[,c(1:16,21:36)]
  vardat.IC.P.O.Pa.D <- vardat.IC.P.O.Pa.D[,c(1:16,21:36)]



  ### calculation of variances
  source('RCode/Helper_functions/forecast_plug_n_play_data_assim.R')

  varMat   <- make_varMat(model_name = model_names[j])
  rowMeans(varMat, na.rm = TRUE)
  write.csv(varMat, file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_names[j],'_total_var.csv'), sep = '_')),row.names = FALSE)

  ###consider adding code here to make sure the intervals are ordered from smallest to greatest
  #to avoid weird overlapping when plotting due to small decreases in predictions
  #with all uncertainties incorporated due to chance
  V.pred.rel.2015 <- apply(varMat[,1:16],2,function(x) {x/max(x)})
  V.pred.rel.2016 <- apply(varMat[,17:32],2,function(x) {x/max(x)})
  write.csv(V.pred.rel.2015,file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_names[j],'_varMat_2015.csv'), sep = '_')),row.names = FALSE)
  write.csv(V.pred.rel.2016,file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_names[j],'_varMat_2016.csv'), sep = '_')),row.names = FALSE)


  # #plot variances
  # dev.off(dev.list()["RStudioGD"])
  # plot_varMat(model_name = model_name)
  #
  # ## write stacked area plot to file
  # png(file=file.path(my_directory,paste(site,paste0(model_name,'_var_part.png'), sep = '_')), res=300, width=30, height=10, units='cm')
  # plot_varMat(model_name = model_name)
  # dev.off()
  #
  #
  # ##looking at percentile of obs in forecast distribution
  forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
  forecast_y <- exp(forecast_y[7:8,])
  forecast_ys <- forecast_y[,-c(17:20)]

  obs_quantile <- NULL
  if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
    for (i in 1:length(forecast_ys)){
      percentile1 <- ecdf(exp(vardat.IC.P.O.Pa[,i])) ##be sure to change this as needed - needs to be made into a function!!!
      obs_quantile[i] <- percentile1(forecast_ys[i])
    }} else if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){
      for (i in 1:length(forecast_ys)){
        percentile1 <- ecdf(exp(vardat.IC.P.O.Pa.D[,i])) ##be sure to change this as needed - needs to be made into a function!!!
        obs_quantile[i] <- percentile1(forecast_ys[i])
      }} else{
        for (i in 1:length(forecast_ys)){
          percentile1 <- ecdf(exp(vardat.IC.P.O[,i])) ##be sure to change this as needed - needs to be made into a function!!!
          obs_quantile[i] <- percentile1(forecast_ys[i])
        }}


  #should add vertical line at 0.5 to this
  png(file=file.path(my_directory,paste(site,paste0(model_names[j],'_obs_decile_4wk.png'), sep = '_')), res=300, width=10, height=10, units='cm')
  hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
       cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.1))
  dev.off()

  png(file=file.path(my_directory,paste(site,paste0(model_names[j],'_obs_quartile_4wk.png'), sep = '_')), res=300, width=10, height=10, units='cm')
  hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
       cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.25))
  dev.off()
}
# #a perfect forecast
# obs_quantile <- rep(0.5, 40)
# png(file=file.path(my_directory,paste("perfect_forecast.png")), res=300, width=10, height=10, units='cm')
# hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
#      cex.axis = 1.2, cex.lab = 1.2, breaks = seq(0,1,0.1))
# dev.off()
#
# #an extremely good forecast
# obs_quantile <- c(0.2, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, rep(0.5,26), 0.6, 0.6, 0.6, 0.6, 0.7, 0.7, 0.8)
# png(file=file.path(my_directory,paste("excellent_forecast.png")), res=300, width=10, height=10, units='cm')
# hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
#      cex.axis = 1.2, cex.lab = 1.2, breaks = seq(0,1,0.1))
# dev.off()
