# Title: 4.3 Hindcasting and uncertainty partitioning
# History:
# adapted by JAZ from from the EcoForecast Activity
# by Michael Dietze, with reference "Ecological Forecasting", chapter 11
# updated and expanded by MEL

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, readxl, rjags, runjags, moments, coda)

#set a directory to use as a local file repository for plots if desire to write to file
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Code_publication"
write_plots <- TRUE

#make vector of model names for model for-loop
my_models <- c("RW","RW_obs","AR")

#set years and weeks for hindcasting for-loop
yrs <- c(2015,2016)
wks <- c(1:20)

########################RUN HINDCASTS##############################################

for (i in 1:length(my_models)){

#1) Model options => pick model -----------------------------------------------------
model_name = my_models[i] # options are found in 4.1_JAGS_models
model=paste0("4.1_JAGS_models/",model_name, '.R') #Do not edit

for (j in 1:length(yrs)){

for (k in 1:length(wks)){

#2) Source helper functions ---------------------------------------------------------
source('0_Function_library/model_calibration_plug_n_play.R')
source('0_Function_library/hindcasting_get_data.R')
source('0_Function_library/model_calibration_plots.R')

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 5000

#2) Read in data for model ----------------------------------------------------------

#see 0_Function_library/hindcasting_get_data.R for this function
hindcast_data <- get_hindcast_data(model_name = model_name,
                                   year = yrs[j],
                                   season_week = wks[k])

#3) JAGS Plug-Ins => initial conditions, priors, data, etc. -------------------------

#see 0_Function_library/model_calibration_plug_n_play.R for this function
jags_plug_ins <- jags_plug_ins(model_name = model_name)

#4) Re-calibrate model with data assimilation --------------------------------------
j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = jags_plug_ins$init.model,
                         n.chains = 3)

jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     adapt =  5000,
                     burnin =  10000,
                     sample = 100000,
                     n.chains = 3,
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#convert to a matrix to sample posteriors for hindcasts
out <- as.matrix(jags.out.mcmc)

#5) Set up initial conditions for hindcasts

#set up number of draws for initial condition distributions at beginning of season
Nmc = 5000

#sample rows from the re-calibrated model output for initial conditions during season
prow = sample.int(nrow(out),5000,replace=TRUE)

  if(wks[k]==1){ #first week of season uses initial conditions prior from Maine lakes

    IC = rnorm(Nmc,-5,sqrt(1/100))

  } else if(wks[k] %in% c(2:20) & yrs[j] == 2015) { #other weeks use last observed time point + observation error OR draws from imputed value distribution + observation error

    if(!is.na(cal_data$y[7,wks[k-1]])){
      IC = rnorm(Nmc,cal_data$y[7,wks[k-1]],1/sqrt(out[prow,"tau_obs"]))}
    else{
      mycol <- paste0("mu","[7,",wks[k-1],"]")
      IC = rnorm(Nmc,out[prow,mycol],1/sqrt(out[prow,"tau_obs"]))}

  } else {

    if(!is.na(cal_data$y[8,wks[k-1]])){
      IC = rnorm(Nmc,cal_data$y[8,wks[k-1]],1/sqrt(out[prow,"tau_obs"]))}
    else{
      mycol <- paste0("mu","[8,",wks[k-1],"]")
      IC = rnorm(Nmc,out[prow,mycol],1/sqrt(out[prow,"tau_obs"]))}
  }

#6) Run deterministic hindcast (no sources of uncertainty included)

#define settings for deterministic hindcast
settings.det <- list(N_out = 5, #length of forecast time points
                 Nmc = 1, #number of Monte Carlo draws
                 IC = mean(IC)) #set initial conditions (will be the same for every model)

#retrieve parameters for deterministic hindcast
params.det <- get_params(model_name = model_name,
                         forecast_type = "det",
                         posteriors = out,
                         num_draws = prow) #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D

#run deterministic hindcast
det.prediction <- forecast_gloeo(model_name = model_name,
                           params = params.det, #list of params necessary to run that model
                           settings = settings.det) #list of settings including N_out, Nmc, and IC

write.csv(det.prediction,file=file.path(my_directory,paste(site,paste0(model_name,'_det.prediction_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)

######## initial condition uncertainty #######


##Set up forecast
settings.IC <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                    Nmc = Nmc,
                    IC = IC)

params.IC <- get_params(model_name = model_name,
                        forecast_type = "IC")


#Run forecast
forecast.IC <- forecast_gloeo(model_name = model_name,
                              params = params.IC,
                              settings = settings.IC)

write.csv(forecast.IC,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)


###### process uncertainty #########

##Set up forecast
settings.IC.P <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                      Nmc = Nmc,
                      IC = IC)

params.IC.P <- get_params(model_name = model_name,
                          forecast_type = "IC.P")

#Run forecast
forecast.IC.P <- forecast_gloeo(model_name = model_name,
                                params = params.IC.P,
                                settings = settings.IC.P)

write.csv(forecast.IC.P,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)



###### observation uncertainty #########

##Set up forecast
settings.IC.P.O <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                        Nmc = Nmc,
                        IC = IC)

params.IC.P.O <- get_params(model_name = model_name,
                            forecast_type = "IC.P.O")


#Run forecast
forecast.IC.P.O <- forecast_gloeo(model_name = model_name,
                                  params = params.IC.P.O,
                                  settings = settings.IC.P.O)

write.csv(forecast.IC.P.O,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P.O_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)

if(!model_name %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
###### parameter uncertainty #######

##Set up forecast
settings.IC.P.O.Pa <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                           Nmc = Nmc,
                           IC = IC)

params.IC.P.O.Pa <- get_params(model_name = model_name,
                               forecast_type = "IC.P.O.Pa")


#Run forecast
forecast.IC.P.O.Pa <- forecast_gloeo(model_name = model_name,
                                     params = params.IC.P.O.Pa,
                                     settings = settings.IC.P.O.Pa)

write.csv(forecast.IC.P.O.Pa,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P.O.Pa_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)

}

if(!model_name %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){

###### driver uncertainty ##########

##Set up forecast
settings.IC.P.O.Pa.D <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                             Nmc = Nmc,
                             IC = IC)

params.IC.P.O.Pa.D <- get_params(model_name = model_name,
                                 forecast_type = "IC.P.O.Pa.D")


#Run forecast
forecast.IC.P.O.Pa.D <- forecast_gloeo(model_name = model_name,
                                       params = params.IC.P.O.Pa.D,
                                       settings = settings.IC.P.O.Pa.D)

write.csv(forecast.IC.P.O.Pa.D,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P.O.Pa.D_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)
}

}



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
