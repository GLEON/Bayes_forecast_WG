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

#make vector of model names for model for-loop
my_models <- c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","GDD","wnd_dir_2day_lag","GDD_test")

#set years and weeks for hindcasting for-loop
yrs <- c(2015,2016)
wks <- c(1:20)

########################RUN HINDCASTS##############################################

for (i in 9:length(my_models)){

#1) Model options => pick model -----------------------------------------------------
model_name = my_models[i] # options are found in 4.1_JAGS_models
model=paste0("4.1_JAGS_models/",model_name, '.R') #Do not edit

for (j in 1:length(yrs)){

for (k in 1:length(wks)){

#2) Source helper functions ---------------------------------------------------------
source('0_Function_library/model_hindcasting_plug_n_play.R')
source('0_Function_library/hindcasting_get_data.R')
source('0_Function_library/hindcasting_get_params.R')
source('0_Function_library/hindcasting_run_hindcast.R')
source('0_Function_library/hindcasting_get_covar_hindcasts.R')


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
jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

#5) Set up initial conditions for hindcasts

#set up number of draws for initial condition distributions at beginning of season
Nmc = 5000

#sample rows from the re-calibrated model output for initial conditions during season
prow = sample.int(nrow(out),Nmc,replace=TRUE)

  if(wks[k]==1){ #first week of season uses initial conditions prior from Maine lakes

    IC = rnorm(Nmc,-5,sqrt(1/100))

  } else if(wks[k] %in% c(2:20) & yrs[j] == 2015) { #other weeks use last observed time point + observation error OR draws from imputed value distribution + observation error

      mycol <- paste0("mu","[7,",wks[k-1],"]")
      IC = out[prow,mycol]

  } else {

      mycol <- paste0("mu","[8,",wks[k-1],"]")
      IC = out[prow,mycol]
  }

###gap-fill missing covariate values using latent states from calibrated model
covar_ls <- out[,grep("covar", colnames(out))]
missing <- which(is.na(hindcast_data$covar_hindcast))

for (m in 1:length(missing)){
  hindcast_data$covar_hindcast[missing[m]] <- mean(covar_ls[,missing[m]],na.rm = TRUE)
}

#set up sampling of covariate hindcasting ensemble for models with covariates
if(!model_name %in% c("RW","RW_obs","AR")){
#set up sampling for hindcasted covariates
if(yrs[j] == 2015){year_no <- c(1:6)}else{year_no <- c(1:7)}
#set up draws from covariate matrix to account for intraanual correlation
yrsamp <- sample(year_no, Nmc, replace = TRUE)}

#6) Run deterministic hindcast (no sources of uncertainty included)

#retrieve parameters for deterministic hindcast
params.det <- get_params(model_name = model_name,
                         forecast_type = "det", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                         posteriors = out,
                         num_draws = prow)

#get hindcasted covariates
if(model_name %in% c("RW","RW_obs","AR")){
  covar.hindcast.det <- NA
} else {
  covar.hindcast.det <- get_covar_hindcasts(forecast_type = "det",
                                           wk = wks[k],
                                           yrsamp = yrsamp,
                                           Nmc = 1,
                                           covar_ensemble = hindcast_data$covar_hindcast)
}

#run deterministic hindcast
det.prediction <- run_hindcast(model_name = model_name,
                           params = params.det,
                           Nmc = 1,
                           IC = mean(IC),
                           wk = wks[k],
                           covar_hindcast = covar.hindcast.det)

write.csv(det.prediction,file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_det.prediction_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

#6) Run hindcasts adding one source of uncertainty at a time

######## initial condition uncertainty #######

#retrieve parameters for hindcast
params.IC <- get_params(model_name = model_name,
                         forecast_type = "IC", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                         posteriors = out,
                         num_draws = prow)

#get hindcasted covariates
if(model_name %in% c("RW","RW_obs","AR")){
  covar.hindcast.IC <- NA
} else {
  covar.hindcast.IC <- get_covar_hindcasts(forecast_type = "IC",
                                             wk = wks[k],
                                             yrsamp = yrsamp,
                                             Nmc = 1,
                                             covar_ensemble = hindcast_data$covar_hindcast)
}

#run hindcast
hindcast.IC <- run_hindcast(model_name = model_name,
                               params = params.IC,
                               Nmc = Nmc,
                               IC = IC,
                               wk = wks[k],
                               covar_hindcast = covar.hindcast.IC)

#write hindcast to file
write.csv(hindcast.IC,file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.IC_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)


###### process uncertainty #########

#retrieve parameters for hindcast
params.IC.P <- get_params(model_name = model_name,
                        forecast_type = "IC.P", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                        posteriors = out,
                        num_draws = prow)

#get hindcasted covariates
if(model_name %in% c("RW","RW_obs","AR")){
  covar.hindcast.IC.P <- NA
} else {
  covar.hindcast.IC.P <- get_covar_hindcasts(forecast_type = "IC.P",
                                                wk = wks[k],
                                                yrsamp = yrsamp,
                                                Nmc = 1,
                                                covar_ensemble = hindcast_data$covar_hindcast)
}

#run hindcast
hindcast.IC.P <- run_hindcast(model_name = model_name,
                            params = params.IC.P, #list of params necessary to run that model
                            Nmc = Nmc,
                            IC = IC,
                            wk = wks[k],
                            covar_hindcast = covar.hindcast.IC.P) #list of settings including N_out, Nmc, and IC

#write hindcast to file
write.csv(hindcast.IC.P,file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.IC.P_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)


if(!model_name %in% c("RW","RW_obs")){
###### parameter uncertainty #######

  #retrieve parameters for hindcast
  params.IC.P.Pa <- get_params(model_name = model_name,
                            forecast_type = "IC.P.Pa", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                            posteriors = out,
                            num_draws = prow)

  #get hindcasted covariates
  if(model_name %in% c("RW","RW_obs","AR")){
    covar.hindcast.IC.P.Pa <- NA
  } else {
    covar.hindcast.IC.P.Pa <- get_covar_hindcasts(forecast_type = "IC.P.Pa",
                                                  wk = wks[k],
                                                  yrsamp = yrsamp,
                                                  Nmc = 1,
                                                  covar_ensemble = hindcast_data$covar_hindcast)
  }

  #run hindcast
  hindcast.IC.P.Pa <- run_hindcast(model_name = model_name,
                                params = params.IC.P.Pa, #list of params necessary to run that model
                                Nmc = Nmc,
                                IC = IC,
                                wk = wks[k],
                                covar_hindcast = covar.hindcast.IC.P.Pa) #list of settings including N_out, Nmc, and IC

  #write hindcast to file
  write.csv(hindcast.IC.P.Pa,file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.IC.P.Pa_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

}

if(!model_name %in% c("RW","RW_obs","AR")){

###### driver uncertainty ##########

  #retrieve parameters for hindcast
  params.IC.P.Pa.D <- get_params(model_name = model_name,
                               forecast_type = "IC.P.Pa.D", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                               posteriors = out,
                               num_draws = prow)

  #get hindcasted covariates
  covar.hindcast.IC.P.Pa.D <- get_covar_hindcasts(forecast_type = "IC.P.Pa.D",
                                                wk = wks[k],
                                                yrsamp = yrsamp,
                                                Nmc = Nmc,
                                                covar_ensemble = hindcast_data$covar_hindcast)

  #run hindcast
  hindcast.IC.P.Pa.D <- run_hindcast(model_name = model_name,
                                   params = params.IC.P.Pa.D, #list of params necessary to run that model
                                   Nmc = Nmc,
                                   IC = IC,
                                   wk = wks[k],
                                   covar_hindcast = covar.hindcast.IC.P.Pa.D) #list of settings including N_out, Nmc, and IC

  #write hindcast to file
  write.csv(hindcast.IC.P.Pa.D,file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.IC.P.Pa.D_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

}

###### observation uncertainty #########

#retrieve parameters for hindcast
params.w_obs <- get_params(model_name = model_name,
                               forecast_type = "w_obs", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                               posteriors = out,
                               num_draws = prow)

#get hindcasted covariates
if(model_name %in% c("RW","RW_obs","AR")){
  covar.hindcast.w_obs <- NA
} else {
  covar.hindcast.w_obs <- get_covar_hindcasts(forecast_type = "w_obs",
                                                wk = wks[k],
                                                yrsamp = yrsamp,
                                                Nmc = Nmc,
                                                covar_ensemble = hindcast_data$covar_hindcast)
}

#run hindcast
hindcast.w_obs <- run_hindcast(model_name = model_name,
                                   params = params.w_obs, #list of params necessary to run that model
                                   Nmc = Nmc,
                                   IC = IC,
                                   wk = wks[k],
                                   covar_hindcast = covar.hindcast.w_obs) #list of settings including N_out, Nmc, and IC

#write hindcast to file
write.csv(hindcast.w_obs,file=file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.w_obs_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

}}}

##Hooray! You have hindcasted all the G. echinulata. Treat yourself.
