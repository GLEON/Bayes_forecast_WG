# Title: 4.3 Hindcasting with known drivers
# History:
# adapted by JAZ from from the EcoForecast Activity
# by Michael Dietze, with reference "Ecological Forecasting", chapter 11
# updated and expanded by MEL
# 26JAN21 adjusted to use known drivers to help assess whether current method
# of hindcasting drivers is adequate

##NOTE!! BEFORE YOU BEGIN:
#this script takes many hours to run as hindcasts for all models with all
#combinations of uncertainty are generated each week of 2015-2016 within the loop

##################################SET-UP##############################################
#set options to stop the loop if there is a warning - we don't want warnings!!
options(warn=2)
#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, readxl, rjags, runjags, moments, coda, uuid, ncdf4, EML, emld)

# devtools::install_github('eco4cast/EFIstandards') # need to install from github if you don't have it; check for updates on this package of if there is a stable version on CRAN
library(EFIstandards)

#make vector of model names for model for-loop
my_models <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD")

#set years and weeks for hindcasting for-loop
yrs <- c(2015,2016)
wks <- c(1:20)

# metadata details
forecast_project_id <- 'GLEON_Bayes_forecast_WG_Gloeo_uncertainty_partition_known_covars_20210211' #An ID that applies to a bunch of forecasts

########################RUN HINDCASTS##############################################

for (i in 1:length(my_models)){

  #1) Model options => pick model -----------------------------------------------------
  model_name = my_models[i] # options are found in 4.1_JAGS_models
  model=paste0("4.1_JAGS_models/",model_name, '.R') #Do not edit

  for (j in 1:length(yrs)){

    for (k in 1:length(wks)){

      #2) Source helper functions ---------------------------------------------------------
      source('0_Function_library/model_hindcasting_plug_n_play.R')
      source('0_Function_library/hindcasting_get_data_known_covars.R') #check
      source('0_Function_library/hindcasting_get_params.R')
      source('0_Function_library/hindcasting_run_hindcast_known_covars.R') #check
      source('0_Function_library/hindcasting_get_known_covars.R') #check


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
                           sample = 50000,
                           n.chains = 3,
                           inits=jags_plug_ins$init.model,
                           monitor = jags_plug_ins$variable.namesout.model)

      #convert to a matrix to sample posteriors for hindcasts
      jags.out.mcmc <- as.mcmc.list(jags.out)
      out <- as.matrix(jags.out.mcmc)

      #5) Set up initial conditions and hindcasted drivers for hindcasts

      #set up number of draws for initial condition distributions at beginning of season
      Nmc = 7500

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
      if(model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")){
        missing <- which(is.na(hindcast_data$covar_hindcast))

        for (m in 1:length(missing)){
          hindcast_data$covar_hindcast[missing[m]] <- hindcast_data$week_avg[missing[m]]
        }
      }
      if(model_name %in% c("schmidt_and_wind","temp_and_wind","wind_and_GDD")){
        missing1 <- which(is.na(hindcast_data$covar1_hindcast))

        missing2 <- which(is.na(hindcast_data$covar2_hindcast))

        for (m in 1:length(missing1)){
          hindcast_data$covar1_hindcast[missing1[m]] <- hindcast_data$week_avg1[missing1[m]]
        }
        for (m in 1:length(missing2)){
          hindcast_data$covar2_hindcast[missing2[m]] <- hindcast_data$week_avg2[missing2[m]]
        }
      }


      #6) Run deterministic hindcast (no sources of uncertainty included)

      #retrieve parameters for deterministic hindcast
      params.det <- get_params(model_name = model_name,
                               forecast_type = "det", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                               posteriors = out,
                               num_draws = prow)

      #get hindcasted covariates
      if(model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        covar.hindcast.det <- NA
      } else if (model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")) {
        covar.hindcast.det <- get_known_covars(model_name = model_name,
                                                  forecast_type = "det",
                                                  wk = wks[k],
                                                  known_covars = list(covar = hindcast_data$covar_hindcast))
      } else {
        covar.hindcast.det <- get_known_covars(model_name = model_name,
                                                  forecast_type = "det",
                                                  wk = wks[k],
                                                  known_covars = list(covar1 = hindcast_data$covar1_hindcast, covar2 = hindcast_data$covar2_hindcast))
      }

      #run deterministic hindcast
      det.prediction <- run_hindcast(model_name = model_name,
                                     params = params.det,
                                     Nmc = 1,
                                     IC = mean(IC),
                                     wk = wks[k],
                                     covar_hindcast = covar.hindcast.det)
      # JAZ; 2020-09-11; working on forecast metadata
      # need to get the output, which is Nmc rows x 4 columns matrix, into a netcdf. Each model/uncert/year/week is a new forecast and should get a new forecast_iteration_ID
      # We probably should have a new netcdfs for each file (i.e. forecast_iteration_ID) since there is a lot of metadata that will be unique to each forecast for the partitioning uncertainty forecasts
      # nc_name_out = file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_det.prediction_',yrs[j],'_',wks[k],'.nc')))
      # eml_file_name = file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_det.prediction_',yrs[j],'_',wks[k],'.eml.xml')))
      #
      # matrix_to_ncdf(hindcast_matrix = det.prediction,
      #                Nmc = Nmc,
      #                forecast_issue_time = as.Date(cur_date),
      #                forecast_iteration_id = create_forecast_iteration_id(forecast_project_id),
      #                forecast_project_id = forecast_project_id,
      #                model_name = paste(model_name, 'det', sep = '_'),
      #                nc_name_out = nc_name_out)
      #
      # # example eml out - should make another function for detailing all ic, param, driver details and complexity for the different model names
      # create_forecast_eml(model_out_nc_file = nc_name_out,
      #                     eml_file_name = eml_file_name,
      #                     initial_conditions = 'no',
      #                     ic_complexity = NA,
      #                     parameters = 'contains',
      #                     param_complexity = 1,
      #                     random_effects = 'no',
      #                     random_complexity = NA,
      #                     drivers = 'no',
      #                     driver_complexity = NA,
      #                     process_error = 'propagates',
      #                     process_complexity = 1,
      #                     covariance = TRUE,
      #                     localization = FALSE)

      write.csv(det.prediction,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_det.prediction_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

      #6) Run hindcasts adding one source of uncertainty at a time

      ######## initial condition uncertainty #######

      #retrieve parameters for hindcast
      params.IC <- get_params(model_name = model_name,
                              forecast_type = "IC", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                              posteriors = out,
                              num_draws = prow)

      #get hindcasted covariates
      if(model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        covar.hindcast.IC <- NA
      } else if (model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")) {
        covar.hindcast.IC <- get_known_covars(model_name = model_name,
                                                 forecast_type = "IC",
                                                 wk = wks[k],
                                                 known_covars = list(covar = hindcast_data$covar_hindcast))
      } else {
        covar.hindcast.IC <- get_known_covars(model_name = model_name,
                                                 forecast_type = "IC",
                                                 wk = wks[k],
                                                 known_covars = list(covar1 = hindcast_data$covar1_hindcast, covar2 = hindcast_data$covar2_hindcast))
      }

      #run hindcast
      hindcast.IC <- run_hindcast(model_name = model_name,
                                  params = params.IC,
                                  Nmc = Nmc,
                                  IC = IC,
                                  wk = wks[k],
                                  covar_hindcast = covar.hindcast.IC)

      # nc_name_out = file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.IC_',yrs[j],'_',wks[k],'.nc')))
      # eml_file_name = file.path(paste("./5_Model_output/5.2_Hindcasting/",paste0(model_name,'_hindcast.IC_',yrs[j],'_',wks[k],'.eml.xml')))
      #
      # matrix_to_ncdf(hindcast_matrix = hindcast.IC,
      #                Nmc = Nmc,
      #                forecast_issue_time = as.Date(cur_date),
      #                forecast_iteration_id = create_forecast_iteration_id(forecast_project_id),
      #                forecast_project_id = forecast_project_id,
      #                model_name = paste(model_name, 'IC', sep = '_'),
      #                nc_name_out = nc_name_out)
      #
      # # example eml out - should make another function for detailing all ic, param, driver details and complexity for the different model names
      # create_forecast_eml(model_out_nc_file = nc_name_out,
      #                     eml_file_name = eml_file_name,
      #                     initial_conditions = 'propagates',
      #                     ic_complexity = 1,
      #                     parameters = 'contains',
      #                     param_complexity = 1,
      #                     random_effects = 'no',
      #                     random_complexity = NA,
      #                     drivers = 'no',
      #                     driver_complexity = NA,
      #                     process_error = 'propagates',
      #                     process_complexity = 1,
      #                     covariance = TRUE,
      #                     localization = FALSE)

      #write hindcast to file
      write.csv(hindcast.IC,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

      ###### parameter uncertainty #########

      if(!model_name %in% c("RW","RW_obs")){

        #retrieve parameters for hindcast
        params.IC.Pa <- get_params(model_name = model_name,
                                   forecast_type = "IC.Pa", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                                   posteriors = out,
                                   num_draws = prow)

        #get hindcasted covariates
        if(model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
          covar.hindcast.IC.Pa <- NA
        } else if (model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")) {
          covar.hindcast.IC.Pa <- get_known_covars(model_name = model_name,
                                                      forecast_type = "IC.Pa",
                                                      wk = wks[k],
                                                      known_covars = list(covar = hindcast_data$covar_hindcast))
        } else {
          covar.hindcast.IC.Pa <- get_known_covars(model_name = model_name,
                                                      forecast_type = "IC.Pa",
                                                      wk = wks[k],
                                                      known_covars = list(covar1 = hindcast_data$covar1_hindcast, covar2 = hindcast_data$covar2_hindcast))
        }
        #run hindcast
        hindcast.IC.Pa <- run_hindcast(model_name = model_name,
                                       params = params.IC.Pa, #list of params necessary to run that model
                                       Nmc = Nmc,
                                       IC = IC,
                                       wk = wks[k],
                                       covar_hindcast = covar.hindcast.IC.Pa) #list of settings including N_out, Nmc, and IC

        #write hindcast to file
        write.csv(hindcast.IC.Pa,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.Pa_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)
      }

      ###### driver uncertainty ##########

      if(!model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){

        #retrieve parameters for hindcast
        params.IC.Pa.D <- get_params(model_name = model_name,
                                     forecast_type = "IC.Pa.D", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                                     posteriors = out,
                                     num_draws = prow)

        #get hindcasted covariates
        if (model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")) {
          covar.hindcast.IC.Pa.D <- get_known_covars(model_name = model_name,
                                                        forecast_type = "IC.Pa.D",
                                                        wk = wks[k],
                                                        known_covars = list(covar = hindcast_data$covar_hindcast))
        } else {
          covar.hindcast.IC.Pa.D <- get_known_covars(model_name = model_name,
                                                        forecast_type = "IC.Pa.D",
                                                        wk = wks[k],
                                                        known_covars = list(covar1 = hindcast_data$covar1_hindcast, covar2 = hindcast_data$covar2_hindcast))
        }
        #run hindcast
        hindcast.IC.Pa.D <- run_hindcast(model_name = model_name,
                                         params = params.IC.Pa.D, #list of params necessary to run that model
                                         Nmc = Nmc,
                                         IC = IC,
                                         wk = wks[k],
                                         covar_hindcast = covar.hindcast.IC.Pa.D) #list of settings including N_out, Nmc, and IC

        #write hindcast to file
        write.csv(hindcast.IC.Pa.D,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.Pa.D_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)

      }

      ###### process uncertainty #########

      #retrieve parameters for hindcast
      params.w_proc <- get_params(model_name = model_name,
                                  forecast_type = "IC.P",
                                  posteriors = out,
                                  num_draws = prow)

      #get hindcasted covariates
      if(model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        covar.hindcast.w_proc <- NA
      } else if (model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")) {
        covar.hindcast.w_proc <- get_known_covars(model_name = model_name,
                                                     forecast_type = "IC.P",
                                                     wk = wks[k],
                                                     known_covars = list(covar = hindcast_data$covar_hindcast))
      } else {
        covar.hindcast.w_proc <- get_known_covars(model_name = model_name,
                                                     forecast_type = "IC.P",
                                                     wk = wks[k],
                                                     known_covars = list(covar1 = hindcast_data$covar1_hindcast, covar2 = hindcast_data$covar2_hindcast))
      }

      #run hindcast
      hindcast.w_proc <- run_hindcast(model_name = model_name,
                                      params = params.w_proc, #list of params necessary to run that model
                                      Nmc = Nmc,
                                      IC = IC,
                                      wk = wks[k],
                                      covar_hindcast = covar.hindcast.w_proc) #list of settings including N_out, Nmc, and IC

      #write hindcast to file
      if(model_name %in% c("RW","RW_obs")){
        write.csv(hindcast.w_proc,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.P_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)}
      if(model_name %in% c("RW_bias","AC","base_DLM")){
        write.csv(hindcast.w_proc,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.Pa.P_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)}
      if(!model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        write.csv(hindcast.w_proc,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.Pa.D.P_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)}


      ###### observation uncertainty #########

      #retrieve parameters for hindcast
      params.w_obs <- get_params(model_name = model_name,
                                 forecast_type = "IC.P.O", #choose from det, IC, IC.P, IC.P.Pa, IC.P.Pa.D, w_obs
                                 posteriors = out,
                                 num_draws = prow)

      #get hindcasted covariates
      if(model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        covar.hindcast.w_obs <- NA
      } else if (model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag")) {
        covar.hindcast.w_obs <- get_known_covars(model_name = model_name,
                                                    forecast_type = "IC.P.O",
                                                    wk = wks[k],
                                                    known_covars = list(covar = hindcast_data$covar_hindcast))
      } else {
        covar.hindcast.w_obs <- get_known_covars(model_name = model_name,
                                                    forecast_type = "IC.P.O",
                                                    wk = wks[k],
                                                    known_covars = list(covar1 = hindcast_data$covar1_hindcast, covar2 = hindcast_data$covar2_hindcast))
      }

      #run hindcast
      hindcast.w_obs <- run_hindcast(model_name = model_name,
                                     params = params.w_obs, #list of params necessary to run that model
                                     Nmc = Nmc,
                                     IC = IC,
                                     wk = wks[k],
                                     covar_hindcast = covar.hindcast.w_obs) #list of settings including N_out, Nmc, and IC

      #write hindcast to file
      if(model_name %in% c("RW","RW_obs")){
        write.csv(hindcast.w_obs,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.P.O_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)}
      if(model_name %in% c("RW_bias","AC","base_DLM")){
        write.csv(hindcast.w_obs,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.Pa.P.O_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)}
      if(!model_name %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
        write.csv(hindcast.w_obs,file=file.path(paste("./5_Model_output/5.4_Hindcasting_known_covars/",paste0(model_name,'_hindcast.IC.Pa.D.P.O_',yrs[j],'_',wks[k],'.csv'))),row.names = FALSE)}

    }}}

##Hooray! You have hindcasted all the G. echinulata. Treat yourself.
