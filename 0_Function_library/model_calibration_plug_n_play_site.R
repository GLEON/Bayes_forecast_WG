# Title: Plug and play scripts to set conditions for model calibration runs
# History:
# created JAZ 2019-02-15
# WB Updates
# MEL updates for seasonal for-loop 30JUL2019
# MEL updates for final publication 27MAR2020
# JAB updates for multi-site models 9May2022

jags_plug_ins <- function(model_name){

#JAGS Plug-ins: Add each separate model here
#variable.names are variables you would like to plot for model convergence (e.g., excludes mu)
#variable.names.out are all variables you would like to monitor in the jags run
#init are a range of initial conditions for parameters in each of 3 chains

#assign same model name for models with the same structure
  if(model_name == "RW_obs_1site"){model_type = "RW_obs_1site"}
  if(model_name == "RW_bias_1site"){model_type = "RW_bias_1site"}
  if(model_name == "AC_1site"){model_type = "AC_1site"}
  if(model_name == "base_DLM"){model_type = "base_DLM"}

  if(model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","schmidt_max_lag","precip")){
    model_type <- "Linear_1var"
  }
  if(model_name %in% c("GDD")){
    model_type <- "Quad_1var"
  }
  if(model_name %in% c("GDD_test")){
    model_type <- "Quad_1var_test"
  }
  if(model_name %in% c("schmidt_and_wind","temp_and_wind","schmidt_and_temp")){
    model_type <- "Linear_2var"
  }
  if(model_name %in% c("wtrtemp_min_and_GDD")){
    model_type <- "Quad_2var"
  }

  if(model_name %in% c("wtrtemp_min_and_GDD_RY")){
    model_type <- "Quad_2var_RY"
  }

  if(model_name == "RW_obs_3sites"){model_type = "RW_obs_3sites"}
  if(model_name == "RW_bias_3sites"){model_type = "RW_bias_3sites"}
  if(model_name == "AC_3sites"){model_type = "AC_3sites"}

  if(model_name %in% c("wtrtemp_min_and_GDD_3sites")){
    model_type <- "Quad_2var_multisite"
  }

  if(model_name %in% c("wtrtemp_min_and_GDD_3sites_RY")){
    model_type <- "Quad_2var_multisite_RY"
  }


#### 1 site Models

# #RW
#   data.RW <- list(y=cal_data$y, year_no = cal_data$year_no,season_weeks=cal_data$season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.001, r_obs = 0.001)
#   variable.names.RW <- c("tau_proc", "tau_obs")
#   variable.namesout.RW <- c("tau_proc","tau_obs","mu")
#   init.RW <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
#   params.RW <- c("tau_proc","tau_obs")

#
#RW_obs_1site
  data.RW_obs_1site <- list(y=cal_data$y, season_weeks=cal_data$season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.RW_obs_1site <- c("tau_proc", "tau_obs")
  variable.namesout.RW_obs_1site <- c("tau_proc","tau_obs","mu")
  init.RW_obs_1site <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.RW_obs_1site <- c("tau_proc","tau_obs")

# #RY
#   data.RY <- list(y=cal_data$y, year_no = cal_data$year_no,season_weeks=cal_data$season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.RY <- c("tau_proc", "tau_obs","tau_yr")
#   variable.namesout.RY <- c("tau_proc","tau_obs","mu","tau_yr","yr")
#   init.RY <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_yr = 0.001), list(tau_proc=0.1, tau_obs = 1, tau_yr = 0.1), list(tau_proc=1, tau_obs = 5, tau_yr = 1))
#   params.RY <- c("tau_proc","tau_obs","tau_yr")
#
#RW_bias_1site
  data.RW_bias_1site <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, beta.m1=0,  beta.v1=0.001, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.RW_bias_1site <- c("tau_proc", "beta1", "tau_obs")
  variable.namesout.RW_bias_1site <- c("tau_proc", "beta1",   "mu", "tau_obs")
  init.RW_bias_1site <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0), list(tau_proc=1, tau_obs = 5, beta1=0.5))
  params.RW_bias_1site <- c("tau_proc","beta1",  "tau_obs")

#AC_1site
  data.AC_1site <- list(y=cal_data$y, season_weeks=cal_data$season_weeks,  beta.m2=0, beta.v2=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.AC_1site <- c("tau_proc", "beta2", "tau_obs")
  variable.namesout.AC_1site <- c("tau_proc",  "beta2",  "mu", "tau_obs")
  init.AC_1site <- list(list(tau_proc=0.001, tau_obs = 0.1,   beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1,  beta2=0), list(tau_proc=1, tau_obs = 5, beta2=0.5))
  params.AC_1site <- c("tau_proc", "beta2",  "tau_obs")

# #base_DLM
#   data.base_DLM <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks, beta.m1=0,  beta.m2=0,beta.v1=0.001, beta.v2=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.base_DLM <- c("tau_proc", "beta1","beta2", "tau_obs")
#   variable.namesout.base_DLM <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs")
#   init.base_DLM <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5, beta1=0.5,beta2=0.5))
#   params.base_DLM <- c("tau_proc","beta1", "beta2",  "tau_obs")
#
# #Linear_1var
#   data.Linear_1var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar=cal_data$covar, week_avg=cal_data$week_avg, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Linear_1var <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_C_proc")
#   variable.namesout.Linear_1var <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_C_proc","covar")
#   init.Linear_1var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_C_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.Linear_1var <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_C_proc")
#
# #Quad_1var
#   data.Quad_1var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar=cal_data$covar, week_avg=cal_data$week_avg, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Quad_1var <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_C_proc")
#   variable.namesout.Quad_1var <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_C_proc")
#   init.Quad_1var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_C_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.Quad_1var <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_C_proc")
#
# #Quad_1var_test
#   data.Quad_1var_test <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar=cal_data$covar, week_avg=cal_data$week_avg, beta.m1=0,  beta.m2=0,beta.m3=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Quad_1var_test <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_C_proc")
#   variable.namesout.Quad_1var_test <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_C_proc")
#   init.Quad_1var_test <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_C_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.Quad_1var_test <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_C_proc")
#
# #Linear_2var
#   data.Linear_2var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Linear_2var <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_C1_proc", "tau_C2_proc")
#   variable.namesout.Linear_2var <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")
#   init.Linear_2var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.Linear_2var <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_C1_proc", "tau_C2_proc")

### Quad_2var - removed #year_no = cal_data$year_no, replaced with site number
  data.Quad_2var <- list(y=cal_data$y, season_weeks=cal_data$season_weeks,covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Quad_2var <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")
  variable.namesout.Quad_2var <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

    init.Quad_2var <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
    list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
    list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc")

  # data = eval(parse(text = paste0('data.', model_type)))
  # variable.names = eval(parse(text = paste0('variable.names.', model_type)))
  # variable.namesout = eval(parse(text = paste0('variable.namesout.', model_type)))
  # init = eval(parse(text = paste0('init.', model_type)))
  # params = eval(parse(text = paste0('params.', model_type)))
  #
  # return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params))

### Quad_2var_RY - Quad 2 variable with Random Year effect

  data.Quad_2var_RY <- list(y=cal_data$y,year_no = cal_data$year_no, totYr = cal_data$totYr, season_weeks = cal_data$season_weeks,covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)

  variable.names.Quad_2var_RY <- c("tau_yr","yr", "tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")

  variable.namesout.Quad_2var_RY <- c("tau_yr","yr", "tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

  init.Quad_2var_RY <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
                list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
                list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var_RY <- c("tau_yr", "tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc")


#### 3 site models

  #RW_obs_3sites
  data.RW_obs_3sites <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.RW_obs_3sites <- c("tau_proc", "tau_obs")
  variable.namesout.RW_obs_3sites <- c("tau_proc","tau_obs","mu")
  init.RW_obs_3sites <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.RW_obs_3sites <- c("tau_proc","tau_obs")


  #RW_bias_3sites
  data.RW_bias_3sites <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, beta.m1=0,  beta.v1=0.001, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.RW_bias_3sites <- c("tau_proc", "beta1", "tau_obs")
  variable.namesout.RW_bias_3sites <- c("tau_proc", "beta1",   "mu", "tau_obs")
  init.RW_bias_3sites <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0), list(tau_proc=1, tau_obs = 5, beta1=0.5))
  params.RW_bias_3sites <- c("tau_proc","beta1",  "tau_obs")

  #AC_3sites
  data.AC_3sites <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, beta.m2=0, beta.v2=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.AC_3sites <- c("tau_proc", "beta2", "tau_obs")
  variable.namesout.AC_3sites <- c("tau_proc",  "beta2",  "mu", "tau_obs")
  init.AC_3sites <- list(list(tau_proc=0.001, tau_obs = 0.1,   beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1,  beta2=0), list(tau_proc=1, tau_obs = 5, beta2=0.5))
  params.AC_3sites <- c("tau_proc", "beta2",  "tau_obs")

  ### Quad_2var multi site
  data.Quad_2var_multisite <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)

  variable.names.Quad_2var_multisite  <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")
  variable.namesout.Quad_2var_multisite  <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

  init.Quad_2var_multisite <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
                list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
                list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var_multisite <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc")


  ### Quad_2var multi site & RY
  data.Quad_2var_multisite_RY <- list(y=cal_data$y,site_no = cal_data$site_no, season_weeks=cal_data$season_weeks,year_no = cal_data$year_no, totYr = cal_data$totYr, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)

  variable.names.Quad_2var_multisite_RY <- c("tau_yr", "yr", "tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")

  variable.namesout.Quad_2var_multisite_RY <- c("tau_yr", "yr", "tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

  init.Quad_2var_multisite_RY <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01,
                                           beta1=-2, beta2=-0.5, beta3=0, beta4=0, beta5=-0.5),
                  list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1,
                       beta1=-1.5, beta2=0, beta3=0.5, beta4=0.5, beta5=0),
                  list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1,
                       beta1=-1,beta2=0.5, beta3=1, beta4=1, beta5=0.5))

#beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5), - old betas

  params.Quad_2var_multisite_RY <- c("tau_yr", "tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc") #"yr",

  data = eval(parse(text = paste0('data.', model_type)))
  variable.names = eval(parse(text = paste0('variable.names.', model_type)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_type)))
  init = eval(parse(text = paste0('init.', model_type)))
  params = eval(parse(text = paste0('params.', model_type)))



  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params))
}





