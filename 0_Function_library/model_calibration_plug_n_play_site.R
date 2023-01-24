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
  if(model_name == "DLM_1site"){model_type = "DLM_1site"}

  if(model_name %in% c("wtrtemp_min_and_GDD_1site")){
    model_type <- "Quad_2var"
  }

  if(model_name %in% c("wtrtemp_min_and_GDD_1site_RY")){
    model_type <- "Quad_2var_RY"
  }

  if(model_name %in% c("wtrtemp_min_and_airtempGDD_1site_HC", "wtrtemp_min_and_airtempGDD_1site_NB", "wtrtemp_min_and_airtempGDD_1site_NSH")){
    model_type <- "Quad_2var_airtemp"
  }

  if(model_name %in% c("wtrtemp_min_and_airtempGDD_1site_RY_HC", "wtrtemp_min_and_airtempGDD_1site_RY_NB", "wtrtemp_min_and_airtempGDD_1site_RY_NSH")){
    model_type <- "Quad_2var_RY_airtemp"
  }

  if(model_name == "RW_obs_3sites"){model_type = "RW_obs_3sites"}
  if(model_name == "DLM_3sites"){model_type = "DLM_3sites"}

  if(model_name %in% c("wtrtemp_min_and_GDD_3sites")){
    model_type <- "Quad_2var_multisite"
  }

  if(model_name %in% c("wtrtemp_min_and_GDD_3sites_RY")){
    model_type <- "Quad_2var_multisite_RY"
  }


#### 1 site Models

# RW_obs_1site
  data.RW_obs_1site <- list(y=cal_data$y, season_weeks=cal_data$season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)
  variable.names.RW_obs_1site <- c("tau_proc", "tau_obs")
  variable.namesout.RW_obs_1site <- c("tau_proc","tau_obs","mu")
  init.RW_obs_1site <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.RW_obs_1site <- c("tau_proc","tau_obs")

# DLM_1site
  data.DLM_1site <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, beta.m1=0,  beta.m2=0,beta.v1=0.001, beta.v2=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)
  variable.names.DLM_1site <- c("tau_proc", "beta1","beta2", "tau_obs")
  variable.namesout.DLM_1site <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs")
  init.DLM_1site <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5, beta1=0.5,beta2=0.5))
  params.DLM_1site <- c("tau_proc","beta1", "beta2",  "tau_obs")


### Quad_2var - removed #year_no = cal_data$year_no, replaced with site number
  data.Quad_2var <- list(y=cal_data$y, season_weeks=cal_data$season_weeks,covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)
  variable.names.Quad_2var <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")
  variable.namesout.Quad_2var <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

    init.Quad_2var <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
    list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
    list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc")

### Quad_2var_RY - Quad 2 variable with Random Year effect

  data.Quad_2var_RY <- list(y=cal_data$y, season_weeks = cal_data$season_weeks, year_no = cal_data$year_no, totYr = cal_data$totYr, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)

  variable.names.Quad_2var_RY <- c("tau_yr","yr", "tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")

  variable.namesout.Quad_2var_RY <- c("tau_yr","yr", "tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

  init.Quad_2var_RY <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
                list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
                list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var_RY <- c("tau_yr", "tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc")

  ### Quad_2var_airtemp - Quad 2 variable - air temp NO random year

  data.Quad_2var_airtemp <- list(y=cal_data$y, season_weeks = cal_data$season_weeks, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)

  variable.names.Quad_2var_airtemp <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc")

  variable.namesout.Quad_2var_airtemp <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc")

  init.Quad_2var_airtemp <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
                                    list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
                                    list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var_airtemp <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc")


### Quad_2var_RY_airtemp - Quad 2 variable with Random Year effect & air temp

  data.Quad_2var_RY_airtemp <- list(y=cal_data$y, season_weeks = cal_data$season_weeks, year_no = cal_data$year_no, totYr = cal_data$totYr, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)

  variable.names.Quad_2var_RY_airtemp <- c("tau_yr","yr", "tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc")

  variable.namesout.Quad_2var_RY_airtemp <- c("tau_yr","yr", "tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc")

  init.Quad_2var_RY_airtemp <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
                            list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
                            list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var_RY_airtemp <- c("tau_yr", "tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc")

# 3 site models ####

# RW_obs_3sites
  data.RW_obs_3sites <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)
  variable.names.RW_obs_3sites <- c("tau_proc", "tau_obs")
  variable.namesout.RW_obs_3sites <- c("tau_proc","tau_obs","mu")
  init.RW_obs_3sites <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.RW_obs_3sites <- c("tau_proc","tau_obs")

  # DLM_3sites
  data.DLM_3sites <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, beta.m1=0,  beta.m2=0,beta.v1=0.001, beta.v2=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)
  variable.names.DLM_3sites <- c("tau_proc", "beta1","beta2", "tau_obs")
  variable.namesout.DLM_3sites <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs")
  init.DLM_3sites <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5, beta1=0.5,beta2=0.5))
  params.DLM_3sites <- c("tau_proc","beta1", "beta2",  "tau_obs")


  ### Quad_2var multi site
  data.Quad_2var_multisite <- list(y=cal_data$y, season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)

  variable.names.Quad_2var_multisite  <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_C1_proc", "tau_C2_proc")
  variable.namesout.Quad_2var_multisite  <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_C1_proc", "tau_C2_proc")

  init.Quad_2var_multisite <- list(list(tau_proc=0.001, tau_obs = 0.1,tau_C1_proc = 0.01,tau_C2_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5),
                list(tau_proc=0.1,tau_obs = 1,tau_C1_proc = 0.1,tau_C2_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0),
                list(tau_proc=1, tau_obs = 5,tau_C1_proc = 1,tau_C2_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))

  params.Quad_2var_multisite <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_C1_proc", "tau_C2_proc")


  ### Quad_2var multi site & RY
  data.Quad_2var_multisite_RY <- list(y=cal_data$y,season_weeks=cal_data$season_weeks, site_no = cal_data$site_no, year_no = cal_data$year_no, totYr = cal_data$totYr, covar1=cal_data$covar1, covar2=cal_data$covar2, week_avg1=cal_data$week_avg1,week_avg2=cal_data$week_avg2, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.555, r_obs = 0.349)

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

