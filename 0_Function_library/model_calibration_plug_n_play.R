# Title: Plug and play scripts to set conditions for model calibration runs
# History:
# created JAZ 2019-02-15
# WB Updates
# MEL updates for seasonal for-loop 30JUL19
# MEL updates for final publication 27MAR20

jags_plug_ins <- function(model_name){

#JAGS Plug-ins: Add each separate model here
#variable.names are variables you would like to plot for model convergence (e.g., excludes mu)
#variable.names.out are all variables you would like to monitor in the jags run
#init are a range of initial conditions for parameters in each of 3 chains

#assign same model name for models with the same structure
  if(model_name == "RW"){model_type = "RW"}
  if(model_name == "RW_obs"){model_type = "RW_obs"}
  if(model_name == "AR"){model_type = "AR"}
  if(model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff")){
    model_type <- "Linear_1var"
  }
  if(model_name %in% c("GDD")){
    model_type <- "Quad_1var"
  }
  if(model_name %in% c("GDD_test")){
    model_type <- "Quad_1var_test"
  }

#RW
  data.RW <- list(y=cal_data$y, year_no = cal_data$year_no,season_weeks=cal_data$season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.001, r_obs = 0.001)
  variable.names.RW <- c("tau_proc", "tau_obs")
  variable.namesout.RW <- c("tau_proc","tau_obs","mu")
  init.RW <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.RW <- c("tau_proc","tau_obs")

#RW_obs
  data.RW_obs <- list(y=cal_data$y, year_no = cal_data$year_no,season_weeks=cal_data$season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.RW_obs <- c("tau_proc", "tau_obs")
  variable.namesout.RW_obs <- c("tau_proc","tau_obs","mu")
  init.RW_obs <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.RW_obs <- c("tau_proc","tau_obs")

#AR
  data.AR <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks, beta.m1=0,  beta.m2=0,beta.v1=0.001, beta.v2=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.AR <- c("tau_proc", "beta1","beta2", "tau_obs")
  variable.namesout.AR <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs")
  init.AR <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5, beta1=0.5,beta2=0.5))
  params.AR <- c("tau_proc","beta1", "beta2",  "tau_obs")

#Linear_1var
  data.Linear_1var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar=cal_data$covar, week_avg=cal_data$week_avg, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Linear_1var <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_C_proc")
  variable.namesout.Linear_1var <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_C_proc")
  init.Linear_1var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_C_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Linear_1var <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_C_proc")

#Quad_1var
  data.Quad_1var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar=cal_data$covar, week_avg=cal_data$week_avg, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Quad_1var <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_C_proc")
  variable.namesout.Quad_1var <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_C_proc")
  init.Quad_1var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_C_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
  params.Quad_1var <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_C_proc")

#Quad_1var_test
  data.Quad_1var_test <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,covar=cal_data$covar, week_avg=cal_data$week_avg, beta.m1=0,  beta.m2=0,beta.m3=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Quad_1var_test <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_C_proc")
  variable.namesout.Quad_1var_test <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_C_proc")
  init.Quad_1var_test <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_C_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_C_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_C_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Quad_1var_test <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_C_proc")

# #Linear_2var
#   data.Linear_2var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,Temp=cal_data$Temp, Schmidt=cal_data$Schmidt, week_avg_T=cal_data$week_avg_T,week_avg_S=cal_data$week_avg_S, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001, x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Linear_2var <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_T_proc", "tau_S_proc")
#   variable.namesout.Linear_2var <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_T_proc", "tau_S_proc")
#   init.Linear_2var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01,tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.Linear_2var <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_T_proc", "tau_S_proc")
#
# #Quad_2var
#   data.Quad_2var <- list(y=cal_data$y, year_no = cal_data$year_no, season_weeks=cal_data$season_weeks,Temp=cal_data$Temp, Schmidt=cal_data$Schmidt, week_avg_T=cal_data$week_avg_T,week_avg_S=cal_data$week_avg_S, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Quad_2var <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_T_proc", "tau_S_proc")
#   variable.namesout.Quad_2var <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_T_proc", "tau_S_proc")
#   init.Quad_2var <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01,tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))
#   params.Quad_2var <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_T_proc", "tau_S_proc")

  data = eval(parse(text = paste0('data.', model_type)))
  variable.names = eval(parse(text = paste0('variable.names.', model_type)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_type)))
  init = eval(parse(text = paste0('init.', model_type)))
  params = eval(parse(text = paste0('params.', model_type)))

  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params))
}





