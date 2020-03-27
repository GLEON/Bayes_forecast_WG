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

# #AR_mintemp
#   data.AR_mintemp <- list(y=y, year_no = year_no,week_min = week_min, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.AR_mintemp <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_T_proc")
#   variable.namesout.AR_mintemp <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_T_proc")
#   init.AR_mintemp <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.AR_mintemp <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_T_proc")
#
# #AR_mintemplag
#   data.AR_mintemplag <- list(y=y, year_no = year_no,week_min = week_min, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.AR_mintemplag <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_T_proc")
#   variable.namesout.AR_mintemplag <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_T_proc")
#   init.AR_mintemplag <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.AR_mintemplag <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_T_proc")
#
# #AR_maxSchmidtlag
#   data.AR_maxSchmidtlag <- list(y=y, year_no = year_no,week_max = week_max, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Schmidt=Schmidt, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.AR_maxSchmidtlag <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_S_proc")
#   variable.namesout.AR_maxSchmidtlag <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_S_proc")
#   init.AR_maxSchmidtlag <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.AR_maxSchmidtlag <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_S_proc")
#
# #AR_minSchmidtdiff
#   data.AR_minSchmidtdiff <- list(y=y, year_no = year_no,week_min = week_min, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Schmidt=Schmidt, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.AR_minSchmidtdiff <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_S_proc")
#   variable.namesout.AR_minSchmidtdiff <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_S_proc")
#   init.AR_minSchmidtdiff <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.AR_minSchmidtdiff <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_S_proc")
#
# #AR_minwind
#   data.AR_minwind <- list(y=y, year_no = year_no,week_min = week_min, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Wnd=Wnd, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.AR_minwind <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_W_proc")
#   variable.namesout.AR_minwind <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_W_proc")
#   init.AR_minwind <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_W_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_W_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_W_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
#   params.AR_minwind <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_W_proc")
#
# #Quad_GDD
#   data.Quad_GDD <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,GDD=GDD, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Quad_GDD <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_G_proc")
#   variable.namesout.Quad_GDD <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_G_proc")
#   init.Quad_GDD <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_G_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_G_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_G_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.Quad_GDD <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_G_proc")
#
# #Quad_SWradiation
#   data.Quad_SWradiation <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,SW=SW, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.Quad_SWradiation <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_SW_proc")
#   variable.namesout.Quad_SWradiation <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_SW_proc")
#   init.Quad_SWradiation <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_SW_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_SW_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_SW_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.Quad_SWradiation <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_SW_proc")
#
# #MinSchmidtdiff_mintemp
#   data.MinSchmidtdiff_mintemp <- list(y=y, year_no = year_no,week_min_T = week_min_T, week_min_S = week_min_S, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001, Temp=Temp, Schmidt=Schmidt,season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.MinSchmidtdiff_mintemp <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_T_proc", "tau_S_proc")
#   variable.namesout.MinSchmidtdiff_mintemp <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_T_proc", "tau_S_proc")
#   init.MinSchmidtdiff_mintemp <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01,tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.MinSchmidtdiff_mintemp <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_T_proc", "tau_S_proc")
#
# #MinSchmidtdiff_minwind
#   data.MinSchmidtdiff_minwind <- list(y=y, year_no = year_no,week_min_W = week_min_W, week_min_S = week_min_S, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001, Wnd = Wnd, Schmidt=Schmidt,season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.MinSchmidtdiff_minwind <- c("tau_proc", "beta1","beta2", "beta3","beta4", "tau_obs","tau_W_proc", "tau_S_proc")
#   variable.namesout.MinSchmidtdiff_minwind <- c("tau_proc", "beta1", "beta2","beta3","beta4",  "mu", "tau_obs", "tau_W_proc", "tau_S_proc")
#   init.MinSchmidtdiff_minwind <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_W_proc = 0.01,tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_W_proc = 0.1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_obs = 5,tau_W_proc = 1,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5))
#   params.MinSchmidtdiff_minwind <- c("tau_proc","beta1", "beta2", "beta3","beta4","tau_obs","tau_W_proc", "tau_S_proc")
#
# #MinSchmidtdiff_SWradiation
#   data.MinSchmidtdiff_SWradiation <- list(y=y, year_no = year_no,week_avg = week_avg, week_min_S = week_min_S, beta.m1=0,  beta.m2=0,beta.m3=0,beta.m4=0,beta.m5=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001,beta.v4=0.001,beta.v5=0.001, SW=SW, Schmidt=Schmidt,season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
#   variable.names.MinSchmidtdiff_SWradiation <- c("tau_proc", "beta1","beta2", "beta3","beta4","beta5", "tau_obs","tau_SW_proc", "tau_S_proc")
#   variable.namesout.MinSchmidtdiff_SWradiation <- c("tau_proc", "beta1", "beta2","beta3","beta4","beta5",  "mu", "tau_obs", "tau_SW_proc", "tau_S_proc")
#   init.MinSchmidtdiff_SWradiation <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_SW_proc = 0.01,tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5, beta5=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_SW_proc = 0.1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0, beta4=0, beta5=0), list(tau_proc=1, tau_obs = 5,tau_SW_proc = 1,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5, beta4=0.5, beta5=0.5))
#   params.MinSchmidtdiff_SWradiation <- c("tau_proc","beta1", "beta2", "beta3","beta4","beta5","tau_obs","tau_SW_proc", "tau_S_proc")

  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  init = eval(parse(text = paste0('init.', model_name)))
  params = eval(parse(text = paste0('params.', model_name)))

  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params))
}





