#Forecast plug n play
#Author:Mary Lofton
#Date: 05OCT19

get_params <- function(model_name, forecast_type, posteriors, num_draws){
  
  ##DETERMINISTIC AND INITIAL CONDITIONS 
  if(forecast_type == "det" | forecast_type == "IC"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
      params <- list(sd_obs = 0, sd_proc = 0)
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE))
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE), sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE), sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_W = 0)
    }
  }
  
  ##PROCESS UNCERTAINTY 
  if(forecast_type == "IC.P"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]))
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), sd_yr = 0)
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE))
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_W = 0)
    }
  }
  
  ##OBSERVATION UNCERTAINTY 
  if(forecast_type == "IC.P.O"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]))
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), sd_yr = 0)
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE))
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE), beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_W = 0)
    }
  }
  
  ##RANDOM EFFECTS UNCERTAINTY 
  if(forecast_type == "IC.P.O.R"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name == "Seasonal_AR" | model_name == "Seasonal_AR_Temperature"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), sd_yr = 1/sqrt(out[prow,"tau_yr"]))
    }
  }
  
  ##PARAMETER UNCERTAINTY 
  if(forecast_type == "IC.P.O.Pa"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"])
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"], sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"], sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_S = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_W = 0)
    }
  }
  
  ##DRIVER UNCERTAINTY 
  if(forecast_type == "IC.P.O.Pa.D"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name == "Seasonal_RandomWalk_RandomYear" | model_name == "Seasonal_AR"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_SW = 1/sqrt(out[prow,"tau_SW_proc"]))
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_D = 1/sqrt(out[prow,"tau_D_proc"]))
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"],sd_D = 1/sqrt(out[prow,"tau_D_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"],sd_SW = 1/sqrt(out[prow,"tau_SW_proc"]), sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_W = 1/sqrt(out[prow,"tau_W_proc"]), sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_L = 1/sqrt(out[prow,"tau_L_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_W = 1/sqrt(out[prow,"tau_W_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_P = 1/sqrt(out[prow,"tau_P_proc"]))
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_L = 1/sqrt(out[prow,"tau_L_proc"]))
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_L = 1/sqrt(out[prow,"tau_L_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_W = 1/sqrt(out[prow,"tau_W_proc"]))
    }
  }
  
  
  return(params)
  
}

