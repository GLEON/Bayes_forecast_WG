#Forecast plug n play - get parameters for hindcast runs
#Author:Mary Lofton
#Date: 05OCT19

get_params <- function(model_name, forecast_type, posteriors, num_draws, year, covar_ensemble){

  #assign same model name for models with the same structure
  if(model_name == "RW"){model_type = "RW"}
  if(model_name == "RW_obs"){model_type = "RW_obs"}
  if(model_name == "AR"){model_type = "AR"}
  if(model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","schmidt_max_lag","precip")){
    model_type <- "Linear_1var"
  }
  if(model_name %in% c("GDD")){
    model_type <- "Quad_1var"
  }
  if(model_name %in% c("schmidt_and_precip","wnd_and_precip","schmidt_and_wnd")){
    model_type <- "Linear_2var"
  }
  if(model_name %in% c("wnd_and_GDD")){
    model_type <- "Quad_2var"
  }

  ##DETERMINISTIC AND INITIAL CONDITIONS
  if(forecast_type == "det" | forecast_type == "IC"){

    if(model_type == "RW" | model_type == "RW_obs"){
      params <- list(sd_obs = 0, sd_proc = 0)
    }

    if(model_type == "AR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE))
    }

    if(model_type == "Linear_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),
                     sd_C = 0)
    }

    if(model_type == "Quad_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
                     sd_C = 0)
    }

    if(model_type == "Linear_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),
                     beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),sd_C1 = 0, sd_C2 = 0)
    }

    if(model_type == "Quad_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),
                     beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),beta5 = mean(posteriors[,grep("beta5",colnames(posteriors))],na.rm = TRUE),sd_C1 = 0, sd_C2 = 0)
    }

  }

  ##PARAMETER UNCERTAINTY
  if(forecast_type == "IC.Pa"){

    if(model_type == "RW" | model_type == "RW_obs"){
      params <- NULL
      print("This type of uncertainty is invalid for model_type.")
    }

    if(model_type == "AR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"])
    }

    if(model_type == "Linear_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
                     sd_C = 0)
    }

    if(model_type == "Quad_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
                     sd_C = 0)
    }

    if(model_type == "Linear_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
                     sd_C1 = 0, sd_C2 = 0)
    }

    if(model_type == "Quad_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
                     beta5 = posteriors[num_draws,"beta5"],sd_C1 = 0, sd_C2 = 0)
    }

  }

  ##DRIVER UNCERTAINTY
  if(forecast_type == "IC.Pa.D"){

    if(model_type == "RW" | model_type == "RW_obs" | model_type == "AR"){
      params <- NULL
      print("This type of uncertainty is invalid for model_type.")
    }

    if(model_type == "Linear_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Quad_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Linear_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
                     sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C1_proc"]), sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C2_proc"]))
    }

    if(model_type == "Quad_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"], beta5 = posteriors[num_draws,"beta5"],
                     sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C1_proc"]), sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C2_proc"]))
    }

  }

  ##PROCESS UNCERTAINTY
  if(forecast_type == "IC.P" | forecast_type == "IC.Pa.P" | forecast_type == "IC.Pa.D.P"){

    if(model_type == "RW" | model_type == "RW_obs"){
      if(model_type == "RW" | model_type == "RW_obs"){
        params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]))
      }
    }

    if(model_type == "AR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"])
    }

    if(model_type == "Linear_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Quad_1var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Quad_1var_test"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Linear_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
                     sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C1_proc"]), sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C2_proc"]))
    }

    if(model_type == "Quad_2var"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"], beta5 = posteriors[num_draws,"beta5"],
                     sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C1_proc"]), sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C2_proc"]))
    }

  }

  ##OBSERVATION UNCERTAINTY
  if(forecast_type == "IC.P.O" | forecast_type == "IC.Pa.P.O" | forecast_type == "IC.Pa.D.P.O"){

    if(model_type == "RW" | model_type == "RW_obs"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]))
    }

    if(model_type == "AR"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"])
    }

    if(model_type == "Linear_1var"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Quad_1var"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
                     sd_C = 1/sqrt(posteriors[num_draws,"tau_C_proc"]))
    }

    if(model_type == "Linear_2var"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
                     sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C1_proc"]), sd_C2 = 1/sqrt(posteriors[num_draws,"tau_C2_proc"]))
    }

    if(model_type == "Quad_2var"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"], beta5 = posteriors[num_draws,"beta5"],
                     sd_C1 = 1/sqrt(posteriors[num_draws,"tau_C1_proc"]), sd_C2 = 1/sqrt(posteriors[num_draws,"tau_C2_proc"]))
    }

  }

  return(params)

}



