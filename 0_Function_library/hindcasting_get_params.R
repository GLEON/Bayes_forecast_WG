#Forecast plug n play - get parameters for hindcast runs
#Author:Mary Lofton
#Date: 05OCT19

get_params <- function(model_name, forecast_type, posteriors, num_draws){

  ##DETERMINISTIC AND INITIAL CONDITIONS
  if(forecast_type == "det" | forecast_type == "IC"){

    if(model_name == "RW" | model_name == "RW_obs"){
      params <- list(sd_obs = 0, sd_proc = 0)
    }

    if(model_name == "AR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE))
    }

    ###########THESE WILL NEED TO BE EDITED TO ELIMINATE OBS UNCERTAINTY
    # if(model_name == "Seasonal_AR_Mintemp"){
    #   params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),
    #                  sd_T = 0)
    # }
    #
    # if(model_name == "Seasonal_DayLength_Quad"){
    #   params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  sd_D = 0)
    # }
    #
    # if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
    #   params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  sd_W = 0, sd_S = 0)
    # }
    #
    # if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
    #   params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  beta5 = mean(posteriors[,grep("beta5",colnames(posteriors))],na.rm = TRUE), sd_D = 0, sd_T = 0)
    # }
  }

  ##PROCESS UNCERTAINTY
  if(forecast_type == "IC.P"){

    if(model_name == "RW" | model_name == "RW_obs"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]))
    }

    if(model_name == "AR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
                     beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE))
    }

    ###########THESE WILL NEED TO BE EDITED TO ELIMINATE OBS UNCERTAINTY
    # if(model_name == "Seasonal_AR_Mintemp"){
    #   params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),
    #                  sd_T = 0)
    # }
    #
    # if(model_name == "Seasonal_SWradiation_Quad"){
    #   params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  sd_SW = 0)
    # }
    #
    # if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
    #   params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  sd_W = 0, sd_S = 0)
    # }
    #
    # if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
    #   params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  beta5 = mean(posteriors[,grep("beta5",colnames(posteriors))],na.rm = TRUE),sd_D = 0, sd_T = 0)
    # }
  }

  ##PARAMETER UNCERTAINTY
  if(forecast_type == "IC.P.Pa"){

    if(model_name == "RW" | model_name == "RW_obs"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }

    if(model_name == "AR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"])
    }

    ###########THESE WILL NEED TO BE EDITED TO ELIMINATE OBS UNCERTAINTY
    # if(model_name == "Seasonal_AR_Mintemp"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
    #                  sd_T = 0)
    # }
    #
    # if(model_name == "Seasonal_SWradiation_Quad"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"], beta4 = posteriors[num_draws,"beta4"],
    #                  sd_SW = 0)
    # }
    #
    # if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
    #                  sd_W = 0, sd_S = 0)
    # }
    #
    # if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
    #                  beta5 = posteriors[num_draws,"beta5"], sd_D = 0, sd_T = 0)
    # }
  }

  ##DRIVER UNCERTAINTY
  if(forecast_type == "IC.P.Pa.D"){

    if(model_name == "RW" | model_name == "RW_obs" | model_name == "AR"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }

    ###########THESE WILL NEED TO BE EDITED TO ELIMINATE OBS UNCERTAINTY
    # if(model_name == "Seasonal_AR_Mintemp"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],
    #                  sd_T = 1/sqrt(posteriors[num_draws,"tau_T_proc"]))
    # }
    #
    # if(model_name == "Seasonal_SWradiation_Quad"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
    #                  sd_SW = 1/sqrt(posteriors[num_draws,"tau_SW_proc"]))
    # }
    #
    # if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
    #                  sd_W = 1/sqrt(posteriors[num_draws,"tau_W_proc"]), sd_S = 1/sqrt(posteriors[num_draws,"tau_S_proc"]))
    # }
    #
    # if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
    #                  beta2 = posteriors[num_draws,"beta2"], beta3 = posteriors[num_draws,"beta3"],beta4 = posteriors[num_draws,"beta4"],
    #                  beta5 = posteriors[num_draws,"beta5"],sd_D = 1/sqrt(posteriors[num_draws,"tau_D_proc"]), sd_T = 1/sqrt(posteriors[num_draws,"tau_T_proc"]))
    # }
  }

  ##OBSERVATION UNCERTAINTY
  if(forecast_type == "w_obs"){

    if(model_name == "RW" | model_name == "RW_obs"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]))
    }

    if(model_name == "AR"){
      params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = posteriors[num_draws,"beta1"],
                     beta2 = posteriors[num_draws,"beta2"])
    }

    #################THESE WILL NEED TO BE EDITED TO HAVE ALL FORMS OF UNCERTAINTY
    # if(model_name == "Seasonal_AR_Mintemp"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),
    #                  sd_T = 0)
    # }
    #
    # if(model_name == "Seasonal_SWradiation_Quad"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE), beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  sd_SW = 0)
    # }
    #
    # if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  sd_W = 0, sd_S = 0)
    # }
    #
    # if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
    #   params <- list(sd_obs = 1/sqrt(posteriors[num_draws,"tau_obs"]), sd_proc = 1/sqrt(posteriors[num_draws,"tau_proc"]), beta1 = mean(posteriors[,grep("beta1",colnames(posteriors))],na.rm = TRUE),
    #                  beta2 = mean(posteriors[,grep("beta2",colnames(posteriors))],na.rm = TRUE), beta3 = mean(posteriors[,grep("beta3",colnames(posteriors))],na.rm = TRUE),beta4 = mean(posteriors[,grep("beta4",colnames(posteriors))],na.rm = TRUE),
    #                  beta5 = mean(posteriors[,grep("beta5",colnames(posteriors))],na.rm = TRUE),sd_D = 0, sd_T = 0)
    # }
  }

  return(params)

}




