#Forecast plug n play - run hindcasts
#Author: Mary Lofton, JB edits
#Date: 05OCT19, 21July2022

run_hindcast <- function(model_name, params, Nmc, IC, season_weeks){ #, covar_hindcast

  #check that model is set up for hindcasting

  # 1 site models
  if(!model_name %in% c("DLM_1site","wtrtemp_min_and_GDD_1site", "wtrtemp_min_and_GDD_1site_RY")){
    print("This model is not included in the hindcasting functions.")
  }

  #assign same model name for models with the same structure

  if(model_name == "DLM_1site"){model_type = "base_DLM"}

  if(model_name %in% c("wtrtemp_min_and_GDD_1site")){
    model_type <- "Quad_2var"
  }

  if(model_name %in% c("wtrtemp_min_and_GDD_1site_RY")){
    model_type <- "Quad_2var_RY"
  }


  #set up output matrices
  proc.model <- matrix(NA, Nmc, 4)
  out <- matrix(NA, Nmc, 4)

  # ts = rbind(1:20,21:40)
  # week_avg = week_avg
  # week_min = week_min
  # week_max = week_max
  # week_num = week_num
  # obs_data = obs_data
  # colnums = colnums


  if(model_type == "base_DLM"){
    if(season_weeks %in% c(1:137)){
      for(t in 1:4){
        #set initial conditions
        if(t == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[t] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[t] = rnorm(Nmc,proc.model[t],params$sd_obs)
        #update IC
        gloeo_prev <- out[t]
      }}

    if(season_weeks == 138){
      for(t in 1:3){
        #set initial conditions
        if(t == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[t] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[t] = rnorm(Nmc,proc.model[t],params$sd_obs)
        #update IC
        gloeo_prev <- out[t]
      }}

    if(season_weeks == 139){
      for(t in 1:2){
        #set initial conditions
        if(t == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[t] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[t] = rnorm(Nmc,proc.model[t],params$sd_obs)
        #update IC
        gloeo_prev <- out[t]
      }}

    if(season_weeks == 140){
      #set initial conditions
      gloeo_prev <- IC
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev
      proc.model[1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[1] = rnorm(Nmc,proc.model[1],params$sd_obs)
    }
  }



  if(model_type == "Quad_2var_RY"){

    if(season_weeks %in% c(1:137)){
      for(t in 1:4){
        #set initial conditions
        if(t == 1){gloeo_prev <- IC}
        #covar model
        covar1 <- gloeo$Temp2[season_weeks]
        covar2 <- gloeo$GDD2[season_weeks]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2 + params$beta5*covar2^2
        proc.model[t] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[t] = rnorm(Nmc,proc.model[t],params$sd_obs)
        #update IC
        gloeo_prev <- out[t]
      }}

    if(season_weeks ==138){
      for(t in 1:3){
        #set initial conditions
        if(t == 1){gloeo_prev <- IC}
        #covar model
        covar1 <- gloeo$Temp2[season_weeks]
        covar2 <- gloeo$GDD2[season_weeks]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2 + params$beta5*covar2^2
        proc.model[t] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[t] = rnorm(Nmc,proc.model[t],params$sd_obs)
        #update IC
        gloeo_prev <- out[t]
      }}

    if(season_weeks ==139){
      for(t in 1:2){
        #set initial conditions
        if(t == 1){gloeo_prev <- IC}
        #covar model
        covar1 <- gloeo$Temp2[season_weeks]
        covar2 <- gloeo$GDD2[season_weeks]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2 + params$beta5*covar2^2
        proc.model[t] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[t] = rnorm(Nmc,proc.model[t],params$sd_obs)
        #update IC
        gloeo_prev <- out[t]
      }}

    if(season_weeks ==140){
      #set initial conditions
      gloeo_prev <- IC
      #covar model
      covar1 <- gloeo$Temp2[season_weeks]
      covar2 <- gloeo$GDD2[season_weeks]
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2 + params$beta5*covar2^2
      proc.model[1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[1] = rnorm(Nmc,proc.model[1],params$sd_obs)
    }
  }

  return(out)
}
