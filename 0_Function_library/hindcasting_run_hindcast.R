#Forecast plug n play - run hindcasts
#Author:Mary Lofton
#Date: 05OCT19

run_hindcast <- function(model_name, params, Nmc, IC, wk, covar_hindcast){

  #check that model is set up for hindcasting
  if(!model_name %in% c("RW","RW_obs","AR","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","GDD_test","schmidt_diff_and_max","wnd_dir_and_speed","schmidt_and_wnd")){
    print("This model is not included in the hindcasting functions.")
  }

  #assign same model name for models with the same structure
  if(model_name == "RW"){model_type = "RW"}
  if(model_name == "RW_obs"){model_type = "RW_obs"}
  if(model_name == "AR"){model_type = "AR"}
  if(model_name %in% c("wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag")){
    model_type <- "Linear_1var"
  }
  if(model_name %in% c("GDD")){
    model_type <- "Quad_1var"
  }
  if(model_name %in% c("GDD_test")){
    model_type <- "Quad_1var_test"
  }
  if(model_name %in% c("schmidt_diff_and_max","wnd_dir_and_speed","schmidt_and_wnd")){
    model_type <- "Linear_2var"
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

  if(model_type == "RW"){
    if(wk %in% c(1:17)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 18){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 19){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 20){
      #set initial conditions
      gloeo_prev <- IC
      #process model
      proc.model[,1] = rnorm(Nmc,gloeo_prev,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }

  if(model_type == "RW_obs"){
    if(wk %in% c(1:17)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 18){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 19){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 20){
      #set initial conditions
      gloeo_prev <- IC
      #process model
      proc.model[,1] = rnorm(Nmc,gloeo_prev,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }

  if(model_type == "AR"){
    if(wk %in% c(1:17)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 18){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 19){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk == 20){
      #set initial conditions
      gloeo_prev <- IC
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }

  if(model_type == "Linear_1var"){

    if(wk %in% c(1:17)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==18){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==19){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==20){
      #set initial conditions
      gloeo_prev <- IC
      #covar model
      covar <- covar_hindcast$covar[,1]
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }

  if(model_type == "Quad_1var"){

    if(wk %in% c(1:17)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar + params$beta4*covar^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==18){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar + params$beta4*covar^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==19){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar + params$beta4*covar^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==20){
      #set initial conditions
      gloeo_prev <- IC
      #covar model
      covar <- covar_hindcast$covar[,1]
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar + params$beta4*covar^2
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }

  if(model_type == "Quad_1var_test"){

    if(wk %in% c(1:17)){
      for(j in 1:4){
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*covar + params$beta3*covar^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
      }}

    if(wk ==18){
      for(j in 1:3){
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*covar + params$beta3*covar^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
      }}

    if(wk ==19){
      for(j in 1:2){
        #covar model
        covar <- covar_hindcast$covar[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*covar + params$beta3*covar^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
      }}

    if(wk ==20){
      #covar model
      covar <- covar_hindcast$covar[,1]
      #process model
      gloeo_temp = params$beta1 + params$beta2*covar + params$beta3*covar^2
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }

  if(model_type == "Linear_2var"){

    if(wk %in% c(1:17)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar1 <- covar_hindcast$covar1[,j]
        covar2 <- covar_hindcast$covar2[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==18){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar1 <- covar_hindcast$covar1[,j]
        covar2 <- covar_hindcast$covar2[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==19){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #covar model
        covar1 <- covar_hindcast$covar1[,j]
        covar2 <- covar_hindcast$covar2[,j]
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}

    if(wk ==20){
      #set initial conditions
      gloeo_prev <- IC
      #covar model
      covar1 <- covar_hindcast$covar1[,1]
      covar2 <- covar_hindcast$covar2[,1]
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*covar1 + params$beta4*covar2
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }


  # if(model_type == "Seasonal_DayLength_Quad"){
  #   if(week_num %in% c(1:16,21:36)){
  #     for(j in 1:5){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(17,37)){
  #     for(j in 1:4){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(18,38)){
  #     for(j in 1:3){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(19,39)){
  #     for(j in 1:2){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(20,40)){
  #     #set initial conditions
  #     gloeo_prev <- IC
  #     #temp model
  #     DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #     #process model
  #     gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
  #     proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #     #data model
  #     out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
  #   }
  # }
  #
  # if(model_type == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
  #   if(week_num %in% c(1:16,21:36)){
  #     for(j in 1:5){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       if(j == 1){Schmidt_prev <- IC_S}
  #       #temp model
  #       Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
  #       Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #       Schmidt_prev <- Schmidt
  #     }}
  #
  #   if(week_num %in% c(17,37)){
  #     for(j in 1:4){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       if(j == 1){Schmidt_prev <- IC_S}
  #       #temp model
  #       Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
  #       Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #       Schmidt_prev <- Schmidt
  #     }}
  #
  #   if(week_num %in% c(18,38)){
  #     for(j in 1:3){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       if(j == 1){Schmidt_prev <- IC_S}
  #       #temp model
  #       Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
  #       Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #       Schmidt_prev <- Schmidt
  #     }}
  #
  #   if(week_num %in% c(19,39)){
  #     for(j in 1:2){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       if(j == 1){Schmidt_prev <- IC_S}
  #       #temp model
  #       Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
  #       Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #       Schmidt_prev <- Schmidt
  #     }}
  #
  #   if(week_num %in% c(20,40)){
  #     #set initial conditions
  #     #set initial conditions
  #     gloeo_prev <- IC
  #     Schmidt_prev <- IC_S
  #     #temp model
  #     Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
  #     Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
  #     #process model
  #     gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
  #     proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #     #data model
  #     out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
  #   }
  # }
  #
  # if(model_type == "Seasonal_DayLength_Quad_Mintemp"){
  #   if(week_num %in% c(1:16,21:36)){
  #     for(j in 1:5){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #temp model
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(17,37)){
  #     for(j in 1:4){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #temp model
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(18,38)){
  #     for(j in 1:3){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #temp model
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(19,39)){
  #     for(j in 1:2){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #       #temp model
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
  #       proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #       #data model
  #       out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
  #       #update IC
  #       gloeo_prev <- out[,j]
  #     }}
  #
  #   if(week_num %in% c(20,40)){
  #     #set initial conditions
  #     gloeo_prev <- IC
  #     #temp model
  #     DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
  #     #temp model
  #     Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #     #process model
  #     gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
  #     proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #     #data model
  #     out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
  #   }
  # }

  return(out)
}
