#Forecast plug n play - run hindcasts
#Author:Mary Lofton
#Date: 05OCT19

run_hindcast <- function(model_name, params, Nmc, IC, wk){

  #check that model is set up for hindcasting
  if(!model_name %in% c("RW","RW_obs","AR")){
    print("This model is not included in the hindcasting functions.")
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

  if(model_name == "RW"){
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

  if(model_name == "RW_obs"){
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

  if(model_name == "AR"){
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

  # if(model_name == "Seasonal_AR_Mintemp"){
  #   if(week_num %in% c(1:16,21:36)){
  #     for(j in 1:5){
  #       #set initial conditions
  #       if(j == 1){gloeo_prev <- IC}
  #       #temp model
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
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
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
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
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
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
  #       Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #       #process model
  #       gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
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
  #     Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
  #     #process model
  #     gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
  #     proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
  #     #data model
  #     out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
  #   }
  # }
  #
  # if(model_name == "Seasonal_DayLength_Quad"){
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
  # if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
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
  # if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
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
