# Title: Create partitioned variance matrix for models from hindcasting output
# History:
# created MEL 2019
# MEL updates for final publication 10APR20

make_varMat <- function(model_name){

  if(model_name == "RW" | model_name == "RW_obs"){
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    var.w_obs    <- apply(vardat.w_obs,2,var)
    vm <- rbind(var.IC,var.IC.P,var.w_obs)
  }

  else if(model_name == "AR"){
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    var.IC.P.Pa   <- apply(vardat.IC.P.Pa,2,var)
    var.w_obs    <- apply(vardat.w_obs,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.Pa,var.w_obs)
  }

  else{
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    var.IC.P.Pa   <- apply(vardat.IC.P.Pa,2,var)
    var.IC.P.Pa.D   <- apply(vardat.IC.P.Pa.D,2,var)
    var.w_obs    <- apply(vardat.w_obs,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.Pa,var.IC.P.Pa.D,var.w_obs)
  }

  return(vm)

}
