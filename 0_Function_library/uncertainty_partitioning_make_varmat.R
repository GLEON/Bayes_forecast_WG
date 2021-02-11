# Title: Create partitioned variance matrix for models from hindcasting output
# History:
# created MEL 2019
# MEL updates for final publication 10APR20

make_varMat <- function(model_name){

  if(model_name == "RW" | model_name == "RW_obs"){
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    vm <- rbind(var.IC,var.IC.P)
  }

  else if(model_name %in% c("RW_bias","AC","base_DLM")){
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.Pa    <- apply(vardat.IC.Pa,2,var)
    var.IC.Pa.P   <- apply(vardat.IC.Pa.P,2,var)
    vm <- rbind(var.IC,var.IC.Pa,var.IC.Pa.P)
  }

  else{
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.Pa    <- apply(vardat.IC.Pa,2,var)
    var.IC.Pa.D   <- apply(vardat.IC.Pa.D,2,var)
    var.IC.Pa.D.P   <- apply(vardat.IC.Pa.D.P,2,var)
    vm <- rbind(var.IC,var.IC.Pa,var.IC.Pa.D,var.IC.Pa.D.P)
  }

  return(vm)

}
