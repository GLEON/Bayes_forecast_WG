# Title: Model calibration plots
# History:
# created by MEL 27MAR20

plot_parameters <- function(params, write_plots, my_directory){

  if (write_plots == TRUE){
    for (i in 1:length(params)){
      png(file=file.path(my_directory,paste(model_name,paste0(params[i],'.png'), sep = '_')))
      plot(jags.out, vars = params[i])
      dev.off()
    }
  } else {
    for (i in 1:length(params)){
      return(plot(jags.out, vars = params[i]))
    }
  }
}
