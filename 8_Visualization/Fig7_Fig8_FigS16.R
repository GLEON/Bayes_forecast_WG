# Title: 8B_Uncertainty_partitioning_output_plots
# History:
# created by MEL 19APR20
# Corresponds to Fig. XX in manuscript

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, lubridate,cowplot)

###############################FIGURE YY##########################################
#set local directory for writing plots
#my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Uncertainty_partitioning_analysis_output/"
my_directory <- "~/Documents/Gloeo Bayesian Modeling/R Output/Uncertainty_partitioning_analysis_output/"


#setting up counters and vectors for for-loop
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","wnd_dir_2day_lag","GDD","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD") #ensemble
model_labels <- c("a. RW","b. OffsetRW","c. AC","d. BaseLM","a. MinWaterTemp","b. MinWaterTempLag","c. WaterTempMA","d. WindDir","e. GDD","f. SchmidtLag","g. Schmidt+Wind","e. Temp+Wind","h. Wind+GDD") #"i. Ensemble"
schmidt_title <- expression(paste("e. ",Delta,"Schmidt", sep = ""))
forecast_weeks <- c(1:4)

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...)
}


##make plots
for(i in 1:length(model_names)){

  if(model_names[i] %in% c("RW","RW_obs")){
    plotdata <- matrix(NA,2,4)
  }
  if(model_names[i] %in% c("RW_bias","AC","base_DLM")){
    plotdata <- matrix(NA,3,4)
  }
  if(!model_names[i] %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
    plotdata <- matrix(NA,4,4)
  }



for(n in 1:length(forecast_weeks)){

  if(model_names[i] == "ensemble"){
    varpart <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv'),sep = " "))))
  } else {
    varpart <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv'),sep = " "))))
  }
  plotdata[1,n] <- mean(varpart[1,], na.rm = TRUE)
  plotdata[2,n] <- mean(varpart[2,], na.rm = TRUE)

  if(!model_names[i] %in% c("RW","RW_obs")){
    plotdata[3,n] <- mean(varpart[3,], na.rm = TRUE)
  }
  if(!model_names[i] %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
    plotdata[4,n] <- mean(varpart[4,], na.rm = TRUE)
  }

}
  col = c("#e66101","#fdb863","#5e3c99","#b2abd2")

  if(model_names[i] %in% c("RW","RW_obs")){
  tiff(file = file.path(my_directory,paste0(model_names[i],"_V.pred.rel.tif")),
       width = 5, height = 3.5, units = "in", res = 300)
  # layout(rbind(1,2), heights=c(7,1))
  par(mar=c(2, 4.1, 2.5, 1), mgp = c(3,1,0))
  plot(forecast_weeks, plotdata[1,], ylim=c(0,1), type='n', ylab="Proportion of Variance", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1,xaxt = "n")
  axis(side = 1, at=1:4,cex.axis = 1.5, labels = c("1wk","2wk","3wk","4wk"))
  ciEnvelope(forecast_weeks, rep(0,ncol(plotdata)), plotdata[1,], col = col[1])
  ciEnvelope(forecast_weeks, plotdata[1,], rep(1,ncol(plotdata)), col = col[4])
  axis(side = 2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis = 1.5, labels = c("","","","","",""))
  if(model_names[i]=="schmidt_med_diff"){
    title(expression(paste("e. ",Delta,"Schmidt", sep = "")),adj=0, font = 2, cex.main = 1.7, line = 0.5)
  } else {
    title(model_labels[i], adj = 0, font.main = 1, cex.main = 1.7, line = 0.5)}
  # par(mar=c(0, 2.1, 0, 0))
  # plot.new()
  # legend("center", legend=c("IC","Proc."), col=c(col[1],col[3]), lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 2, bty = "n", seg.len = 0.5)
  dev.off()}

  if(model_names[i] %in% c("RW_bias","AC","base_DLM")){
  tiff(file = file.path(my_directory,paste0(model_names[i],"_V.pred.rel.tif")),
       width = 5, height = 3.5, units = "in", res = 300)
  # layout(rbind(1,2), heights=c(7,1))
  par(mar=c(2, 4.1, 2.5, 1), mgp = c(3,1,0))
  plot(forecast_weeks, plotdata[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1,xaxt = "n")
  axis(side = 1, at=1:4,cex.axis = 1.5, labels = c("1wk","2wk","3wk","4wk"))
  ciEnvelope(forecast_weeks, rep(0,ncol(plotdata)), plotdata[1,], col = col[1])
  ciEnvelope(forecast_weeks, plotdata[1,], plotdata[2,], col = col[2])
  ciEnvelope(forecast_weeks, plotdata[2,], rep(1,ncol(plotdata)), col = col[4])
  axis(side = 2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis = 1.5, labels = c("","","","","",""))
  if(model_names[i]=="schmidt_med_diff"){
    title(expression(paste("e. ",Delta,"Schmidt", sep = "")),adj=0, font = 2, cex.main = 1.7, line = 0.5)
  } else {
    title(model_labels[i], adj = 0, font.main = 1, cex.main = 1.7, line = 0.5)}
  # par(mar=c(0, 2.1, 0, 0))
  # plot.new()
  # legend("center", legend=c("IC","Para.","Proc."), col=c(col[1],col[4],col[3]), lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 3, bty = "n", seg.len = 0.5)
  dev.off()}

  if(!model_names[i] %in% c("RW","RW_obs","RW_bias","AC","base_DLM")){
  tiff(file = file.path(my_directory,paste0(model_names[i],"_V.pred.rel.tif")),
       width = 5, height = 3.5, units = "in", res = 300)
  # layout(rbind(1,2), heights=c(7,1))
  par(mar=c(2, 4.1, 2.5, 1), mgp = c(3,1,0))
  plot(forecast_weeks, plotdata[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1,xaxt = "n")
  axis(side = 1, at=1:4,cex.axis = 1.5, labels = c("1wk","2wk","3wk","4wk"))
  ciEnvelope(forecast_weeks, rep(0,ncol(plotdata)), plotdata[1,], col = col[1])
  ciEnvelope(forecast_weeks, plotdata[1,], plotdata[2,], col = col[2])
  ciEnvelope(forecast_weeks, plotdata[2,], plotdata[3,], col = col[3])
  ciEnvelope(forecast_weeks, plotdata[3,], rep(1,ncol(plotdata)), col = col[4])
  axis(side = 2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis = 1.5, labels = c("","","","","",""))
  if(model_names[i]=="schmidt_med_diff"){
    title(schmidt_title,adj=0, font = 2, cex.main = 1.7, line = 1)
  } else {
    title(model_labels[i], adj = 0, font.main = 1, cex.main = 1.7, line = 0.5)}
  # par(mar=c(0, 2.1, 0, 0))
  # plot.new()
  # legend("center", legend=c("IC","Para.","Driv.","Proc."), col=c(col[1],col[4],col[2],col[3]), lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 4, bty = "n", seg.len = 0.5)
  dev.off()}

}


############plot that just has legend to include in final figures

tiff(file = file.path(paste(my_directory,paste0("Fig7_FigSX_legend.tif"),sep = "")),
     width = 4, height = 4, units = "in", res = 300)
plot.new()
legend("center", legend=c("Initial conditions","Parameter","Driver","Process"), col=c(col[1],col[2],col[3],col[4]), pch = 15, pt.cex = 2, bg = 'white', cex = 1.5, bty = "n")


dev.off()

###############################FIGURE ZZ##########################################

#set local directory for writing plots
#my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Uncertainty_partitioning_analysis_output/"

#setting up counters and vectors for for-loop
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","wnd_dir_2day_lag","GDD","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD")#,"ensemble"
model_labels <- c("RW","OffsetRW","AC","BaseLM","MinWaterTemp","MinWaterTempLag","WaterTempMA","WindDir","GDD","SchmidtLag","Schmidt+Wind","Temp+Wind","Wind+GDD") #,"Ensemble"
forecast_weeks <- c(1:4)
plotdata <- NULL

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...)
}


##make plots
for(n in 1:length(forecast_weeks)){

  plotdata0 <- matrix(NA,length(model_names),7)

for(i in 1:length(model_names)){

    plotdata0[i,1] <- model_names[i]
    plotdata0[i,2] <- model_labels[i]

    if(model_names[i] == "ensemble"){
    varpart <- as.matrix(read_csv(file=file.path(paste("./7_Model_ensemble/",paste0('ensemble_varRelative_',forecast_weeks[n],'.csv'),sep = " "))))
    } else {
    varpart <- as.matrix(read_csv(file=file.path(paste("./5_Model_output/5.3_Uncertainty_partitioning/",paste0(model_names[i],'_varRelative_',forecast_weeks[n],'.csv'),sep = " "))))}

    if(model_names[i] == "RW_obs"){
      plotdata0[i,3] <- mean(varpart[1,], na.rm = TRUE)
      plotdata0[i,6] <- mean(varpart[2,]-varpart[1,], na.rm = TRUE)
    }
    if(model_names[i] %in% c("RW_bias","AC","base_DLM")){
      plotdata0[i,3] <- mean(varpart[1,], na.rm = TRUE)
      plotdata0[i,4] <- mean(varpart[2,]-varpart[1,], na.rm = TRUE)
      plotdata0[i,6] <- mean(varpart[3,]-varpart[2,], na.rm = TRUE)
    }
    if(!model_names[i] %in% c("RW_obs","RW_bias","AC","base_DLM")){
      plotdata0[i,3] <- mean(varpart[1,], na.rm = TRUE)
      plotdata0[i,4] <- mean(varpart[2,]-varpart[1,], na.rm = TRUE)
      plotdata0[i,5] <- mean(varpart[3,]-varpart[2,], na.rm = TRUE)
      plotdata0[i,6] <- mean(varpart[4,]-varpart[3,], na.rm = TRUE)
    }

    plotdata0[i,7] <- forecast_weeks[n]

  }

  plotdata <- rbind(plotdata, plotdata0)

}

#data wrangling to get in shape for plotting
plotdata <- as_tibble(plotdata) %>%
  mutate(V3 = as.double(V3),
         V4 = as.double(V4),
         V5 = as.double(V5),
         V6 = as.double(V6),
         V7 = as.double(V7),
         V2 = fct_relevel(V2, "RW","OffsetRW","AC","BaseLM","MinWaterTemp","MinWaterTempLag","WaterTempMA","SchmidtLag","WindDir","GDD","Schmidt+Wind","Temp+Wind","Wind+GDD")) #,"Ensemble"
colnames(plotdata) <- c("model_name","mod_label","Initial Conditions","Parameter","Driver","Process","forecast_week")
plotdata1 <- as.data.frame(plotdata)
plotdata1$best_delPL <- c(NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,rep(NA, times = 13),rep(NA, times = 13),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,NA) # changed from 14 and one NA in each list for ensemble model

plotdata1 <- plotdata1 %>%
  gather(`Initial Conditions`:Process, key = "var_type",value = "mean_perc_var") %>%
  mutate(var_type = factor(var_type, levels = rev(c("Initial Conditions","Parameter","Driver","Process"))))

#set colors
col = c("#e66101","#fdb863","#5e3c99","#b2abd2")

#make plot
allvar1 <- ggplot(data = subset(plotdata1, plotdata1$forecast_week == 1), aes(x = mod_label,y = mean_perc_var,color = var_type,fill = var_type))+
  geom_col()+
  geom_point(aes(x = mod_label,y = best_delPL), col = "black",fill = "white",shape = 25, show.legend = FALSE, size = 7)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 14), legend.text = element_text(size = 13),
        legend.position = "none",plot.title = element_text(size = 18))+
  labs(fill = "Variance type:",color = "Variance type:")+
  scale_fill_manual(values = c(col[4],col[3],col[2],col[1]))+
  scale_color_manual(values = c(col[4],col[3],col[2],col[1]))+
  xlab("")+
  ggtitle("a. One-week horizon")+
  ylab("Proportion of Variance")
allvar1

allvar4 <- ggplot(data = subset(plotdata1, plotdata1$forecast_week == 4), aes(x = mod_label,y = mean_perc_var,color = var_type,fill = var_type))+
  geom_col()+
  geom_point(aes(x = mod_label,y = best_delPL),shape = 25,col = "black",fill = "white", size = 7)+
  #geom_point(aes(x = mod_label,y = best_RMSE), col = "black",shape = 8, show.legend = FALSE, size = 5)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16,angle = 90, hjust = 1,vjust = 0.01),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 18), legend.text = element_text(size = 18),
        legend.position = "bottom", plot.title = element_text(size = 18),
        legend.box="vertical")+
  labs(fill = "Variance type:",color = "Variance type:")+
  scale_fill_manual(values = c(col[4],col[3],col[2],col[1]))+
  scale_color_manual(values = c(col[4],col[3],col[2],col[1]))+
  xlab("")+
  ggtitle("b. Four-week horizon")+
  ylab("Proportion of Variance")+
  guides(fill = guide_legend(nrow=2),col = guide_legend(nrow=2))
allvar4

final_plot <- plot_grid(allvar1, allvar4, align = "hv", nrow = 2, ncol = 1,
                        rel_heights = c(1, 1.75))

ggsave(final_plot, filename = file.path(my_directory,paste0("V.pred.rel.all.models.tif")),device = "tiff",
       height = 9, width = 6, units = "in", scale = 1.1, dpi = 600)


rm(list = ls())

