#Title: Renaming hindcast files
#Author: Mary Lofton
#Date: 25MAY20
library(tidyverse)

setwd("./9_Data_publication/")

startingDir <- "./5_Model_output/5.2_Hindcasting/"

#create wks vector
wks <- c(paste0("2015_",1:20),paste0("2016_",1:20))

#get sampling dates
obs <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv") %>%
  filter(year %in% c(2015:2016))
dates <- as.character(obs$date)
new_wks <- c("2015-05-14",dates[1:19],"2016-05-19",dates[21:39])

#create vector of model names
model_names <- c("RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","GDD","wnd_dir_2day_lag","schmidt_max_lag","schmidt_and_wind","temp_and_wind","wind_and_GDD")
new_model_names <- c("RW","BiasRW","AC","BaseDLM","MinWaterTemp","MinWaterTempLag","WaterTempMA","GDD","WindDir","SchmidtLag","SchmidtAndWind","TempAndWind","WindAndGDD")

for(n in 13:length(model_names)){

filez<-list.files(startingDir,pattern=paste0(model_names[n],"_hindcast."))

if(model_names[n] == "GDD"){filez <- filez[1:200]}


for (i in 1:length(filez)){

  fil <- read.csv(filez[i])

  colnames(fil) <- c("wk_1","wk_2","wk_3","wk_4")

  my_filename0 <- sub(pattern=paste0(model_names[n],"_hindcast."),replacement=paste0(new_model_names[n],"_"),filez[i])

  for (j in 1:length(wks)){
    if(grepl(wks[j], my_filename0, fixed = TRUE)){
      my_filename <- sub(pattern=wks[j],replacement=new_wks[j],my_filename0)
    }
  }

  write.csv(fil, file = my_filename, row.names = FALSE)

}
}


