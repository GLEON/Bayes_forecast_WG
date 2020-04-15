# Script to run linear correlations on covarites and gloeo data
# Last updated 2020 April 14 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

# Load all linear covariates ####

# gloeo data
hc_gloeo_data <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv")

# onset water temp data
water_temp_data <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all.csv")

# schmidt stability data
schmidt_stability_data <- read_csv("./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")

# wind speed data
wind_speed_data <- read_csv("./00_Data_files/Covariate_analysis_data/wind_speed_all.csv")

# precip data
precip_data <- read_csv("./00_Data_files/Covariate_analysis_data/PRISM_precip_all.csv")

# gdd
gdd <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days.csv")

# Join all covariate data with gloeo - no wind speed at the moment

covariates_all <- bind_cols(hc_gloeo_data[,c(1:5,10)], water_temp_data[,-1], schmidt_stability_data[,-1], precip_data[,-1])

str(covariates_all)

# Filter for just 2009-2014
covariates_all_filter <- covariates_all %>%
  filter(year %in% 2009:2014)

# Set-up for loop to run through covariates
# Outputs - for each year: Pearson's r, linear r2
# Fit all years together?

model1 <- lm(ln_totalperL ~ HCS.tempC_mean, data = covariates_all_filter)
summary(model1)

r2 <- summary(model1)$adj.r.squared

output <- matrix(nrow = 38, ncol = 14) #
# colname would be first column

# vector of years
2009-2014

for(i in 1:ncol(covariates_all_filter)) {

  output[i,1] <- colnames(covariates_all_filter[,i])

  mod <- lm(ln_totalperL ~ covariates_all_filter[,i], data = covariates_all_filter)
  r <- cor(covariates_all_filter$ln_totalperL, covariates_all_filter[,i], method = "pearson", use = "complete.obs")
  r2 <- summary(mod)$adj.r.squared

  for(j in 1: number of years) {
   # my_year filter for specific year
  }


}

colnames(output) <- c("global_r2")


