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
years <- c(2009:2014)

# create matrix just year + gloeo + covariates
covariates_all_filter1 <- covariates_all_filter[,c(2,6:44)]
covariates_all_filter_matrix <- as.matrix(covariates_all_filter[,c(2, 6:44)])

covariates_all_filter2 <- covariates_all_filter[,c(6:44)]


# Set-up output matrix

output <- matrix(NA,nrow = length(covariates_all_filter1), ncol = 21) #42 rows
length(covariates_all_filter1)

for(i in 1:ncol(covariates_all_filter1)) {

  output[i,1] <- colnames(covariates_all_filter1[,i])

  mod <- lm(ln_totalperL ~ covariates_all_filter_matrix[,i], data = covariates_all_filter1) # x needs to be a matrix for lm to work
  output[i,2] <- round(cor(covariates_all_filter1$ln_totalperL, covariates_all_filter1[,i], method = "pearson", use = "complete.obs"), 2)
  output[i,3] <- round(cor(covariates_all_filter1$ln_totalperL, covariates_all_filter1[,i], method = "spearman", use = "complete.obs"),2)
  output[i,4] <- summary(mod)$adj.r.squared


  for(j in 1:length(years)) {
   mod_year <- subset(covariates_all_filter1, year==years[j])
   mod_year_matrix <- as.matrix(mod_year)
   mod2 <- lm(ln_totalperL ~ mod_year_matrix[,i], data = mod_year)
   output[i, j +6] <- round(cor(mod_year$ln_totalperL, mod_year[,i], method = "pearson", use = "complete.obs"),2)
   output[i, j +6] <- round(cor(mod_year$ln_totalperL, mod_year[,i], method = "spearman", use = "complete.obs"),2)
   output[i, j +6] <- summary(mod2)$adj.r.squared

  }

}


colnames(output) <- c("global_r2","r_pearson", "r_spearman", "adj_r2")

# Testing

mod <- lm(covariates_all_filter1$ln_totalperL ~ covariates_all_filter1$HCS.tempC_mean)
mod <- lm(ln_totalperL ~ 	precip_mm_1weeklag, data = covariates_all_filter2)
summary(mod)


mod_year <- subset(covariates_all_filter1, year==years[5])
cor(mod_year$ln_totalperL, mod_year[,3], method = "pearson", use = "complete.obs")


str(years)
