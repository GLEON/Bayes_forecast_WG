# Script to run linear correlations on covarites and gloeo data
# Last updated 2020 April 14 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

# set local directory for plots
my_directory <- ("C:/Users/Mary Lofton/Dropbox/Ch5/Covariate_analysis_output/")
my_directory <- ("~/Desktop/Covariate Analysis Plots/")


# Load all linear covariates ####

# gloeo data
hc_gloeo_data <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv")

# onset water temp data
water_temp_data <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all.csv")

# schmidt stability data
schmidt_stability_data <- read_csv("./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")

# precip data
precip_data <- read_csv("./00_Data_files/Covariate_analysis_data/PRISM_precip_all.csv")

# gdd
gdd <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days.csv")

# sw radiation
swrad <- read_csv("./00_Data_files/Covariate_analysis_data/solar_radiation_daily_summary.csv")

# par
par <- read_csv("./00_Data_files/Covariate_analysis_data/par_daily_summary.csv")

# wind speed data
wind_speed_data <- read_csv("./00_Data_files/Covariate_analysis_data/wind_speed_data_all_combined.csv")


# Join all covariate data with gloeo

covariates_all <- bind_cols(hc_gloeo_data[,c(1:5,10)], water_temp_data[,-1], schmidt_stability_data[,-1], precip_data[,-1], gdd[,3], swrad[,-1], par[,-1], wind_speed_data[,-1])


# Filter for just 2009-2014
covariates_all_filter <- covariates_all %>%
  filter(year %in% 2009:2014)

# Set-up for loop to run through covariates ####
# Outputs - for each year: Pearson's r, Spearman's r, linear r2, quadratic r2
# Fit all years together?

output <- matrix(nrow = length(covariates_all_filter)-6, ncol = 29) #length of covariates all -6

# vector of years
years <- c(2009:2014)

# set up counter to help with year indexing
col <- c(6,10,14,18,22,26)

# par data columns
colnames(covariates_all_filter[52:57])

# Run for loop ####
for(i in 7:ncol(covariates_all_filter)) {

  # put variable name in output matrix
  output[i-6,1] <- colnames(covariates_all_filter[,i])

  #prepare data to do exploratory viz plot
  plotdata <- data.frame(covariates_all_filter[,i],covariates_all_filter[,6])

  #write exploratory viz plot to file
  png(filename = paste(my_directory,paste0("gloeo_vs_",colnames(covariates_all_filter[,i]),".png")))
  plot(plotdata)
  dev.off()

  #prepare data for regression and correlation
  y <- unlist(covariates_all_filter[,6])
  x <- unlist(covariates_all_filter[,i])
  x2 <- unlist(covariates_all_filter[,i]^2)


  #run regression and correlations and assign to ouput matrix
  mod <- lm(y~x)
  output[i-6,2] <- round(summary(mod)$adj.r.squared,2)
  output[i-6,3] <- round(cor(x,y, method = "pearson", use = "complete.obs"), 2)
  output[i-6,4] <- round(cor(x,y, method = "spearman", use = "complete.obs"),2)
  quad_mod <- lm(y~x + x2) # x needs to be a matrix for lm to work
  output[i-6,5] <- round(summary(quad_mod)$adj.r.squared,2)

  #no par data for 2014 requires an if else statement to make sure 2014 excluded
  #from loop with par data columns
  if(colnames(covariates_all_filter[,i]) %in% colnames(covariates_all_filter)[52:57]){ # check columns match PAR
  for(j in 1:5) {

   #subset to a single year
   mod_year <- subset(covariates_all_filter, year==years[j])

   #prepare data for regression and correlation
   y <- unlist(mod_year[,6])
   x <- unlist(mod_year[,i])
   x2 <- unlist(mod_year[,i]^2)

   #run regression and correlations and assign to ouput matrix
   mod <- lm(y~x)
   output[i-6,col[j]] <- round(summary(mod)$adj.r.squared,2)
   output[i-6,col[j]+1] <- round(cor(x,y, method = "pearson", use = "complete.obs"), 2)
   output[i-6,col[j]+2] <- round(cor(x,y, method = "spearman", use = "complete.obs"),2)
   quad_mod <- lm(y~x + x2)
   output[i-6,col[j]+3] <- round(summary(quad_mod)$adj.r.squared,2)
  }}
  else{
    for(j in 1:length(years)) {

      #subset to a single year
      mod_year <- subset(covariates_all_filter, year==years[j])

      #prepare data for regression and correlation
      y <- unlist(mod_year[,6])
      x <- unlist(mod_year[,i])
      x2 <- unlist(mod_year[,i]^2)

      #run regression and correlations and assign to ouput matrix
      mod <- lm(y~x)
      output[i-6,col[j]] <- round(summary(mod)$adj.r.squared,2)
      output[i-6,col[j]+1] <- round(cor(x,y, method = "pearson", use = "complete.obs"), 2)
      output[i-6,col[j]+2] <- round(cor(x,y, method = "spearman", use = "complete.obs"),2)
      quad_mod <- lm(y~x + x2)
      output[i-6,col[j]+3] <- round(summary(quad_mod)$adj.r.squared,2)
    }

  }

}

output <- data.frame(output)
colnames(output) <- c("covariate_name","global_linear_r2","global_Pearsons_r","global_Spearmans_r","global_quad_r2",
                      "linear_r2_2009","Pearsons_r_2009","Spearmans_r_2009","quad_r2_2009",
                      "linear_r2_2010","Pearsons_r_2010","Spearmans_r_2010","quad_r2_2010",
                      "linear_r2_2011","Pearsons_r_2011","Spearmans_r_2011","quad_r2_2011",
                      "linear_r2_2012","Pearsons_r_2012","Spearmans_r_2012","quad_r2_2012",
                      "linear_r2_2013","Pearsons_r_2013","Spearmans_r_2013","quad_r2_2013",
                      "linear_r2_2014","Pearsons_r_2014","Spearmans_r_2014","quad_r2_2014")

write.csv(output, file = "./2_Covariate_correlation_analysis/output_wind.csv",row.names = FALSE)

#####################FILTERING FOR VARIABLES TO INCLUDE IN BAYES MODELS################
bayes_variables <- read_csv("./2_Covariate_correlation_analysis/output_wind.csv")

# Add row means for 2009-2014 for Pearsons & Spearmans

# Add row means for Pearson & Spearmans
pearson <- bayes_variables %>%
  select(starts_with("Pearsons"))

pearson_mean <- pearson %>%
  mutate(Pearson_2009_2014_mean = round(rowMeans(pearson, na.rm = T),2))

spearman <- bayes_variables %>%
  select(starts_with("Spearmans"))

spearman_mean <- spearman %>%
  mutate(Spearman_2009_2014_mean = round(rowMeans(spearman, na.rm = T),2))

# Bind back to original dataset
bayes_variables_means <- bind_cols(bayes_variables, pearson_mean[,7], spearman_mean[,7])

hist(bayes_variables$global_Pearsons_r)
hist(bayes_variables$global_Spearmans_r)

bayes_variables_keep <- bayes_variables_means %>%
  filter(abs(global_Spearmans_r)>=0.3 & abs(global_Pearsons_r)>=0.3) %>%
  select(1:5,30:31)

bayes_variables_keep2 <- bayes_variables_means %>%
  filter(abs(Pearson_2009_2014_mean)>=0.3 & abs(Spearman_2009_2014_mean)>=0.3) %>%
  select(1:5,30:31)


#this is where judgment comes in - we chose the following summary statistics:
#1. HCS.tempC_min
#2. HCS_tempC_min_lag
#3. ma_7 (7-day moving average of water temp.)/ JB - ma 14 is close but ok with 7!
#4. schmidt.stability_median_diff
#5. gdd_sum
#JB thoughts using global Pearsons & Spearmans for wind data:
#6. AveWindDir_cove_mean_2daylag - positive relationship with wind direction indicator variable
#7. AveWindSp_ms_min_3daylag_in - wind blowing in positive relationship, wind speed in less missing data than out (negative relationship)
# **Ignore windsp_cumsum_2day_in - too much missing data

# Missing data
sum(is.na(covariates_all_filter$AveWindSp_ms_min_3daylag_in)) #44
sum(is.na(covariates_all_filter$AveWindDir_cove_mean_2daylag)) #12
sum(is.na(covariates_all_filter$AveWindSp_ms_min_out)) #93
