# Script to run linear correlations on covarites and gloeo data
# Last updated 2020 April 14 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate, googledrive)

# set local directory for plots
my_directory <- ("C:/Users/Mary Lofton/Dropbox/Ch5/Covariate_analysis_output/")

# Load all linear covariates ####

# gloeo data
hc_gloeo_data <- read_csv("./00_Data_files/Covariate_analysis_data/HC_Gechinulata_long.csv")

# gdd
gdd <- read_csv("./00_Data_files/Covariate_analysis_data/growing_degree_days.csv")

# sw radiation
swrad <- read_csv("./00_Data_files/Covariate_analysis_data/solar_radiation_daily_summary.csv")

# par
par <- read_csv("./00_Data_files/Covariate_analysis_data/par_daily_summary.csv")


# Join all covariate data with gloeo - no wind speed at the moment

covariates_all <- bind_cols(hc_gloeo_data[,c(1:5,10)], gdd[,3], swrad[,-1], par[,-1])

str(covariates_all)

# Filter for just 2009-2014
covariates_all_filter <- covariates_all %>%
  filter(year %in% 2009:2014)

# Set-up for loop to run through covariates
# Outputs - for each year: Pearson's r, Spearman's r, linear r2
# Fit all years together?

# set up output matrix
output <- matrix(nrow = 13, ncol = 8)

# vector of years
years <- c(2009:2014)

for(i in 7:ncol(covariates_all_filter)) {

  # put variable name in output matrix
  output[i-6,1] <- colnames(covariates_all_filter[,i])

  #prepare data to do exploratory viz plot
  plotdata <- data.frame(covariates_all_filter[,i],covariates_all_filter[,6])

  #write exploratory viz plot to file
  png(filename = paste(my_directory,paste0("gloeo_vs_",colnames(covariates_all_filter[,i]),".png")))
  plot(plotdata)
  dev.off()

  #prepare data for quadratic regression
  y <- unlist(covariates_all_filter[,6])
  x <- unlist(covariates_all_filter[,i])
  x2 <- unlist(covariates_all_filter[,i]^2)

  #find r2 of quadratic regression and assign to output matrix
  mod <- lm(y ~ x + x2)
  r2 <- summary(mod)$adj.r.squared
  output[i-6,2] <- round(r2,2)

  #no par data for 2014 requires an if else statement to make sure 2014 excluded
  #from loop with par data columns
  if(colnames(covariates_all_filter[,i]) %in% colnames(covariates_all_filter)[7:13]){
  for(j in 1:length(years)) {

   this_year <- covariates_all_filter %>%
     filter(year == years[j])

   #prepare data for quadratic regression
   y <- unlist(this_year[,6])
   x <- unlist(this_year[,i])
   x2 <- unlist(this_year[,i]^2)

   #find r2 of quadratic regression and assign to output matrix
   mod <- lm(y ~ x + x2)
   r2 <- summary(mod)$adj.r.squared
   output[i-6,j+2] <- round(r2,2)}
    }

    else{
      for(j in 1:5) {

        this_year <- covariates_all_filter %>%
          filter(year == years[j])

        #prepare data for quadratic regression
        y <- unlist(this_year[,6])
        x <- unlist(this_year[,i])
        x2 <- unlist(this_year[,i]^2)

        #find r2 of quadratic regression and assign to output matrix
        mod <- lm(y ~ x + x2)
        r2 <- summary(mod)$adj.r.squared
        output[i-6,j+2] <- round(r2,2)}
    }
}

output <- data.frame(output)
colnames(output) <- c("covariate_name","global_quad_r2","2009_quad_r2","2010_quad_r2","2011_quad_r2","2012_quad_r2","2013_quad_r2","2014_quad_r2")
write.csv(output, file = "./2_Covariate_correlation_analysis/quad_output.csv",row.names = FALSE)

