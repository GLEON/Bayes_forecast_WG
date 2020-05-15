#2B_Bayes_selected_variables_correlation
#Checking correlations between environmental covariates that performed well
#in Bayesian single-covariate models
#Date: 15MAY20
#Author: MEL

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate)

#define function to extract p-value for linear and quadratic models
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Load all covariates ####

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

# wind speed data - a little data wrangling to fill filtered variables with 0 when
# wind blowing away from cove
wind_data <- read_csv("./00_Data_files/Covariate_analysis_data/wind_data.csv")

# Join all covariate data with gloeo
covariates_all <- bind_cols(hc_gloeo_data[,c(1:5,10)], water_temp_data[,-1], schmidt_stability_data[,-1], precip_data[,-1], gdd[,3], swrad[,-1], par[,-1], wind_data[-1])

# Filter for just 2009-2014
covariates_all_filter <- covariates_all %>%
  filter(year %in% 2009:2014)

#select covariates that make the model selection cut for single covar Bayes models
bayes_covars <- covariates_all_filter %>%
  select(date, HCS.tempC_min, schmidt.stability_median_diff, precip_mm, gdd_sum)

#plot all covars against each other
plot(bayes_covars$HCS.tempC_min,bayes_covars$schmidt.stability_median_diff)
plot(bayes_covars$HCS.tempC_min,bayes_covars$precip_mm)
plot(bayes_covars$HCS.tempC_min,bayes_covars$gdd_sum)
plot(bayes_covars$precip_mm,bayes_covars$schmidt.stability_median_diff)
plot(bayes_covars$precip_mm,bayes_covars$gdd_sum)
plot(bayes_covars$gdd_sum,bayes_covars$schmidt.stability_median_diff)

#check correlation coefficients for non-quadratic relationships
cor(bayes_covars$HCS.tempC_min,bayes_covars$schmidt.stability_median_diff,method = "spearman", use = "complete.obs")
cor.test(bayes_covars$HCS.tempC_min,bayes_covars$schmidt.stability_median_diff, method = "spearman", use = "complete.obs", exact = F)

cor(bayes_covars$HCS.tempC_min,bayes_covars$precip_mm,method = "spearman", use = "complete.obs")
cor.test(bayes_covars$HCS.tempC_min,bayes_covars$precip_mm, method = "spearman", use = "complete.obs", exact = F)

cor(bayes_covars$precip_mm,bayes_covars$schmidt.stability_median_diff,method = "spearman", use = "complete.obs")
cor.test(bayes_covars$precip_mm,bayes_covars$schmidt.stability_median_diff, method = "spearman", use = "complete.obs", exact = F)

cor(bayes_covars$precip_mm,bayes_covars$gdd_sum,method = "spearman", use = "complete.obs")
cor.test(bayes_covars$precip_mm,bayes_covars$gdd_sum, method = "spearman", use = "complete.obs", exact = F)

cor(bayes_covars$gdd_sum,bayes_covars$schmidt.stability_median_diff,method = "spearman", use = "complete.obs")
cor.test(bayes_covars$gdd_sum,bayes_covars$schmidt.stability_median_diff, method = "spearman", use = "complete.obs", exact = F)

#look at quadratic R2 for mintemp and GDD
y <- unlist(bayes_covars$HCS.tempC_min)
x <- unlist(bayes_covars$gdd_sum)
x2 <- unlist(bayes_covars$gdd_sum^2)

quad_mod <- lm(y~x + x2) # x needs to be a matrix for lm to work
round(summary(quad_mod)$adj.r.squared,2)
lmp(quad_mod)

#summary of findings:

#gdd_sum and schmidt_stability_median_diff have Spearman's r = -0.51 and
#highly significant p-value (p = 4.22e-7)

#gdd_sum and mintemp have quadratic R2 = 0.75 and
#highly significant p-value (p = 1.44e-32)

#all other Spearman's r < 0.3, and some relationships not even significant (p > 0.05)
