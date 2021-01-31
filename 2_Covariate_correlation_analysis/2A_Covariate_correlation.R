# Script to run Spearman's correlations and quadratic regressions on environmental covariates and gloeo data
# Last updated 2020 May 29 - JB

# Load packages ####
# run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages

pacman::p_load(tidyverse)

# set local directory for plots
# my_directory <- ("C:/Users/Mary Lofton/Dropbox/Ch5/Covariate_analysis_output/")

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
water_temp_data <- read_csv("./00_Data_files/Covariate_analysis_data/onset_watertemp_all_HC.csv")

# schmidt stability data
schmidt_stability_data <- read_csv("./00_Data_files/Covariate_analysis_data/schmidt_stability_all.csv")

# precip data
precip_data <- read_csv("./00_Data_files/Covariate_analysis_data/PRISM_precipitation_2009-2016.csv")

# sw radiation
swrad <- read_csv("./00_Data_files/Covariate_analysis_data/NLDAS_solar_radiation_2009-2016.csv")

# par
par <- read_csv("./00_Data_files/Covariate_analysis_data/par_daily_summary.csv")

# wind data
wind_data <- read_csv("./00_Data_files/Covariate_analysis_data/wind_data_all.csv")

# Join all covariate data with gloeo
covariates_all <- bind_cols(hc_gloeo_data[,c(1:5,10)], water_temp_data[,-1], schmidt_stability_data[,-1], precip_data[,-1], swrad[,-1], par[,-1], wind_data[-1])

# Filter for just 2009-2014
covariates_all_filter <- covariates_all %>%
  filter(year %in% 2009:2014)

# Set-up for loop to run through covariates ####
# Outputs - for each year: Pearson's r, Spearman's r, linear r2, quadratic r2

output <- matrix(nrow = length(covariates_all_filter)-6, ncol = 9) #length of covariates all -6

# vector of years
# years <- c(2009:2014)

# # set up counter to help with year indexing
# col <- c(6,10,14,18,22,26)
#
# # par data columns - special case b/c missing 2014 data
# colnames(covariates_all_filter[52:57])

# Run for loop ####
for(i in 7:ncol(covariates_all_filter)) {

  # put variable name in output matrix
  output[i-6,1] <- colnames(covariates_all_filter[,i])

  #prepare data to do exploratory viz plot
  # plotdata <- data.frame(covariates_all_filter[,i],covariates_all_filter[,6])
  #
  # #write exploratory viz plots to file
  # png(filename = paste(my_directory,paste0("gloeo_vs_",colnames(covariates_all_filter[,i]),".png")))
  # plot(plotdata)
  # dev.off()
  #
  # png(filename = paste(my_directory,paste0(colnames(covariates_all_filter[,i]),"_hist.png")))
  # hist(as.numeric(unlist(covariates_all_filter[,i])), main = colnames(covariates_all_filter[,i]))
  # dev.off()
  #
  # png(filename = paste(my_directory,paste0(colnames(covariates_all_filter[,i]),"_QQplot.png")))
  # qqnorm(as.numeric(unlist(covariates_all_filter[,i])), pch = 1, frame = FALSE,main = colnames(covariates_all_filter[,i]))
  # qqline(as.numeric(unlist(covariates_all_filter[,i])), col = "steelblue", lwd = 2)
  # dev.off()

  #prepare data for regression and correlation
  y <- unlist(covariates_all_filter[,6])
  x <- unlist(covariates_all_filter[,i])
  x2 <- unlist(covariates_all_filter[,i]^2)


  #run regression and correlations and assign to ouput matrix
  mod <- lm(y~x)
  output[i-6,2] <- round(summary(mod)$adj.r.squared,2)
  output[i-6,3] <- round(cor(x,y, method = "pearson", use = "complete.obs"), 2)
  output[i-6,4] <- round(cor(x,y, method = "spearman", use = "complete.obs"),2)
  quad_mod <- lm(y~x + x2)
  output[i-6,5] <- round(summary(quad_mod)$adj.r.squared,2)
  output[i-6,6] <- lmp(mod)
  pearson_p <- cor.test(x,y, method = "pearson", use = "complete.obs")
  output[i-6,7] <- pearson_p$p.value
  spearman_p <- cor.test(x,y, method = "spearman", use = "complete.obs", exact = F)
  output[i-6,8] <- spearman_p$p.value
  output[i-6,9] <- lmp(quad_mod)

  # #no par data for 2014 requires an if else statement to make sure 2014 excluded
  # #from loop with par data columns
  # if(colnames(covariates_all_filter[,i]) %in% colnames(covariates_all_filter)[52:57]){ # check columns match PAR
  # for(j in 1:5) {
  #
  #  #subset to a single year
  #  mod_year <- subset(covariates_all_filter, year==years[j])
  #
  #  #prepare data for regression and correlation
  #  y <- unlist(mod_year[,6])
  #  x <- unlist(mod_year[,i])
  #  x2 <- unlist(mod_year[,i]^2)
  #
  #  #run regression and correlations and assign to ouput matrix
  #  mod <- lm(y~x)
  #  output[i-6,col[j]] <- round(summary(mod)$adj.r.squared,2)
  #  output[i-6,col[j]+1] <- round(cor(x,y, method = "pearson", use = "complete.obs"), 2)
  #  output[i-6,col[j]+2] <- round(cor(x,y, method = "spearman", use = "complete.obs"),2)
  #  quad_mod <- lm(y~x + x2)
  #  output[i-6,col[j]+3] <- round(summary(quad_mod)$adj.r.squared,2)
  # }}
  # else{
  #   for(j in 1:length(years)) {
  #
  #     #subset to a single year
  #     mod_year <- subset(covariates_all_filter, year==years[j])
  #
  #     #prepare data for regression and correlation
  #     y <- unlist(mod_year[,6])
  #     x <- unlist(mod_year[,i])
  #     x2 <- unlist(mod_year[,i]^2)
  #
  #     #run regression and correlations and assign to ouput matrix
  #     mod <- lm(y~x)
  #     output[i-6,col[j]] <- round(summary(mod)$adj.r.squared,2)
  #     output[i-6,col[j]+1] <- round(cor(x,y, method = "pearson", use = "complete.obs"), 2)
  #     output[i-6,col[j]+2] <- round(cor(x,y, method = "spearman", use = "complete.obs"),2)
  #     quad_mod <- lm(y~x + x2)
  #     output[i-6,col[j]+3] <- round(summary(quad_mod)$adj.r.squared,2)
  #   }
  #
  # }

}

output <- data.frame(output)
colnames(output) <- c("covariate_name","global_linear_r2","global_Pearsons_r","global_Spearmans_r","global_quad_r2","linear_pvalue", "Pearsons_pvalue","Spearmans_pvalue","quad_pvalue")
# "linear_r2_2009","Pearsons_r_2009","Spearmans_r_2009","quad_r2_2009",
# "linear_r2_2010","Pearsons_r_2010","Spearmans_r_2010","quad_r2_2010",
# "linear_r2_2011","Pearsons_r_2011","Spearmans_r_2011","quad_r2_2011",
# "linear_r2_2012","Pearsons_r_2012","Spearmans_r_2012","quad_r2_2012",
# "linear_r2_2013","Pearsons_r_2013","Spearmans_r_2013","quad_r2_2013",
# "linear_r2_2014","Pearsons_r_2014","Spearmans_r_2014","quad_r2_2014")

write.csv(output, file = "./2_Covariate_correlation_analysis/output.csv",row.names = FALSE)

#####################FILTERING FOR VARIABLES TO INCLUDE IN BAYES MODELS################
bayes_variables <- read_csv("./2_Covariate_correlation_analysis/output.csv") %>%
  mutate(adj_Spearmans_pvalue = p.adjust(Spearmans_pvalue, method = "holm"))

bayes_variables_keep <- bayes_variables %>%
  filter((abs(global_Spearmans_r)>=0.3 & adj_Spearmans_pvalue <=0.05) | abs(global_quad_r2) >= 0.3)

mean(bayes_variables$global_Spearmans_r, na.rm = TRUE)

#this is where judgment comes in - we chose the following summary statistics based on the highest global Spearman's rho for each covariate group:
#1. HCS.tempC_min
#2. HCS_tempC_min_lag
#3. ma_7 (7-day moving average of water temp.)
#4. gdd_sum (growing degree days)
#5. schmidt.stability_max_lag
#6. schmidt.stability_median_diff
#7. precip_sum
#8. AveWindDir_cove_mean_2daylag
