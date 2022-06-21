# Title: 4.2 Calibrate Bayesian models
# History:
# State-space model - revised by S. LaDeau (11/2017) from the EcoForecast Activity
# by Michael Dietze, with reference "Ecological Forecasting", chapter 8
# updates JAZ, WB, MEL for Lofton et al. 2020
# 9 May 2022 - JB updates to run multi-site models

##################################SET-UP##############################################

#install to load and install other packages as needed
#install.packages('pacman')

#load packages
pacman::p_load(tidyverse, readxl, rjags, runjags, moments, coda)

#set a directory to use as a local file repository for plots if desire to write to file
my_directory <- "~/Documents/Gloeo Bayesian Modeling/R Output/Bayes_model_calibration_output/" #"C:/Users/Mary Lofton/Dropbox/Ch5/Bayes_model_calibration_output"
write_plots <- TRUE #FALSE

#make vector of model names for for-loop
#my_models <- c("RW","RW_obs","RW_bias","AC","base_DLM","wtrtemp_min","wtrtemp_min_lag","wtrtemp_MA7","schmidt_med_diff","wnd_dir_2day_lag","GDD","schmidt_max_lag","precip","schmidt_and_wind","wind_and_GDD","schmidt_and_GDD","schmidt_and_temp","temp_and_wind","RY")

# 1 site
my_models <- c("RW_obs_1site","RW_bias_1site","AC_1site","wtrtemp_min_and_GDD", "wtrtemp_min_and_GDD_RY")

# 3 sites
my_models <- c("RW_obs_3sites","RW_bias_3sites","AC_3sites", "wtrtemp_min_and_GDD_3sites", "wtrtemp_min_and_GDD_3sites_RY")

length(my_models)



########################CALIBRATE MODELS##############################################

for (i in 1:length(my_models)){

#1) Source helper functions ---------------------------------------------------------
  source('0_Function_library/model_calibration_plug_n_play_site.R')
  #source('0_Function_library/model_calibration_get_data_site_longformat.R') #check correct script sourced
  source('0_Function_library/model_calibration_get_data_multisite_longformat.R') #check correct script sourced
  source('0_Function_library/model_calibration_plots.R')

#2) Model options => pick model -----------------------------------------------------

model_name = my_models[i] # options are found in 4.1_JAGS_models
model=paste0("4.1_JAGS_models/",model_name, '.R') #Do not edit


#3) Read in data for model ------------------------------------------------------------------------------------------------------------

#see 0_Function_library/model_calibration_get_data.R for this function
cal_data <- get_calibration_data(model_name)


#4) JAGS Plug-Ins => initial conditions, priors, data, etc. --------------------------------------------------------------------------------------

#see 0_Function_library/model_calibration_plug_n_play.R for this function
jags_plug_ins <- jags_plug_ins(model_name = model_name)


#5) Run model (no edits, unless you want to change # of iterations) -------------------------------------------------------------
j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = jags_plug_ins$init.model,
                         n.chains = 3)

jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     adapt =  5000,
                     burnin =  10000,
                     sample = 50000, #50000
                     n.chains = 3,
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#convert to an MCMC list to calculate cross-correlation later
jags.out.mcmc <- as.mcmc.list(jags.out)


#6) Save output for calibration assessment

#save predicted states
Nmc = 10000
out <- as.matrix(jags.out.mcmc)
srow <- sample.int(nrow(out),Nmc,replace=TRUE)
mus <- out[srow,grep("mu",colnames(out))]
write.csv(mus,file = file.path("./5_Model_output/5.1_Calibration",paste0(model_name,'_predicted_states.csv')),row.names = FALSE)


#plot parameters
plot_parameters(params = jags_plug_ins$params.model,
                write_plots = write_plots,
                my_directory = my_directory)

#calculate parameter summaries, effective sample size, and cross-correlations
sum <- summary(jags.out, vars = jags_plug_ins$variable.names.model)
crosscorr <- crosscorr(jags.out.mcmc[,c(jags_plug_ins$params.model)])

#save results
sink(file = file.path("./5_Model_output/5.1_Calibration",paste0(model_name,'_param_summary.txt')))
print("Parameter summary")
print(sum)
print("Parameter cross-correlations")
print(crosscorr)
sink()


#7) Save runjags output
write.jagsfile(jags.out, file=file.path("./5_Model_output/5.1_Calibration",paste0(model_name,'_calibration.txt')),
               remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

}

#Congratulations! You have run all the models. You may have a cookie.


jags.out_DL_RY_3  <- coda.samples (model = j.model,
                                        variable.names = c("beta1","beta2","beta3","beta4","beta5", "tau_proc","tau_obs","tau_yr", "yr"), #"tau_yr", "yr"
                                        n.iter = 50000) #10000

plot(jags.out_DL)
summary(jags.out_DL)

gelman.diag(jags.out_DL)
GBR <- gelman.plot(jags.out_DL) #The point up to where the GBR drops below 1.05 is termed the "burn in" period

burnin = 10000                              ## determine convergence
jags.burn2 <- window(jags.out_DL,start=burnin)  ## remove burn-in
plot(jags.burn2)
summary(jags.burn2)
gelman.diag(jags.burn)


# Calculate DIC
DIC.multi <- dic.samples(j.model, n.iter=5000)
DIC.multi
