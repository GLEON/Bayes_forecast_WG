model{

    #### Data Model
    #this fits the latent Gloeo to your observed Gloeo
    #run this on logged data
    for(t in 1:max(season_weeks)){

    y[t] ~ dnorm(mu[t],tau_obs)

    #gap-filling model for covariates
    covar1[t]~dnorm(week_avg1[t],tau_C1_proc)
  #  covar2[t]~dnorm(week_avg2[t],tau_C2_proc) # no missing data for covar2 = air temp GDD

  }

  #### Process Model
  for(t in 2:max(season_weeks)){
    mu[t]~dnorm(lambda[t],tau_proc)
    lambda[t] <- beta1  + beta2*mu[t-1] + beta3*covar1[t] + beta4*covar2[t] + beta5*covar2[t]^2 #+ yr[year_no[t]]

  }

  #### Initial Conditions
  mu[1] ~ dnorm(x_ic,tau_ic)
  #lambda[1] ~ dnorm(x_ic, tau_ic) # might not need now?

  ### Random Year Effect
  # for(k in 1:totYr){
  #   yr[k] ~ dnorm(0, tau_yr) # Centered on 0 or beta1
  # }

  #### Priors
  beta1 ~ dnorm(beta.m1,beta.v1)
  beta2 ~ dnorm(beta.m2,beta.v2)
  beta3 ~ dnorm(beta.m3,beta.v3)
  beta4 ~ dnorm(beta.m4,beta.v4)
  beta5 ~ dnorm(beta.m5,beta.v5)

  tau_C1_proc ~ dgamma(0.01,0.01)
#  tau_C2_proc ~ dgamma(0.01,0.01)

  tau_obs ~ dgamma(a_obs,r_obs)
  tau_proc ~ dgamma(a_proc,r_proc)
  #tau_yr ~ dgamma(0.01,0.01)


}
