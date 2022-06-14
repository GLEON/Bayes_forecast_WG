model{

    #### Data Model
    #this fits the latent Gloeo to your observed Gloeo
    #run this on logged data

    for(j in 1:max(site_no)){

      for(t in 1:max(season_weeks)){

    y[t,j] ~ dnorm(mu[t,j],tau_obs)

    #gap-filling model for covariates
    covar1[t,j]~dnorm(week_avg1[t],tau_C1_proc)
    covar2[t,j]~dnorm(week_avg2[t],tau_C2_proc)

  }

  #### Process Model

  for(t in 2:max(season_weeks)){
    mu[t,j]~dnorm(lambda[t,j],tau_proc)
    lambda[t,j] <- beta1  + beta2*mu[t-1,j] + beta3*covar1[t,j] + beta4*covar2[t,j] + beta5*covar2[t,j]^2+ yr[year_no[t]] #changed beta2 to mu instead of lambda

  }

  #### Priors
  mu[1,j] ~ dnorm(x_ic,tau_ic)
  lambda[1,j] ~ dnorm(x_ic, tau_ic)

    }

  for(k in 1:totYr){
    yr[k] ~ dnorm(beta1, tau_yr) # k loop on yr, centered on beta1 instead of 0
  }

  beta1 ~ dnorm(beta.m1,beta.v1)
  beta2 ~ dnorm(beta.m2,beta.v2)
  beta3 ~ dnorm(beta.m3,beta.v3)
  beta4 ~ dnorm(beta.m4,beta.v4)
  beta5 ~ dnorm(beta.m5,beta.v5)

  tau_obs ~ dgamma(a_obs,r_obs)
  tau_C1_proc ~ dgamma(0.01,0.01)
  tau_C2_proc ~ dgamma(0.01,0.01)

  tau_proc ~ dgamma(a_proc,r_proc)
  tau_yr ~ dgamma(0.01,0.01)


}
