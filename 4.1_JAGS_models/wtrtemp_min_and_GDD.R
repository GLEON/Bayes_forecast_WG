model{

  for(k in 1:max(year_no)){ # Change to loop through sites, all years together

  for(j in 1:max(season_weeks)){

    #### Data Model
    #this fits the latent Gloeo to your observed Gloeo
    #run this on logged data
    y[k,j] ~ dnorm(mu[k,j],tau_obs)

    #gap-filling model for covariates
    covar1[k,j]~dnorm(week_avg1[j],tau_C1_proc)
    covar2[k,j]~dnorm(week_avg2[j],tau_C2_proc)

  }

  #### Process Model

  for(j in 2:max(season_weeks)){

    #process model for Gloeo
    # Things to add:
    # drop INTERACTION TERM BETWEEN 2 BETAS? - covariate 3 multiplication outside model
    #
    # Random site effect - 1 model with 1 model without, single site no random site
    # Fixed year effect - categorical
    mu[k,j]~dnorm(lambda[k,j],tau_proc)
    lambda[k,j] <- beta1  + beta2*mu[k,j-1] + beta3*covar1[k,j] + beta4*covar2[k,j] + beta5*covar2[k,j]^2

  }

    #Loops through items in seasonal for-loop and defines initial conditions
    mu[k,1] ~ dnorm(x_ic,tau_ic)

  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1)
  beta2 ~ dnorm(beta.m2,beta.v2)
  beta3 ~ dnorm(beta.m3,beta.v3)
  beta4 ~ dnorm(beta.m4,beta.v4)
  beta5 ~ dnorm(beta.m5,beta.v5)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_C1_proc ~ dgamma(0.01,0.01)
  tau_C2_proc ~ dgamma(0.01,0.01)


}
