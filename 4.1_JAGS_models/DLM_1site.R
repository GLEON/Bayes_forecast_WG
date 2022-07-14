model{

  #### Data Model
  #this fits the latent Gloeo to your observed Gloeo
  #run this on logged data
  for(t in 1:max(season_weeks)){
    y[t] ~ dnorm(mu[t],tau_obs)
  }

  #### Process Model
  for(t in 2:max(season_weeks)){
    mu[t]~dnorm(lambda[t],tau_proc)
    lambda[t] <- beta1 + beta2*mu[t-1]
  }

    #### Initial conditions
    mu[1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix
    #lambda[1] ~ dnorm(x_ic, tau_ic)


  #### Priors
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_proc ~ dgamma(a_proc,r_proc)

  beta1 ~ dnorm(beta.m1,beta.v1)
  beta2 ~ dnorm(beta.m2,beta.v2)

}
