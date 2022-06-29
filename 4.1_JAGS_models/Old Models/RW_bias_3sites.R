model{

  for(j in 1:max(site_no)){

  #### Data Model

  for(t in 1:max(season_weeks)){
    #this fits the latent Gloeo to your observed Gloeo
    #run this on logged data
    y[t,j] ~ dnorm(mu[t,j],tau_obs)

  }

  #### Process Model

  for(t in 2:max(season_weeks)){

    #process model for Gloeo
    mu[t,j]~dnorm(lambda[t,j],tau_proc)
    lambda[t,j] <- beta1 + mu[t-1,j]

  }

    #Loops through items in seasonal for-loop and defines initial conditions
    mu[1,j] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix
    lambda[1,j] ~ dnorm(x_ic, tau_ic)

  }

  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1)
  tau_obs ~ dgamma(a_obs,r_obs)


}
