model{


  for(t in 1:max(season_weeks)){
    #this fits the latent Gloeo to your observed Gloeo
    #run this on logged data
    y[t] ~ dnorm(mu[t],tau_obs)

  }

  #### Process Model

  for(t in 2:max(season_weeks)){

    #process model for Gloeo
    mu[t]~dnorm(lambda[t],tau_proc)
    lambda[t] <- beta2*mu[t-1]

  }

    #Loops through items in seasonal for-loop and defines initial conditions
    mu[1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix
    lambda[1] ~ dnorm(x_ic, tau_ic)



  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta2 ~ dnorm(beta.m2,beta.v2)
  tau_obs ~ dgamma(a_obs,r_obs)

}
