model{

  # Loop over all sites
  for(j in 1:max(site_no)){

  #### Data Model
  #this fits the latent Gloeo to your observed Gloeo
  #run this on logged data
  for(t in 1:max(season_weeks)){
    y[t,j] ~ dnorm(mu[t,j],tau_obs)
  }

  #### Process Model
  for(t in 2:max(season_weeks)){
    mu[t,j]~dnorm(lambda[t,j],tau_proc)
    lambda[t,j] <- beta1 + beta2*mu[t-1,j]
  }

    #### Initial conditions
    mu[1,j] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix
    #lambda[1] ~ dnorm(x_ic, tau_ic)

      } # end loop over sites

  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1)
  beta2 ~ dnorm(beta.m2,beta.v2)
  tau_obs ~ dgamma(a_obs,r_obs)

}
