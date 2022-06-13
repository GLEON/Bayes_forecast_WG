model{

  for(j in 1:max(site_no)){

    for(t in 1:max(season_weeks)){
      #this fits the blended model to your observed data.
      y[t,j] ~ dnorm(mu[t,j],tau_obs)
    }
    #### Process Model
    for(t in 2:max(season_weeks)){
      mu[t,j]~dnorm(mu[t-1,j],tau_proc)
    }
    #Loops through number of years and defines prior for each year
    mu[1,j] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix
  }

  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  tau_obs ~ dgamma(a_obs, r_obs)

}
