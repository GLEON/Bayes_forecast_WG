# Title: Model validation metric functions
# History:
# created MEL 10APR20

#1. RMSE of log(totalperL)
#m = model values, o = observed values
rmse = function(m, o){
  sqrt(mean((m - o)^2, na.rm = TRUE))
}

#3. coverage (% of values falling within 95% predictive interval)
#pred_dist = distributions of predictions where each predictive distribution
#for each time point is a column
#obs = vector of observed values corresponding to predictive distributions
coverage = function(pred_dist, obs){

  pi <- apply(pred_dist,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  cover <- c(NULL)

  for (i in 1:length(obs)){
    if(is.na(obs[i])){cover[i] <- NA}
    else if(obs[i] > pi[1,i] & obs[i] < pi[3,i]){cover[i] <- 1}
    else{cover[i] <- 0}
  }

  coverage = round((sum(cover, na.rm = TRUE))/(length(subset(cover,!is.na(cover))))*100,1)
  return(coverage)

  }

#4. peak timing metric (when did model predict the peak vs. when it occurred)
#reported as difference in weeks, where -1 means model predicted 1 week early
#and 1 means model predicted 1 week late
#pred = vector of predicted values, obs = vector of observed values
peak_timing = function(pred, obs){
  peak <- which.max(obs)
  pred_peak <- which.max(pred)
  peak_timing = pred_peak - peak
  return(peak_timing)
}

#5. Mean quantile of observations in distribution of predictions
#pred_dist = distributions of predictions where each predictive distribution
#for each time point is a column
#obs = vector of observed values corresponding to predictive distributions
mean_quantile = function(pred_dist, obs){

  obs_quantile <- c(NULL)

  for (i in 1:length(obs)){
    percentile <- ecdf(pred_dist[,i])
    obs_quantile[i] <- percentile(obs[i])
  }

  mean_quantile <- mean(obs_quantile, na.rm = TRUE)
  return(mean_quantile)

}

#6. Quantile of 2015 max. density in predictive interval
#pred_dist = distributions of predictions where each predictive distribution
#for each time point is a column
#obs = vector of observed values corresponding to predictive distributions
max_quantile = function(pred_dist, obs){
  max_density <- which.max(obs)
  percentile <- ecdf(pred_dist[,max_density])
  obs_quantile <- percentile(max(obs, na.rm = TRUE))
  return(obs_quantile)
}

#8. **Mean diff. in predicted-observed in total per L
#pred_dist = distributions of predictions where each predictive distribution
#for each time point is a column
#obs = vector of observed values corresponding to predictive distributions
bias = function(pred_dist, obs){

  obs_diff <- c(NULL)

  for (i in 1:length(obs)){
    obs_diff[i] <- median(pred_dist[,i], na.rm = TRUE) - obs[i]
  }

  bias <- mean(obs_diff, na.rm = TRUE)
  return(bias)
}

#9. **Bias in predictions during highest density point in 2015
#pred_dist = distributions of predictions where each predictive distribution
#for each time point is a column
#obs = vector of observed values corresponding to predictive distributions
max_bias = function(pred_dist, obs){
  max_density <- which.max(obs)
  obs_diff <- mean(pred_dist[,max_density], na.rm = TRUE) - max(obs, na.rm = TRUE)
  return(obs_diff)
}

#10. **Mean range of 95% predictive interval in total per L
#pred_dist = distributions of predictions where each predictive distribution
#for each time point is a column
mean_range = function(pred_dist){
  pi <- apply(pred_dist,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  ranges <- pi[3,] - pi[1,]
  mean_range <- mean(ranges, na.rm = TRUE)
  return(mean_range)
}
