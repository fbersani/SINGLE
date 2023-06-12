est_log_like_LOO_samplesGiven <-
function(data, h, kernel='gaussian', samples){
  # samples given to avoid unnecessary variability
  
  log_lik = lapply(samples, FUN=function(x){log_like_h_LOO(data,ID=x,h, kernel=kernel)})
  return(mean(unlist(log_lik)))
}
