choose_h <-
function(data, sample_size, kernel='gaussian', h){
  # estimate h using LOO likelihood
  # h is a vector of values of h to try
  samples = sample(2:nrow(data), sample_size)
  
  if (kernel=="gaussian"){
    results = unlist(lapply(h, FUN=function(x){est_log_like_LOO_samplesGiven(data, x, samples, kernel=kernel)}))    
  } else {
    results = unlist(lapply(h, FUN=function(x){est_log_like_LOO_samplesGiven(data, x, samples, kernel=kernel)}))
  }
  
  return(h[which(results==max(results))])
}
