get_aic = function(data, precision){
  # precision is a list of estimated precision matrices. 
  # we use this to estimate the number of effictive parameters as 
  # suggested by Tibshirani 2005 "Sparsity & Smoothness usign Fused Lasso"
  # make into an array:
  precision_ = array(unlist(precision), c(ncol(precision[[1]]), ncol(precision[[1]]), length(precision)))
  
  p_no = dim(precision_)[1]
  k = 0
  for (i in 1:(p_no-1)){
    for (j in (i+1):p_no){
      k = k + length(unique(precision_[i,j,])) - 1 # subtract 1 so we dont include 0
    }
  }
  
  L = sapply(1:nrow(data), FUN=function(i){log_lik(data[i,], precision_[,,i])})
  return(2*k-2*sum(L))
}