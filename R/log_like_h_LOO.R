log_like_h_LOO <-
function(data, ID, h, kernel='gaussian'){
  # estimate look ahead likelihood with leave-one-out cross validation 
    
  mu_est = get_kern_mean_ind(data=data, ID=ID, h=h, kernel=kernel)
  C_est = get_kern_cov_ind(data=data, ID=ID, h=h, kernel=kernel)
  #   C_est = C_est/sqrt(diag(C_est)) %*% t(sqrt(diag(C_est)))
  
  log_lik = log(det(C_est)) + t(data[ID,]-mu_est) %*% solve(C_est) %*% (data[ID,] - mu_est)
  
  return (-0.5 *log_lik )
}
