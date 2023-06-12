minimize_z_fused_FL <-
function(A, lambda1, lambda2, rho){
  # minimisation for FUSED LASSO smoothness penalty
  # minimize Z as in this step c)ii) in 'The joint graphical lasso for inverse covariance estimation across multiple classes', Witten et al. 2012
  # A1 = theta1 + U1 (similarly for A2)
  A = array(unlist(A), dim=c(nrow(A[[1]]), ncol(A[[1]]), length(A)))
  sudoZ = A
  for (i in 1:ncol(A[,,1])){
    for (j in i:ncol(A[,,1])){ 
      resp = A[i,j,]
      beta_hat = flsa(y=resp, lambda1=lambda1, lambda2=lambda2, connListObj=NULL)
      #       sudoZ[i,j] = sudoZ[j,i] = beta_hat
      sudoZ[i,j,] = sudoZ[j,i,] = unlist(lapply(beta_hat, unlist))
    }
  }
  # put back into a list (must be a better way of doing this!)
  Z = list()
  for (i in 1:dim(sudoZ)[3]){
    Z[[i]] = sudoZ[,,i]
  }
  return(Z)
}
