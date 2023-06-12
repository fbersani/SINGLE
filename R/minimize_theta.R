minimize_theta <-
function(S_, rho, obs){
  # minimize theta as in step c)i) in 'The joint graphical lasso for inverse covariance estimation across multiple classes', Witten et al. 2012
  # S_ is matrix S - (rho/obs)*Z + (rho/obs)*U
  # obs is sample size (in this case effective sample size)
  
  V = eigen(S_)  
  #   D = diag(V$values)
  D_ = V$values
  V = V$vectors
  
  D = diag(obs/(2*rho) * (-D_ + sqrt(D_**2 + 4*rho/obs)))
  
  return(V%*%D%*%t(V))
  
}
