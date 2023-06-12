jgl_fgl_offline <-
function(S, lambda1, lambda2, rho, obs, max_iter=50, tol=0.001){
  # Joint Glasso with Fused Glasso penalty
  # Given a list of covariances (S) this will obtain sparse precision matricies that are temporally smooth
  # input:
  #     - S is a list where S[[i]] is the estimated covariance matrix at time i
  #     - lambda1, lambda2 are regularization parameters for glasso and fused glasso penalties respectively
  #     - rho is used to tune the stepsize (large value of rho result in smaller size size & higher computational cost)
  #     - obs is a vector withere obs[i] is number of observations (or ESS). We typically set obs = rep(1, length(S))
  
  # INITIALIZATION:
  theta = list()
  U = Z = Zold = list()
  # connected components for FUSED LASSO penalty!
  connlist = vector("list", length(S)) 
  class(connlist) = "connListObj"
  for (i in 1:length(S)){
    theta[[i]] = diag(ncol(S[[1]]))
    U[[i]] = Z[[i]] = Zold[[i]] = matrix(0, ncol=ncol(S[[1]]), nrow=ncol(S[[1]]))
    #connlist[[i]] = as.integer(seq(max(0, i-window), min(length(S)-1, i+window)))
  }
  
  
  #obs = obs/sum(obs)
  
  convergence = FALSE
  iter = 0
  while(convergence==FALSE & iter < max_iter){
    # update theta1, theta2
    A = list()
    for (i in 1:length(S)){
      theta[[i]] = minimize_theta(S_ = S[[i]] - (rho/obs[i])*Z[[i]] + (rho/obs[i])*U[[i]], rho=rho, obs=obs[i])
      A[[i]] = theta[[i]] + U[[i]]
    }
    
    Z = minimize_z_fused_FL(A, lambda1, lambda2, rho)
    
    for (i in 1:length(S)){
      U[[i]] = U[[i]] + theta[[i]] - Z[[i]]
    }
    
    #     cat(sum(abs(Z[[1]]-theta[[1]])), sum(abs(Z[[2]] - theta[[2]])), '\n')
    
    if (check_conv(theta, Z, Zold, tol)==TRUE){
      convergence = TRUE
    } else {
      # not converged
      iter = iter + 1
#       cat(iter,'\n')
      Zold  = Z
    }
  }
  return(list(theta=theta,Z=Z, converged=convergence))
}
