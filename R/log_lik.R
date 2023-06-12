log_lik <-
function(obs, est_cov){
  # estimate log likelihood of MVG assuming 0 mean
  L = - log(det(est_cov)) + t(obs) %*% est_cov %*% (obs)
  return (-0.5 * L)
}
