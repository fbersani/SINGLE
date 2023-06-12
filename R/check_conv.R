check_conv <-
function(theta, Z, Zold, tol){
  # theta, Z and Zold all lists. Used to check convergence
  cond1 = TRUE
  cond2 = TRUE
  for (i in 1:length(theta)){
    if (sum((theta[[i]]-Z[[i]])**2) >= tol){
      cond1 = FALSE
    }
    if (sum((Z[[i]] - Zold[[i]])**2) >= tol){
      cond2 = FALSE
    }
  }
  return(cond1 & cond2)
}
