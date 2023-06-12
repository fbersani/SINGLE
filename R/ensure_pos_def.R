ensure_pos_def <-
function(P){
  # ensure square matrix P is positive definite by making a few changes 
  # as suggested by Danaher et al. (2012)
  
  # remove diagonal if present:
  diag(P)=0
  eps = 1e-05
  
  # first divide all entries by sum of absolute values
  row_sum = apply(P, 1, FUN=function(x){sum(abs(x))})
  p = nrow(P)
  denom_matrix = matrix(rep(1.5*row_sum + eps,p), ncol=p, byrow=TRUE)
  P = P/denom_matrix
  
  # now average with transpose:
  P = 0.5 * (P + t(P))
  diag(P) = 1
  
  return(P)
  
}
