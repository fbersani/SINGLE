soft_threshold <-
function(x,lambda){
  # apply soft thresholding to x
  return( sign(x)* max(abs(x)-lambda, 0))
}
