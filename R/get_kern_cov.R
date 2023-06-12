get_kern_cov <-
function(data, h, kernel='gaussian'){
  # calculate kernel covariance for data based on Gassiand kernel with specified radius
  # returns an array with estimated covariances
  
  # center data first:
  data = data - get_kern_mean(data, h=h, kernel=kernel)
  
  kern_cov = array(0, c(ncol(data),ncol(data),nrow(data))) 
  for (i in 1:nrow(data)){
    kern_cov[,,i] = get_kern_cov_ind(data, i, h=h, centered=TRUE, kernel=kernel)
  }
  return(kern_cov)
}
