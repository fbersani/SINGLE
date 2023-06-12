get_kern_cov_ind <-
function(data, ID, h, centered=FALSE, kernel='gaussian'){
  # calculate kernel covariance for a given data point based on Gassiand kernel with specified radius, h
  
  # center data first (if needed):
  if (centered==FALSE){
    data = data - get_kern_mean(data, h=h, kernel=kernel)    
  }

  x = seq(1:nrow(data))
  
  if (kernel=='gaussian'){
    norm_ = my_exp_kern(ID,x,h)    
  } else if (kernel=='window'){
    norm_ = my_window_kern(ID, x, h)
  }
  norm_[ID] = 0
  data = data * sqrt(norm_)
  
  cov_ = matrix(apply(apply(data, 1, FUN=function(x){x%*%t(x)}),1,sum), ncol=ncol(data))
 
  return(cov_/sum(norm_))
  
 # cov_ = matrix(apply(apply(data, 1, FUN=function(x){x%*%t(x)}), 1, FUN=function(x){sum(x)/sum(norm_)}), ncol=ncol(data)) 
}
