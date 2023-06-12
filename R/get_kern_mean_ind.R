get_kern_mean_ind <-
function(ID, data, h, kernel='gaussian'){
  # calculate kernel mean for a given data point based on Gaussian kernel with specified radius, h
  x = seq(1:nrow(data))
  
  if (kernel=='gaussian'){
    norm_ = my_exp_kern(ID,x,h)    
  } else if (kernel=='window'){
    norm_ = my_window_kern(ID, x, h)
  }
  
  norm_[ID] = 0
    
  mean_ = apply(data * norm_,2,FUN=function(x){sum(x)/sum(norm_)})
  
  return(mean_)
}
