get_kern_mean <-
function(data, h, kernel='gaussian'){
  # calculate mean based on Gaussian kernel with some specified radius, h
  ID = matrix(seq(1:nrow(data)), ncol=1)
  kern_mean = apply(ID,1, FUN=get_kern_mean_ind, data=data, h=h, kernel=kernel)
  
  return(t(kern_mean))
}
