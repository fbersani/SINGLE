precision_recall <-
function(true_cov, estimated_cov){
  # true_cov is an array with true covariance structures
  # estimated_cov is another array with my estimated covariance structures
  my_pres = rep(0, dim(true_cov)[3])
  my_recall = rep(0, dim(true_cov)[3])
  
  for (i in 1:length(my_pres)){
    diag(estimated_cov[,,i])=0
    diag(true_cov[,,i])=0
    retrieved = which(estimated_cov[,,i]!=0)
    true_ = which(true_cov[,,i]!=0)
    if (length(true_)==0){
      if (length(retrieved)==0){
        my_pres[i] = my_recall[i] = 1
      } else {
        my_pres[i] = 0
        my_recall[i] = 1
      }
    } else {
      my_recall[i] = sum(retrieved %in% true_)/length(true_)
      if (length(retrieved)==0){
        # missed them all (retrieved nothing!)
        my_pres[i] = 0
      } else {
        # everything ok,neither retrieved nor true_ is 0
        my_pres[i] = sum(retrieved %in% true_)/length(retrieved)
      }
    }
  }
  
  f1_scores = 2*(my_pres * my_recall)/(my_pres + my_recall+0.00001)
    
  return(list(pres=my_pres, recall=my_recall, F1=f1_scores))
}
