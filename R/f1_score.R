f1_score <-
function(pr_object){
  # pr_object is an object returned from the function precision_recall
  if (sum(names(pr_object)==c('pres', 'recall'))!=2){
    stop('Wrong kind of object has been given!')
  }
  f1_scores = 2*(pr_object$pres * pr_object$recall)/(pr_object$pres + pr_object$recall+0.00001)
  
  return(list(F1=f1_scores, F1sum=sum(f1_scores)))  
}
