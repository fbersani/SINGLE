fill_in_edges <-
function(adj){
  # fill in induced edges
  for (i in 1:nrow(adj)){
    index = which(adj[i,]!=0)
    if (length(index)>0){
      for (j in 1:length(index)){
        adj[index[j], index[-j]] =  adj[index[-j], index[j]] = 1
      }
    }
  }
  return(adj)  
}
