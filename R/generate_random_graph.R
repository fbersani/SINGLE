generate_random_graph <-
function(ROI, length_, mode = 'ER', sparsity, str=0.6){
  # this function generates 1 segment of graphically structured data using Erdos-Renyi random graphs
  # Input:
  #   - ROI is number of regions of interest (ie number of nodes)
  #   - length is the length of each segment
  #   - mode: nature of the simulated graph: can be ER = Erdos Renyi (default) or BA = barabasi albert model for scale free networks
  #   - sparsity is the level of sparsity
  #   - str is strength of correlation
  # Output:
  #   - data = VAR process of dimension ROI x length
  #   - true_cov = array with true graphical structure
  
  if (mode=='ER'){
    y = as.matrix(get.adjacency(erdos.renyi.game(ROI, sparsity), sparse=FALSE))
    y = fill_in_edges(y)
    y = y*str
    diag(y)=1    
  } else if (mode=='BA'){
    node_no = sample(seq(1, ROI), min(ceiling(ROI*ROI*0.5*sparsity), 10))
    dim_ = length(node_no)
    y = matrix(0, ncol=ROI, nrow=ROI)
    
    # simulate small world network:
    sub_matrix = as.matrix(get.adjacency(barabasi.game(length(node_no), power=1, directed=FALSE)))
    # simulate edge weights:
    W = matrix(0, ncol=ncol(sub_matrix), nrow=nrow(sub_matrix)) 
    weights = runif(dim_*(dim_-1)*0.5, str/2, str)
    W[lower.tri(W)] = weights
    W[upper.tri(W)] = weights
    sub_matrix = sub_matrix * W
    # simulate edge signs:
    S = matrix(0, ncol=ncol(sub_matrix), nrow=nrow(sub_matrix)) #  matrix of signs
    signs = sample(c(-1,1), dim_*(dim_-1)*0.5, replace=TRUE)
    S[lower.tri(S)] = signs
    S[upper.tri(S)] = signs
    sub_matrix = sub_matrix * S
    
    # ensure symetric positive definite:
    sub_matrix = ensure_pos_def(sub_matrix)
    
    y[node_no, node_no] = sub_matrix    
    diag(y)=1
  }
  
  
  if (sum(eigen(y)$values>=0)<ROI){
    while(sum(eigen(y)$values>=0)<ROI){
      if (mode=='ER'){
      	y = as.matrix(get.adjacency(erdos.renyi.game(ROI, sparsity)))
	      y = fill_in_edges(y)
      } else if (mode=='BA'){
      	# THIS SHOULD NOT HAPPEN AS I HAVE EXPLICITLY TAKEN STEPS TO AVOID THIS
        #cat('Albert Barabasi method\n')
#       	node_no = sample(seq(1, ROI), min(ceiling(ROI*ROI*0.5*sparsity), 10))
#       	y = matrix(0, ncol=ROI, nrow=ROI)
#       	y[node_no, node_no] = as.matrix(get.adjacency(barabasi.game(length(node_no), power=1, directed=FALSE)))
#         y = fill_in_edges(y)
      }
      
      y = y*str
      diag(y)=1
    }
  }
  true_cov = array(y, c(1,ROI,ROI))
  VAR = ARMA(A=true_cov, B=diag(ROI))
  data = simulate(VAR, sampleT=length_)$output
  
  return(list(data=data, true_cov=true_cov))  
}
