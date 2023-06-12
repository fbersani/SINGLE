generate_random_data <-
function(ROI, length_, mode='ER', seg, sparsity, str=0.6){
  # This function generates random graphical structure 
  # Input:
  #   - ROI is number of regions of interest (ie number of nodes)
  #   - length_ is the length of each segment
  #   - mode: nature of the simulated graph: can be ER = Erdos Renyi (default) or BA = barabasi albert model for scale free networks
  #   - seg is number of segments 
  #   - sparsity is the level of sparsity
  # Output:
  #   - data = VAR process of dimension ROI x (length_*seg)
  #   - true_cov = array with true graphical structure
  
  # empty true correlation structure (fill this in)
  true_cov = true_cov = array(0, c(ROI,ROI, length_*seg))
  data = matrix(0, length_*seg, ROI)
  for (i in 1:seg){
    X = generate_random_graph(ROI=ROI, length_=length_, sparsity=sparsity, mode=mode, str=str)
    data[((i-1)*length_+1):(i*length_),] = X$data
    true_cov[,,((i-1)*length_+1):(i*length_)] = X$true_cov
  }
  
  return(list(data=data, true_cov=true_cov))  
  
}
