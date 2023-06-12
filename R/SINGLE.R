SINGLE <-
function(data, C, h, l1, l2, kernel="gaussian", tol=0.01, verbose=FALSE){
  # Implementation of the SINGLE algorithm
  # 
  # Input:
  #     - data: matrix where each row is an observation, each column refers to a ROI
  #     - C (optional): list of estimated covariance matrices. Must provide one of either data or C
  #     - h (optional): radius for the kernel
  #     - kernel: choice of kernel for sample covariance estimation. Can be either "gaussian" or "window"
  #     - l1 (optional): Glasso penalty coefficient. If l1 is just a unit entry then it is used. If l1 is a vector then the optimal choice is estimated by minimising AIC 
  #     - l2 (optional): is fused lasso penalty coefficient. Again, if l2 is just a unit entry then it is used. If l2 is a vector then the optimal choice is estimated by minimising AIC 
  #     - verbose: flag for printing output as iterations proceed.
  #
  # Output:
  #     - P: list of estimation precision matrices
  #     - P_: estimated precision matrices as array
  #     - C: estimated sample covariance matrices
  #     - l1: sparsity parameter 
  #     - l2: smoothness parameters
  #     - h: kernel width
  #     - AIC: estimated AIC values 
  
  # -- Check Input is Correct -- #
  
  if(missing(data)){
    stop("Missing data matrix")
  }
  
  # test with inherits inserted by Fbersani in 2023 to replace previous one class(data) != "matrix"
  # https://stackoverflow.com/questions/64660948/issue-with-creating-a-matrix-in-r
  
  if (!(inherits(data, "matrix"))){
    stop("data must be a matrix. See help(SINGLE) for details.")
  }
    
  if (!(kernel %in% c("gaussian", "window"))){
    stop('kernel must either be "gaussian" or "window". See help(SINGLE) for details.')
  }
  
  # -- Kernel Estimation of Covariance Matricies -- #  
  
  if (missing(C)){
    if (verbose==TRUE) cat("Covariance matrices not provided... will be estimated\n")
    if (missing(h)){
      if (verbose==TRUE) cat("Kernel width, h, not provided... will be estimated\n")
      h_seq = seq(ceiling(nrow(data)/10), ceiling(nrow(data)/2), length.out=10)
      h_seq = sapply(h_seq, ceiling)
      h = choose_h(data=data, sample_size=nrow(data)-1, kernel=kernel, h=h_seq)
    } else if (length(h)>1){
      if (class(h)!="numeric"){
        stop("h must be numeric. See help(SINGLE) for details.")
      }
      if (verbose==TRUE) cat("Sequence of h values provided... optimal value will be chosen\n")
      h = choose_h(data=data, sample_size=nrow(data)/10, kernel=kernel, h=h)
    }
    C = get_kern_cov(data=data, h=sqrt(h) * as.numeric(kernel=="gaussian") + h *as.numeric(kernel!="gaussian"))
  } else {
    if (class(C)!="array"){
      stop("If C is provided this must be an array. See help(SINGLE) for details.")
    }
    if (dim(C)[3]!=nrow(data) | dim(C)[1]!=ncol(data) | dim(C)[1]!=dim(C)[2]){
      stop("Incorrect dimensions for entries in C. See help(SINGLE) for details.")
    }
    
    if (verbose==TRUE) cat("Sample covariance matrices provided\n")
  }
  C_ = vector("list", nrow(data))
  for (i in 1:nrow(data)){ # this is incredibly inefficient but it only needs to be done once
    C_[[i]]=C[,,i]
  }
  
  # -- Estimate of Precision Matricies -- #
  if (missing(l1)){
    if (verbose==TRUE) cat("l1 parameter not provided... will be estimated\n")
    l1 = seq(quantile(C, .25), quantile(C,.5), length.out=5)
    if (sum(l1<0)>0){
      l1 = seq(quantile(abs(C), .25), quantile(abs(C),.5), length.out=5)
    }
  } else {
    if (class(l1)!="numeric"){
      stop("l1 must be numeric. See help(SINGLE) for details.")
    }
  }
  if (missing(l2)){
    if (verbose==TRUE) cat("l2 parameter not provided... will be estimated\n")
    l2 = seq(.1, .5, .1)
  } else {
    if (class(l2)!="numeric"){
      stop("l2 must be numeric. See help(SINGLE) for details.")
    }
  }
  
  # will need centered data in order to estimate AIC/BIC
  if (!missing(h)){
    data = data - get_kern_mean(data, h=h)
    # if h not given we just assume data is centered
  } else {
    h = NA # need to return something at the end of the function!
  }
  
  if (length(l1)>1 | length(l2)>1){
    # need to choose optimal l1 and l2 parameters
    AIC = matrix(0, ncol=length(l1), nrow=length(l2))
        
    if (verbose==TRUE) cat("Estimating optimal l1 and l2 parameters...\n")
    for (i in 1:length(l1)){
      for (j in 1:length(l2)){
#         cat(i,j, "\n")
        S = jgl_fgl_offline(S=C_, lambda1=l1[i], lambda2=l2[j], rho=1, obs=rep(1, length(C_)), max_iter=500, tol=tol)
        AIC[j,i] = get_aic(data, S$theta)
      }
    }
    
    loc = which(AIC==min(AIC))
    l1 = l1[ceiling(loc/length(l2))]
    l2 = l2[length(l2) - (loc %% length(l2))]
  }
  
  # now finally run on estimated or given l1,l2 values:
  result = jgl_fgl_offline(S=C_, lambda1=l1, lambda2=l2, rho=1, obs=rep(1, length(C_)), max_iter=500, tol=tol)
  precision = result$Z
  # make into an array:
  precision_ = array(unlist(precision), c(ncol(precision[[1]]), ncol(precision[[1]]), length(precision)))
  
  AIC = get_aic(data, result$theta)
  
  return(list(P=precision, P_=precision_, C=C_, AIC=AIC, l1=l1, l2=l2, h=h))
}



