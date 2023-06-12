\name{SINGLE-package}
\alias{SINGLE-package}
\docType{package}
\title{
Estimate dynamic networks using the Smooth Incremental Graphical Lasso Estimation (SINGLE) Algorithm
}
\description{
This package provides an implementation of the Smooth Incremental Graphical Lasso Estimation (SINGLE) Algorithm which can be used to accurately estimate dynamic networks from non-stationary time series data. 
}
\details{
\tabular{ll}{
Package: \tab SINGLE\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-10-13\cr
License: \tab  GPL-2\cr
}
The SINGLE algorithm is indeed 
}
\author{
Ricardo Pio Monti, Christoforos Anagnostopoulos and Giovanni Montana

Maintainer: Ricardo Pio Monti <ricardo.monti08@gmail.com>
}
\references{
Monti R. P., Hellyer P., Sharp D., Leech R., Anagnostopoulos C. and Montana G. (2013) Estimating Dynamic Brain Connectivity Networks from Functional MRI Time Series
}
\seealso{
\code{\link{SINGLE}, \link{plotSINGLE}, \link{generate_random_data}, \link{precision_recall}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.15, str=0.6)
\dontrun{
S = SINGLE(data=X$data, radius=5, l1=0.75, l2=0.3, k=3)
result = precision_recall(true_cov=X$true_cov, estimated_cov=S$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
   main='F1 Score', xlab='Time') # plot F1 score over time
}
}