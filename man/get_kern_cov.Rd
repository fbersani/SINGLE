\name{get_kern_cov}
\alias{get_kern_cov}
\title{
Estimate time varying sample covariance matrices using kernels
}
\description{
Estimate sample covariance matrices using Guassian kernel or sliding window.
}
\usage{
get_kern_cov(data, h, kernel = "gaussian")
}
\arguments{
  \item{data}{
Matrix containing data from which to estimate time varying sample covariance matrices. Each row should contain an observation
}
  \item{h}{
Kernel width
}
  \item{kernel}{
Choice of kernel. Options are "gaussian" for Gaussian kernel and "window" for Sliding window.
}
}
\details{
The ith sample covariance matrix \eqn{S_i} is estimated as:
\deqn{
S_i = \frac{ \sum_{j=1}^T K_h(i, j) \cdot (X_j-\mu_j)^T (X_j-\mu_j)}{\sum_{j=1}^T K_h(i, j)}
}
}
\value{
A list where the ith element is the sample covariance at the ith observation
}
\author{
Ricardo Pio Monti
}

\seealso{
\code{\link{SINGLE}, \link{generate_random_data}, \link{choose_h}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.15, str=0.6)
C = get_kern_cov(data=X$data, h=50, kernel="gaussian")
\dontrun{
S = SINGLE(C=C, l1=0.7, l2=0.5)
plotSINGLE(S, index = c(1,2,3,4,5), x.axis=seq(1,150), 
    col.names=seq(1,5), n.row=2, fix.axis=TRUE)
}
}
\keyword{ kernel }
