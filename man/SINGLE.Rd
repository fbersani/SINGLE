\name{SINGLE}
\alias{SINGLE}
\title{
Smooth Incremental Graphical Lasso Estimation (SINGLE)
}
\description{
Estimate sparse dynamic graphs using the Smooth Incremental Graphical Lasso Estimation (SINGLE) algorithm
}
\usage{
SINGLE(data, C, h, l1, l2, kernel="gaussian", tol = 0.01, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Matrix of observations where each row is a vector of observations.
}
  \item{C}{
  (Optional) Array of estimated sample covariance matrices. If provided then these are used directly in the SINGLE algorithm. Format should be such that \code{C[,,i]} is the estimated
  sample covariance matrix at the ith observation.
  Alternatively, if \code{C} is not provided then sample covariance matrices will be estimated from \code{data}.
}
  \item{h}{
  (Optional) Width of the Gaussian kernel. \code{h} can either be numeric or a vector. If \code{h} is numeric then this value will be used to estimate sample covariance matrices. If a 
  vector is provided then the optimal \code{h} will be estimated using the \code{choose_h} function. If \code{h} is missing, the optimal value will be estimated using \code{choose_h} function
  for some values based on the dimensions of \code{data}. Finally, if \code{C} is provided, this will be ignored.
}
  \item{l1}{
  (Optional) Value for the \eqn{\lambda_1} penalty. \code{l1} can either be numeric or a vector. If \code{l1} is numeric then this value will be used directly. If a 
  vector is provided then the optimal \code{l1} will be estimated by minising AIC. If \code{l1} is missing, the optimal value will be estimated over a range of values.
}
  \item{l2}{
  (Optional) Value for the \eqn{\lambda_2} penalty. \code{l2} can either be numeric or a vector. If \code{l2} is numeric then this value will be used directly. If a 
  vector is provided then the optimal \code{l2} will be estimated by minising AIC. If \code{l2} is missing, the optimal value will be estimated over a range of values.
}
  \item{kernel}{
  (Optional) Choice of kernel. Can be either "gaussian" for a Gaussian kernel or "window" for a sliding window. If omitted a Gaussian kernel will be used. 
}
  \item{tol}{
  Threshold for convergence.
}
  \item{verbose}{
  Flag for printing out information as iterations proceed.
}

}
\details{
The SINGLE algorithm minimises the following objective function:
\deqn{
 f(\{ \Theta \}) =  \sum_{i=1}^T  [ -log det \hat \Theta_i + trace ( S_i \Theta_i) ] + \lambda_1 \sum_{i=1}^T || \Theta_i||_1 + \lambda_2 \sum_{i=2}^T || \Theta_i -  \Theta_{i-1}||_1
}
where \eqn{\{  \Theta \}} is the set of all precision matrices indexed over time.
}
\value{
\item{P}{List where the ith element is the ith estimated precision matrix}
\item{P_}{An array where \code{P_[,,i]} is the estimated precision matrix for the ith observation}
\item{C_}{List where the ith element is the ith estimated covariance matrix}
\item{l1}{\eqn{\lambda_1} parameter used for final fit. If \code{l1} not given this is the estimated \eqn{\lambda_1} value.}
\item{l2}{\eqn{\lambda_2} parameter used for final fit. If \code{l2} not given this is the estimated \eqn{\lambda_2} value.}
\item{h}{Kernel width parameter used for final fit. If \code{h} not given this is the estimated kernel width.}
\item{AIC}{AIC for final model based on parameters \eqn{\lambda_1}, \eqn{\lambda_2} and h.}
}

\references{
Monti R. P., Hellyer P., Sharp D., Leech R., Anagnostopoulos C. and Montana G. (2013) Estimating Dynamic Brain Connectivity Networks from Functional MRI Time Series
}
\author{
Ricardo Pio Monti
}\seealso{
\code{\link{plotSINGLE}, \link{generate_random_data}, \link{precision_recall}, \link{choose_h}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.1, str=-0.6)
\dontrun{
S = SINGLE(data=X$data, verbose=TRUE)
plotSINGLE(S, index=c(1,2,3,4,5), fix.axis=T)
}
}
\keyword{ SINGLE }