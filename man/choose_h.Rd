\name{choose_h}
\alias{choose_h}
\title{
Estimate kernel width for Gaussian kernel and Sliding window
}
\description{
Estimate the optimal width of kernel/sliding window using look-ahead log-likelihood
}
\usage{
choose_h(data, sample_size, kernel = "gaussian", h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data used to estimate optimal width. Each row should be an observation.
}
  \item{sample_size}{
Number of samples to use when computing the look-ahead log-likelihood. This is done in order to reduce the computational cost. 
In order to use all samples simply set sample_size = nrow(data).
}
  \item{kernel}{
Type of kernel to use. Options are "gaussian" for Gaussian kernel and "window" for sliding window.
}
  \item{h}{
A vector of potential kernel widths.
}
}

\details{
We estimate the optimal value of \eqn{h} by looking to maximise the look-ahead log-likelihood. This is defined for the ith observation as follows:
\deqn{
\mathcal{L}_{-1}(i;h) =  -\frac{1}{2} \mbox{ log det }(S_{i-1}) - \frac{1}{2} (X_i - \mu_{i-1})^T S_{i-1}^{-1}(X_i - \mu_{i-1})
}
We note that both \eqn{\mu_{i-1}} and \eqn{S_{i-1}} are estimated with 
the ith observation removed. It follows that calculating \eqn{\mathcal{L}_{-1}(h)} for all observations is computationally expensive, thus in order to save computational
effort we sample a subset of observations \eqn{R \subseteq \{2,\ldots,T\}}. The optimal value of \eqn{h} can then be estimated by choosing \eqn{h} to maximise the following score function:
\deqn{
CV(h) = \sum_{i \in R} \mathcal{L}_{-1}(i;h),
}
where in order to reduce the variability of estimated look-ahead log-likelihoods we use the same sample \eqn{R} to calculate each \eqn{CV(h)}.
}
\value{
Optimal value of \eqn{h} is returned
}
\references{
Monti R. P., Hellyer P., Sharp D., Leech R., Anagnostopoulos C. and Montana G. (2013) Estimating Dynamic Brain Connectivity Networks from Functional MRI Time Series
}
\author{
Ricardo Pio Monti
}

\seealso{
\code{\link{SINGLE}, \link{generate_random_data}, \link{get_kern_cov}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.15, str=0.6)
h_est = choose_h(data=X$data, sample_size=30, kernel='window', h=seq(10,100,10))
C = get_kern_cov(data=X$data, h=h_est, kernel="window")
}

\keyword{ kernel }
