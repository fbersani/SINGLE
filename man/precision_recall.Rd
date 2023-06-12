\name{precision_recall}
\alias{precision_recall}
\title{
Calculate the Precision, Recall and F scores 
}
\description{
This function calculates the Precision, Recall and F scores by comparing the true graphical structure to the estimated graphical structure.
}
\usage{
precision_recall(true_cov, estimated_cov)
}
\arguments{
  \item{true_cov}{An array where \code{true_cov[,,i]} is the adjacency matrix of the true graphical structure at the ith observation}
  \item{estimated_cov}{An array where estimated_cov[,,i] is the adjacency matrix of the estimated graphical structure at the ith observation}
}
\details{
In our case, Precision is defined as the fraction of reported edges which are true edges and 
Recall is the fraction of edges which are correctly reported.

We calculate the F scores as \eqn{F_i= \frac{P_i \cdot R_i}{(P_i+R_i)}} where \eqn{P_i} and \eqn{R_i} are the precision and recall at 
the ith observation respectively.
}
\value{
\item{pres}{A vector where the ith entry is the precision of the estimated graph at the ith observation}
\item{recall}{A vector where the ith entry is the recall of the estimated graph at the ith observation}
\item{F1}{A vector where the ith entry is the F score of the estimate graph at the ith observation}
}
\author{
Ricardo Pio Monti
}

\seealso{
\code{\link{SINGLE}, \link{plotSINGLE}, \link{generate_random_data}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.1, str=0.6)
\dontrun{
S = SINGLE(data=X$data, h=50, l1=0.7, l2=0.5)
result = precision_recall(true_cov=X$true_cov, estimated_cov=S$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
   main='F1 Score', xlab='Time') # plot F1 score over time
}
}

