\name{plotSINGLE}
\alias{plotSINGLE}
\title{
Plot pairwise partial correlations as estimated by the SINGLE algorithm
}
\description{
Plot pairwise partial correlations as estimated by the SINGLE algorithm
}
\usage{
plotSINGLE(object, index, x.axis, col.names, n.row, fix.axis = FALSE)
}
\arguments{
  \item{object}{
An object of class SINGLE.
}
  \item{index}{
Subset of indices for which we wish to plot partial correlations. If is.na(index) then all partial correlations are plotted, however this may be a bit crowded.
}
  \item{x.axis}{
X axis for partial correlation plots
}
  \item{col.names}{
Names for columns. This must match the index.
}
  \item{n.row}{
Will plot pairwise partial correlations on a grid, use this to determine number of rows on the grid.
}
  \item{fix.axis}{
Should axis on all plots be the same, this allows for easier comparison of partial correlations.
}
}
\details{
We note that given an estimate of a precision matrix, \eqn{\hat \Theta}, the partial correlation between the \eqn{i} and \eqn{j} nodes, \eqn{p_{i,j}}, will be
\deqn{p_{i,j} = -\frac{ \Theta_{ij}}{\sqrt{\Theta_{ii} \Theta_{jj}}}}
}

\author{
Ricardo Pio Monti
}

\seealso{
\code{\link{SINGLE}, \link{generate_random_data}, \link{precision_recall}}
}

\examples{
set.seed(1)
X = generate_random_data(ROI=5, length_=50, seg=3, sparsity=0.1, str=0.6)
\dontrun{
S = SINGLE(data=X$data, h=50, l1=0.7, l2=0.5)
plotSINGLE(S, index = c(1,2,3,4,5), x.axis=seq(1,150), 
    col.names=seq(1,5), n.row=2, fix.axis=TRUE)
}
}
\keyword{ SINGLE }

