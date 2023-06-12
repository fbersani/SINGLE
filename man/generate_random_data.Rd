\name{generate_random_data}
\alias{generate_random_data}
\title{
Generate a random dynamic graph using Vector Autoregressive (VAR) processes
}
\description{
This function generates random data with a dynamic correlation structure. Given the number of changepoints, the correlation structure within each segment of data (i.e. between changepoints) is randomly simulated using Erdos-Renyi random graphs. 
}
\usage{
generate_random_data(ROI, length_, mode='ER', seg, sparsity, str = 0.6)
}
\arguments{
  \item{ROI}{
Number of regoins of interest (i.e., number of nodes in the simulated graph)
}
  \item{length_}{
The length of each segment of data between changepoints
}
  \item{mode}{
The type of random network to generate. Options are 'ER' for Erdos-Renyi or 'BA' for Barabasi-Albert scale-free networks.
}
  \item{seg}{
Number of data segments (each with its own randomly sampled correlation structure)
}
  \item{sparsity}{
Sparsity parameter when simulating Erdos-Renyi random graphs. This is the probability of an edge occuring between each pair of nodes. Should be between 0 and 1.
}
  \item{str}{
The strength of the correlation between nodes. This should be be between -1 and 1.
We note that in the case of Erdos-Renyi random graphs all edges between nodes will have a strength of str. However, in the case of Barabasi-Albert scale-free networks
the strength of each edge will follow a uniform distribution on \eqn{[-str, \frac{str}{2}]\cup [\frac{str}{2}, str]}. 
}
}
\value{
\item{data}{A matrix containing the simulated data}
\item{true_cov}{An array where \code{true_cov[,,i]} contains the adjacency matrix corresponding to the true correlation structure at the ith observation}
}
\author{
Ricardo Pio Monti
}


\seealso{
\code{\link{SINGLE}, \link{plotSINGLE}, \link{precision_recall}, \link{choose_h}, \link{get_kern_cov}}
}
\examples{
set.seed(1)
X = generate_random_data(ROI=10, length_=100, seg=3, sparsity=0.1, str=0.6)
\dontrun{
S = SINGLE(data=X$data, h=50, l1=0.7, l2=0.5)
precision_recall(true_cov=X$true_cov, estimated_cov=S$P_)
}
}

