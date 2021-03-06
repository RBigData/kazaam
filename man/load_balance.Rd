% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance.r
\name{load_balance}
\alias{load_balance}
\alias{balance.info}
\alias{.balance}
\alias{unbalance}
\alias{balance}
\title{Load Balancing}
\usage{
balance.info(X.gbd, comm = 0, gbd.major = 1L, method = "llb")

.balance(X.gbd, bal.info = NULL, gbd.major = 1L)

unbalance(x, bal.info)

balance(x, method = "llb")
}
\arguments{
\item{X.gbd}{A matrix.}

\item{comm}{An MPI communicator number.}

\item{gbd.major}{1 for row-major storage, 2 for column-major.}

\item{method}{One of "llb" (locally load balanced) "bc" (block-cyclic).}

\item{bal.info}{A returned object from \code{balance.info()}.}

\item{x}{A shaq.}
}
\description{
Re-distribute a shaq so that the data is "balanced" (in one of two ways; see
Details section for more information). This can be useful when, for example,
reading data onto multiple MPI ranks from a parallel file system, and then
broadcasting/redistributing the data out to the other ranks.
}
\details{
There are two separate interfaces. The functions \code{balance()} and
\code{unbalance()} are the "high level" interface, and are appropriate for
most use cases. However, for custom redistributions, the functions
\code{balance.info()} and \code{.balance()} can be used. Use at your own
risk.

\code{balance()} will rearrange the data distributed across the MPI ranks in
one of two ways. The first is in a "locally load balanced" (llb) way. This
means that each rank is given roughly the same amount of data. For example,
balancing 6 rows across 4 MPI ranks would leave 2 rows on ranks 0 and 1 and
1 row on ranks 3 and 4. The other method of distribution is "block-cyclic"
(bc). This will put the data into a 1-dimensional block-cyclic distribution
so that the data could be handed off to pbdDMAT. See that package's
documentation for more details.
}
\examples{
\dontrun{
# Run with 4 MPI ranks
suppressMessages(library(kazaam, quietly=TRUE))
comm.set.seed(1234, diff=TRUE)

n = sample(10, size=1)
d = matrix(comm.rank(), n, 3)

x = shaq(d)
x

y = balance(x)
y

finalize()
}

}
\author{
Wei-Chen Chen and Drew Schmidt
}
