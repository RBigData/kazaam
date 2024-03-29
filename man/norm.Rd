% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norm.r
\name{norm}
\alias{norm}
\alias{norm,shaq,ANY-method}
\title{norm}
\usage{
\S4method{norm}{shaq,ANY}(x, type = c("O", "I", "F", "M", "2"))
}
\arguments{
\item{x}{A shaq}

\item{type}{The type of norm: one, infinity, frobenius, max-modulus, and spectral.}
}
\value{
A number (length 1 regular vector).
}
\description{
Implementation of R's \code{norm()} function for shaq objects.
}
\details{
If \code{type == "O"} then the norm is calculated as the maximum of the
column sums.

If \code{type == "I"} then the norm is calculated as the maximum absolute
value of the row sums.

If \code{type == "F"} then the norm is calculated as the square root of the
sum of the square of the values of the matrix.

If \code{type == "M"} then the norm is calculated as the max of the absolute
value of the values of the matrix.

If \code{type == "2"} then the norm is calculated as the largest singular
value.
}
\section{Communication}{

If \code{type == "O"} then the communication consists of an allreduce,
quadratic on the number of columns.

If \code{type == "I"} then the communication conists of an allgather.

If \code{type == "F"} then the communication is an allreduce, quadratic on
the number of columns.

If \code{type == "M"} then the communication consists of an allgather.

If \code{type == "2"} then the communication consists of the same as that of
an \code{svd()} call:  an allreduce, quadratic on the number of columns.
}

\examples{
\dontrun{
library(kazaam)
x = ranshaq(runif, 10, 3)

nm = norm(x)
comm.print(nm)

finalize()
}

}
