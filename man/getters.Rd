% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-getters.r
\name{getters}
\alias{getters}
\alias{nrow,gbd1d-method}
\alias{NROW,gbd1d-method}
\alias{nrow.local}
\alias{nrow.local,gbd1d-method}
\alias{ncol,gbd1d-method}
\alias{NCOL,gbd1d-method}
\alias{ncol.local}
\alias{ncol.local,gbd1d-method}
\alias{length,gbd1d-method}
\alias{DATA}
\alias{DATA,gbd1d-method}
\alias{dim,gbd1d-method}
\title{getters}
\usage{
\S4method{nrow}{gbd1d}(x)

\S4method{NROW}{gbd1d}(x)

nrow.local(x)

\S4method{nrow.local}{gbd1d}(x)

\S4method{ncol}{gbd1d}(x)

\S4method{NCOL}{gbd1d}(x)

ncol.local(x)

\S4method{ncol.local}{gbd1d}(x)

\S4method{length}{gbd1d}(x)

DATA(x)

\S4method{DATA}{gbd1d}(x)

\S4method{dim}{gbd1d}(x)
}
\arguments{
\item{x}{A shaq.}
}
\description{
Getters for shaq objects.
}
\details{
Functions to return the number of rows (\code{nrow()} and \code{NROW()}),
the number of columns (\code{ncol()} and \code{NCOL()}), the length - or
product of the number of rows and cols - (\code{length()}), and the local
submatrix (\code{DATA()}).
}
\section{Communication}{

Each operation is completely local.
}

\seealso{
\code{\link{setters}}
}
