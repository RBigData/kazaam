#' Tall Matrices
#' 
#' Many data science problems reduce to operations on very tall,
#' skinny matrices.  However, sometimes these matrices can be so tall that they
#' are difficult to work with, or do not even fit into main memory.  One
#' strategy to deal with such objects is to distribute their rows across
#' several processors.  To this end, we offer a container for tall, skinny,
#' distributed matrices ('shaqs') in the form of an 'S4' class.  We also
#' provide many useful numerical methods for operating on these distributed
#' objects.
#' 
#' This package requires an MPI library (OpenMPI, MPICH2, or LAM/MPI).
#' 
#' @import pbdMPI
#' @import methods
#' @importFrom stats prcomp
#' 
#' @name pbdSHAQ-package
#' @docType package
#' @author Drew Schmidt \email{wrathematics@gmail.com}, Wei-Chen Chen, Mike Matheson, and George Ostrouchov.
#' @references Programming with Big Data in R Website: \url{http://r-pbd.org/}
#' @keywords Package
NULL
