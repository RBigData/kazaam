#' Tall Matrices
#' 
#' Many data science problems reduce to operations on very tall,
#' skinny matrices.  However, sometimes these matrices can be so tall that they
#' are difficult to work with, or do not even fit into main memory.  One
#' strategy to deal with such objects is to distribute their rows across
#' several processors.  To this end, we offer an 'S4' class for tall, skinny,
#' distributed matrices, called the 'shaq'.  We also provide many useful
#' numerical methods and statistics operations for operating on these
#' distributed objects.  The naming is a bit "tongue-in-cheek", with the class
#' a play on the fact that 'Shaquille' 'ONeal' ('Shaq') is very tall, and he
#' starred in the film 'Kazaam'.
#' 
#' @import pbdMPI
#' @import methods
#' @importFrom stats prcomp optim runif
#' @importFrom float float32 is.float
#' 
#' @name kazaam-package
#' @docType package
#' @references Programming with Big Data in R Website: \url{https://pbdr.org/}
#' @keywords Package
NULL
