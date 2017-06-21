#' Class shaq
#' 
#' Distributed tall matrix class.
#' 
#' @slot DATA
#' The local submatrix.
#' @slot nrows, ncols
#' The global matrix dimension.
#' 
#' @name shaq-class
#' @docType class
setClass(
  Class="shaq", 
  representation=representation(
    Data="matrix",
    nrows="numeric",
    ncols="numeric"
  ),
  
  prototype=prototype(
    Data=matrix(nrow=0, ncol=0),
    nrows=0,
    ncols=0
  )
)
