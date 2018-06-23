#' Covariance and Correlation
#' 
#' Covariance and (pearson) correlation.
#' 
#' @section Communication:
#' The operation is completely local except for forming the crossproduct, which
#' is an \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' A shaq.
#' @param y
#' At this time, this must be \code{NULL}.
#' @param use 
#' NA handling rules, as with R's cov/cor functions.  At this time, only
#' "everything" is supported.
#' @param method 
#' The cov/cor method. Currently only "pearson" is available.
#' 
#' @return 
#' A regular matrix.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' cov(x)
#' cor(x)
#' 
#' finalize()
#' }
#' 
#' @name cov
#' @rdname cov
NULL



covcor.check.y = function(y)
{
  if (!is.null(y))
    comm.stop("only supported when argument 'y' is NULL")
}



cov.shaq = function (x, y=NULL, use="everything", method="pearson")
{
  covcor.check.y(y)
  
  cp.internal(scale(x, TRUE, FALSE), 1.0/(nrow(x) - 1L))
}

cor.shaq = function (x, y=NULL, use="everything", method="pearson")
{
  covcor.check.y(y)
  
  cp.internal(scale(x, TRUE, TRUE), 1.0/(nrow(x) - 1L))
}



#' @rdname cov
#' @export
setMethod("cov", signature(x="shaq"), cov.shaq)

#' @rdname cov
#' @export
setMethod("cor", signature(x="shaq"), cor.shaq)
