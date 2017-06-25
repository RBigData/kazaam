#' Covariance and Correlation
#' 
#' Covariance and (pearson) correlation.
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
#' TODO
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
  
  crossprod(scale(x, TRUE, FALSE)) / (nrow(x) - 1)
}

cor.shaq = function (x, y=NULL, use="everything", method="pearson")
{
  covcor.check.y(y)
  
  crossprod(scale(x, TRUE, TRUE)) / (nrow(x) - 1)
}



#' @rdname cov
#' @export
setMethod("cov", signature(x="shaq"), cov.shaq)

#' @rdname cov
#' @export
setMethod("cor", signature(x="shaq"), cor.shaq)
