#' Matrix Multiplication
#' 
#' Conceptually, this computes \code{t(x) \%*\% x} for a shaq \code{x}.
#' 
#' @param x
#' A shaq.
#' @param y
#' Must be \code{NULL}.
#' 
#' @return 
#' A regular matrix.
#' 
#' @name crossprod
#' @rdname crossprod
NULL



cp.shaq = function(x, y = NULL)
{
  if (!is.null(y))
    comm.stop("only implemented for y=NULL")
  
  cp.local = crossprod(Data(x))
  allreduce(cp.local)
}



#' @rdname crossprod
#' @export
setMethod("crossprod", signature(x="shaq"), cp.shaq)
