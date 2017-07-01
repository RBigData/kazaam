#' Matrix Multiplication
#' 
#' Conceptually, this computes \code{t(x) \%*\% x} for a shaq \code{x}.
#' 
#' @section Communication:
#' The operation consists of a local crossproduct, followed by an
#' \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' A shaq.
#' @param y
#' Must be \code{NULL}.
#' 
#' @return 
#' A regular matrix.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' cp = crossprod(x)
#' comm.print(cp)
#' 
#' finalize()
#' }
#' 
#' @name crossprod
#' @rdname crossprod
NULL



cp.shaq = function(x, y = NULL)
{
  if (!is.null(y))
    comm.stop("only implemented for y=NULL")
  
  cp.local = base::crossprod(Data(x))
  allreduce(cp.local)
}



#' @rdname crossprod
#' @export
setMethod("crossprod", signature(x="shaq"), cp.shaq)
