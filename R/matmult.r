#' Matrix Multiplication
#' 
#' Multiplies two distributed matrices, if they are conformable.
#' 
#' @details
#' The two shaqs must be distributed \emph{identically}.
#' 
#' @section Communication:
#' The operation is completely local.
#' 
#' @param x
#' A shaq.
#' @param y
#' A regular matrix, globally owned on all ranks.  Since the number of columns
#' of a shaq should be small, this matrix should be small as well.
#' 
#' @return 
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' y = matrix(1:9, 3, 3)
#' 
#' x %*% y
#' 
#' finalize()
#' }
#' 
#' @name matmult
#' @rdname matmult
NULL



matmult.shaq = function(x, y)
{
  if (ncol(x) != NROW(y))
    comm.stop("non-conformable arguments")
  
  nrows = nrow(x)
  ncols = NCOL(y)
  Data = Data(x) %*% y
  
  shaq(Data, nrows, ncols, checks=FALSE)
}



#' @rdname matmult
#' @export
setMethod("%*%", signature(x="shaq", y="matrix"), matmult.shaq)
