#' Matrix Multiplication
#' 
#' Multiplies two distributed matrices, if they are conformable.
#' 
#' @param x
#' A shaq.
#' @param y
#' A reguarl matrix.
#' 
#' @return 
#' A shaq.
#' 
#' @name matmult
#' @rdname matmult
NULL



matmult.shaq = function(x, y)
{
  if (ncol(x) != NROW(y))
    comm.stop("non-conformable arguments")
  
  nrows = ncol(x)
  ncols = NCOL(y)
  Data = Data(x) %*% y
  
  shaq(Data, nrows, ncols, checks=FALSE)
}



#' @rdname matmult
#' @export
setMethod("%*%", signature(x="shaq", y="matrix"), matmult.shaq)
