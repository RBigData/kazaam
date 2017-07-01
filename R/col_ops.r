#' Column Operations
#' 
#' Column operations (currently sums/means) for shaq objects.
#' 
#' @section Communication:
#' The operation consists of a local column sum operation, followed by an
#' \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' A shaq.
#' @param na.rm
#' Should \code{NA}'s be removed?
#' @param dims
#' Ignored.
#' 
#' @return
#' A regular vector.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' cs = colSums(x)
#' comm.print(cs)
#' 
#' finalize()
#' }
#' 
#' @name col_ops
#' @rdname col_ops
NULL



colsum.shaq = function(x, na.rm = FALSE, dims = 1L)
{
  check.is.flag(na.rm)
  
  cs = base::colSums(Data(x), na.rm=na.rm)
  allreduce(cs)
}

colmean.shaq = function(x, na.rm = FALSE, dims = 1L)
{
  check.is.flag(na.rm)
  
  cs = base::colSums(Data(x), na.rm=na.rm)
  allreduce(cs) / nrow(x)
}



#' @rdname col_ops
#' @export
setMethod("colSums", signature(x="shaq"), colsum.shaq)

#' @rdname col_ops
#' @export
setMethod("colMeans", signature(x="shaq"), colmean.shaq)
