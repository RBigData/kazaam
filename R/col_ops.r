#' Column Operations
#' 
#' Column operations (currently sums/means) for shaq objects.
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
#' TODO
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
