colsum.shaq = function(x, na.rm = FALSE, dims = 1L)
{
  cs = base::colSums(x@Data, na.rm=na.rm)
  allreduce(cs)
}



colmean.shaq = function(x, na.rm = FALSE, dims = 1L)
{
  cs = base::colSums(x@Data, na.rm=na.rm)
  allreduce(cs) / x@nrows
}



#' @export
setMethod("colSums", signature(x="shaq"), colsum.shaq)

#' @export
setMethod("colMeans", signature(x="shaq"), colmean.shaq)
