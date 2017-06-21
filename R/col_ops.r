colsum.shaq = function(x, na.rm = FALSE, dims = 1L)
{
  cs = base::colSums(Data(x), na.rm=na.rm)
  allreduce(cs)
}



colmean.shaq = function(x, na.rm = FALSE, dims = 1L)
{
  cs = base::colSums(Data(x), na.rm=na.rm)
  allreduce(cs) / nrow(x)
}



#' @export
setMethod("colSums", signature(x="shaq"), colsum.shaq)

#' @export
setMethod("colMeans", signature(x="shaq"), colmean.shaq)
