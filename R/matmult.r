matmult.shaq = function(x, y)
{
  if (x@ncols != NROW(y))
    comm.stop("non-conformable arguments")
  
  nrows = x@nrows
  ncols = NCOL(y)
  Data = x@Data %*% y
  
  shaq(Data, nrows, ncols, checks=FALSE)
}



#' @export
setMethod("%*%", signature(x="shaq", y="matrix"), matmult.shaq)
