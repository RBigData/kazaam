matmult.shaq = function(x, y)
{
  if (x@ncols != NROW(y))
    comm.stop("non-conformable arguments")
  
  ret = x
  ret@ncols = NCOL(y)
  ret@Data = x@Data %*% y
  
  ret
}



#' @export
setMethod("%*%", signature(x="shaq", y="matrix"), matmult.shaq)
