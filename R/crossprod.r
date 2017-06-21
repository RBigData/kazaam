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
