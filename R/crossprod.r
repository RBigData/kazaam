cp.shaq = function(x, y = NULL)
{
  cp.local = crossprod(x@Data)
  allreduce(cp.local)
}



#' @export
setMethod("crossprod", signature(x="shaq", y="NULL"), cp.shaq)
