cp.shaq = function(x, y = NULL)
{
  if (!is.null(y))
    comm.stop("only implemented for y=NULL")
  
  cp.local = crossprod(x@Data)
  allreduce(cp.local)
}



# #' @export
# setGeneric(name="crossprod", useAsDefault=base::crossprod, package="pbdSHAQ")

#' @export
setMethod("crossprod", signature(x="shaq"), cp.shaq)
