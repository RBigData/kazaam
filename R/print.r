print_shaq = function(x, ...)
{
  if (comm.rank() == 0)
  {
    cat(paste0("# A shaq: ", x@nrows, "x", x@ncols, " on ", comm.size(), " MPI ranks\n"))
    print(head(Data(x)))
    cat("# ...\n")
  }
}



#' print
#' 
#' Print method for a shaq.
#' 
#' @param x,object
#' A shaq.
#' @param ...
#' Ignored
#' 
#' @name print
#' @rdname print
NULL

#' @rdname print
#' @export
setMethod("print", signature(x="shaq"), print_shaq)

#' @rdname print
#' @export
setMethod("show", signature(object="shaq"), function(object) print_shaq(object))
