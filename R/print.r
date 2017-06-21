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



print_shaq = function(x)
{
  if (comm.rank() == 0)
  {
    cat(paste0("# A shaq: ", nrow(x), "x", ncol(x), " on ", comm.size(), " MPI ranks\n"))
    
    toprow = min(10, NROW(Data(x)))
    topcol = min(6, NCOL(Data(x)))
    submat = Data(x)[1:toprow, 1:topcol]
    print(submat)
    
    cat("# ...\n\n")
  }
}



#' @rdname print
#' @export
setMethod("print", signature(x="shaq"), function(x, ...) print_shaq(x))

#' @rdname print
#' @export
setMethod("show", signature(object="shaq"), function(object) print_shaq(object))
