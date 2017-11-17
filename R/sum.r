#' sum
#' 
#' Sums the elements of a shaq.
#' 
#' @details
#' TODO
#' 
#' @section Communication:
#' Communication consists of a an allreduce on a single value.
#' 
#' @param x
#' A shaq.
#' @param ...
#' Additionaly elements (shaqs or numeric vectors/matrices) to sum.  Regular
#' vectors/matrices are assumed to be global, but will only be included to the
#' sum on rank 0.
#' @param na.rm
#' should NA's be removed?
#'  
#' @return 
#' A single value.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' sum(x)
#' sum(x, 1)
#' 
#' finalize()
#' }
#' 
#' @name sum
#' @rdname sum
NULL



summer = function(x, na.rm)
{
  if (is.shaq(x))
    sum(DATA(x), na.rm=na.rm)
  else
  {
    if (comm.rank() == 0)
      sum(x, na.rm=na.rm)
    else
      0
  }
}

sum.shaq = function(x, ..., na.rm=FALSE)
{
  additional = list(...)
  if (length(additional) > 0)
  {
    s = sum(sapply(additional, summer, na.rm=na.rm))
  }
  else
    s = 0
  
  if (is.na(s))
    pbdMPI::allreduce(s)
  else
  {
    local = sum(DATA(x), na.rm=na.rm) + s
    pbdMPI::allreduce(local)
  }
}



#' @rdname sum
#' @export
setMethod("sum", signature(x="shaq"), sum.shaq)
