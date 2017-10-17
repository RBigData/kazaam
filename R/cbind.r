#' cbind
#' 
#' Column binding for shaqs.
#' 
#' @details
#' All shaqs should have the same number of rows.  Additionally, all shaqs
#' should be distributed in identical fashion.
#' 
#' @section Communication:
#' The operation is completely local.
#' 
#' @param ...
#' A collection of shaqs.
#' @param deparse.level
#' Ignored.
#' 
#' @return
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' y = ranshaq(runif, 10, 1)
#' 
#' cbind(x, y)
#' 
#' finalize()
#' }
#' 
#' @export
cbind.shaq = function(..., deparse.level=1)
{
  args = list(...)
  
  checks = sapply(args, is.shaq)
  if (!all(checks))
    comm.stop("")
  
  nrows = nrow(args[[1]])
  ncols = sum(sapply(args, ncol))
  nrows.local = nrow(DATA(args[[1]]))
  ncols.local = sapply(args, function(x) ncol(DATA(x)))
  Data = matrix(0, nrows.local, sum(ncols.local))
  
  ncols.local = c(0L, cumsum(ncols.local))
  for (i in 1:length(args))
    Data[, ncols.local[i] + (1:ncol(DATA(args[[i]])))] = DATA(args[[i]])
  
  shaq(Data, nrows, ncols, checks=FALSE)
}
