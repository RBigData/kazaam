#' ranshaq
#' 
#' Generate a random shaq object.
#' 
#' @details
#' If 
#' 
#' @section Communication:
#' The operation is entirely local.
#' 
#' @param generator
#' A function, such as \code{runif()} or \code{rnorm()} (passed without parens).
#' See examples for a demonstration of usage.
#' @param nrows,ncols
#' The number of rows 
#' @param local
#' Is the problem size \code{nrows*ncols} specifying the local or global problem
#' size?
#' @param ...
#' Additional arguments passed to the generator.
#' 
#' @return
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' 
#' # a 10x3 shaq with random uniform data
#' x = ranshaq(runif, 10, 3)
#' x
#' 
#' # a (comm.size() * 10)x3 shaq with random normal data
#' y = ranshaq(rnorm, 10, 3, local=TRUE)
#' y
#' 
#' finalize()
#' }
#' 
#' @export
ranshaq = function(generator, nrows, ncols, local=FALSE, ...)
{
  if (!missing(nrows))
    check.is.posint(nrows)
  if (!missing(ncols))
    check.is.posint(ncols)
  check.is.flag(local)
  
  
  if (missing(nrows) && missing(ncols))
  {
    s = shaq(matrix(nrow=0, ncol=0), 0, 0, checks=FALSE)
    return(s)
  }
  else if (missing(ncols))
    ncols = 1L
  else if (missing(nrows))
    nrows = 1L
  
  
  if (local)
  {
    nrows.local = nrows
    nrows = nrows * comm.size()
  }
  else
  {
    size = comm.size()
    nrows.local = as.integer(nrows %/% size)
    base = as.integer(nrows %/% size)
    rem = as.integer(nrows - nrows.local*size)
    if (comm.rank()+1 <= rem)
      nrows.local = nrows.local + 1L
  }
  
  Data = generator(nrows.local*ncols, ...)
  dim(Data) <- c(nrows.local, ncols)
  
  shaq(Data, nrows, ncols, checks=FALSE)
}
