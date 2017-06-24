#' ranshaq
#' 
#' ranshaq
#' 
#' @param generator
#' 
#' @param nrows,ncols
#' 
#' @param local
#' 
#' @param ...
#' Additional arguments passed to the generator.
#' 
#' @return
#' A shaq.
#' 
#' @export
ranshaq = function(generator, nrows, ncols, local=FALSE, ...)
{
  check.is.posint(nrows)
  check.is.posint(ncols)
  check.is.flag(local)
  
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
