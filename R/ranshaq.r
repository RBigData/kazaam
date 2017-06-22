#' ranshaq
#' 
#' ranshaq
#' 
#' @param generator
#' 
#' @param nrows,ncols
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
    nrows.local = nrows / comm.size()
  
  Data = generator(nrows.local*ncols, ...)
  dim(Data) <- c(nrows.local, ncols)
  
  shaq(Data, nrows, ncols, checks=FALSE)
}
