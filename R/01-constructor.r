#' shaq
#' 
#' Constructor for shaq objects.
#' 
#' @param Data
#' The local submatrix.
#' @param nrows, ncols
#' The GLOBAL number of rows and columns.
#' @param checks
#' Logical. Should some basic dimension checks be performed?  Note that these
#' require communication, and with many MPI ranks, could be expensive.
#' 
#' @export
shaq = function(Data, nrows, ncols, checks=TRUE)
{
  if (!is.matrix(Data))
    comm.stop("argument 'Data' must be a matrix")
  check.is.natnum(nrows)
  check.is.natnum(ncols)
  check.is.flag(checks)
  
  if (checks)
    nrows = check.shaq(Data, nrows, ncols)
  
  new("shaq", Data=Data, nrows=nrows, ncols=ncols)
}



check.shaq = function(Data, nrows, ncols)
{
  colcheck = comm.all(NCOL(Data) == ncols)
  if (!isTRUE(colcheck))
    comm.stop("local column dimensions disagree across ranks")
  
  rowcheck = allreduce(NROW(Data))
  nrows = rowcheck
  
  return(nrows)
}
