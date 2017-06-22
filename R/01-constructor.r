#' shaq
#' 
#' Constructor for shaq objects.
#' 
#' @param Data
#' The local submatrix.
#' @param nrows,ncols
#' The GLOBAL number of rows and columns.
#' @param checks
#' Logical. Should some basic dimension checks be performed?  Note that these
#' require communication, and with many MPI ranks, could be expensive.
#' 
#' @name shaq
#' @rdname shaq
NULL



check.shaq = function(Data, nrows, ncols)
{
  colcheck = comm.all(NCOL(Data) == ncols)
  if (!isTRUE(colcheck))
    comm.stop("local column dimensions disagree across ranks")
  
  rowcheck = allreduce(NROW(Data))
  nrows = rowcheck
  
  return(nrows)
}



#' @rdname shaq
#' @export
shaq = function(Data=matrix(nrow=0, ncol=0), nrows=0, ncols=0, checks=TRUE)
{
  check.is.matrix(Data)
  check.is.natnum(nrows)
  check.is.natnum(ncols)
  check.is.flag(checks)
  
  if (checks)
    nrows = check.shaq(Data, nrows, ncols)
  
  new("shaq", Data=Data, nrows=nrows, ncols=ncols)
}
