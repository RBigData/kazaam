#' shaq
#' 
#' Constructor for shaq objects.
#' 
#' @details
#' If \code{nrows} and/or \code{ncols} is missing, then it will be imputed.
#' This means one must be especially careful to manually provide \code{ncols}
#' if some of ranks have "placeholder data" (a 0x0 matrix), which is typical
#' when reading from a subset of processors and then broadcasting out to the
#' remainder.
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



intuit.shaq.nrows = function(Data)
{
  rowcheck = allreduce(NROW(Data))
  rowcheck
}

check.shaq.ncols = function(Data, ncols)
{
  colcheck = comm.all(NCOL(Data) == ncols)
  if (!isTRUE(colcheck))
    comm.stop("local column dimensions disagree across ranks")
}

check.shaq = function(Data, nrows, ncols)
{
  check.shaq.ncols(Data, ncols)
  
  nrows = intuit.shaq.nrows(Data)
  
  return(nrows)
}



#' @rdname shaq
#' @export
shaq = function(Data=matrix(nrow=0, ncol=0), nrows, ncols, checks=TRUE)
{
  check.is.matrix(Data)
  if (!missing(nrows))
    check.is.natnum(nrows)
  if (!missing(ncols))
    check.is.natnum(ncols)
  check.is.flag(checks)
  
  if (missing(nrows) || missing(ncols))
  {
    if (missing(nrows))
      nrows = intuit.shaq.nrows(Data)
    
    if (missing(ncols))
      ncols = NCOL(Data)
  }
  else if (checks)
    nrows = check.shaq(Data, nrows, ncols)
  
  new("shaq", Data=Data, nrows=nrows, ncols=ncols)
}
