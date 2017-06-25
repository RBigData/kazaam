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
#' @section Communication:
#' If \code{checks=TRUE}, a check on the global number of rows is performed.
#' This amounts to an allgather operation on a logical value (the local
#' dimension check).
#' 
#' @param Data
#' The local submatrix.
#' @param nrows,ncols
#' The GLOBAL number of rows and columns.
#' @param checks
#' Logical. Should some basic dimension checks be performed?  Note that these
#' require communication, and with many MPI ranks, could be expensive.
#' 
#' @seealso
#' \code{\link{shaq-class}}
#' 
#' @name shaq
#' @rdname shaq
#' @export
shaq <- function (Data, nrows, ncols, checks=TRUE)
{
  UseMethod("shaq", Data)
}



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
shaq.matrix = function(Data, nrows, ncols, checks=TRUE)
{
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




#' @rdname shaq
#' @export
shaq.numeric = function(Data, nrows, ncols, checks=TRUE)
{
  if (!missing(nrows))
    check.is.natnum(nrows)
  if (!missing(ncols))
    check.is.natnum(ncols)
  check.is.flag(checks)
  
  if (missing(nrows) || missing(ncols))
  {
    if (missing(nrows) && missing(ncols))
    {
      nrows = length(Data)
      ncols = 1L
    }
    else if (missing(nrows))
      nrows = 1L
    else if (missing(ncols))
      ncols = 1L
  }
  
  
  size = comm.size()
  base = nrows %/% size
  rem = nrows - base*size
  nrows.local = base
  if (comm.rank()+1L < rem)
    nrows.local = nrows.local + 1
  
  Data = matrix(Data, as.integer(nrows.local), ncols)
  
  new("shaq", Data=Data, nrows=nrows, ncols=ncols)
}
