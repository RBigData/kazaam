#' shaq
#' 
#' Constructor for shaq objects.
#' 
#' @details
#' If \code{nrows} and/or \code{ncols} is missing, then it will be imputed.
#' One can pass \code{NULL} for the \code{Data} argument to specify that that
#' MPI rank owns no data. In this case, you \emph{must} manually provide
#' \code{ncols}. This use case is typical when reading from a subset of
#' processors and then broadcasting out to the remainder.
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
#' \code{\link{shaq-class}} and \code{\link{load_balance}}
#' 
#' @name shaq
#' @rdname shaq
NULL

#' @rdname shaq
#' @export
shaq <- function(Data, nrows, ncols, checks=TRUE) UseMethod("shaq", Data)

#' @rdname shaq
#' @export
tshaq <- function(Data, nrows, ncols, checks=TRUE) UseMethod("tshaq", Data)



intuit.shaq.nrows = function(Data)
{
  allreduce(NROW(Data))
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



shaq.Mat = function(Data, nrows, ncols, checks=TRUE)
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
shaq.matrix = shaq.Mat

#' @rdname shaq
#' @export
shaq.float32 = shaq.Mat

#' @rdname shaq
#' @export
shaq.vector = function(Data, nrows, ncols, checks=TRUE)
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
  
  dim(Data) = c(floor(nrows.local), ncols)
  
  new("shaq", Data=Data, nrows=nrows, ncols=ncols)
}

#' @rdname shaq
#' @export
shaq.integer = shaq.vector

#' @rdname shaq
#' @export
shaq.double = shaq.vector

#' @rdname shaq
#' @export
shaq.NULL = function(Data, nrows, ncols, checks=TRUE)
{
  if (missing(ncols))
    stop("'ncols' can not be missing if 'Data' is NULL on any rank")
  
  Data = matrix(nrow=0, ncol=ncols)
  shaq.matrix(Data, nrows, ncols, checks)
}



intuit.tshaq.ncols = function(Data)
{
  allreduce(NCOL(Data))
}

check.tshaq.nrows = function(Data, nrows)
{
  rowcheck = comm.all(NROW(Data) == nrows)
  if (!isTRUE(rowcheck))
    comm.stop("local row dimensions disagree across ranks")
}

check.tshaq = function(Data, nrows, ncols)
{
  check.tshaq.nrows(Data, nrows)
  
  ncols = intuit.tshaq.ncols(Data)
  
  return(ncols)
}



tshaq.Mat = function(Data, nrows, ncols, checks=TRUE)
{
  if (!missing(nrows))
    check.is.natnum(nrows)
  if (!missing(ncols))
    check.is.natnum(ncols)
  
  check.is.flag(checks)
  
  if (missing(nrows) || missing(ncols))
  {
    if (missing(ncols))
      ncols = intuit.tshaq.ncols(Data)
    
    if (missing(nrows))
      nrows = NROW(Data)
  }
  else if (checks)
    ncols = check.tshaq(Data, nrows, ncols)
  
  new("tshaq", Data=Data, nrows=nrows, ncols=ncols)
}

#' @rdname shaq
#' @export
tshaq.matrix = tshaq.Mat

#' @rdname shaq
#' @export
tshaq.float32 = tshaq.Mat

#' @rdname shaq
#' @export
tshaq.vector = function(Data, nrows, ncols, checks=TRUE)
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
      nrows = 1L
      ncols = length(Data)
    }
    else if (missing(nrows))
      nrows = 1L
    else if (missing(ncols))
      ncols = 1L
  }
  
  
  size = comm.size()
  base = ncols %/% size
  rem = ncols - base*size
  ncols.local = base
  if (comm.rank()+1L < rem)
    ncols.local = ncols.local + 1
  
  dim(Data) = c(nrows, floor(ncols.local))
  
  new("tshaq", Data=Data, nrows=nrows, ncols=ncols)
}

#' @rdname shaq
#' @export
tshaq.NULL = function(Data, nrows, ncols, checks=TRUE)
{
  if (missing(nrows))
    stop("'nrows' can not be missing if 'Data' is NULL on any rank")
  
  Data = matrix(nrow=0, ncol=nrows)
  tshaq.matrix(Data, nrows, ncols, checks)
}
