check.shaq = function(Data, nrows, ncols)
{
  colcheck = comm.all(NCOL(Data) == ncols)
  if (!isTRUE(colcheck))
    comm.stop("local column dimensions disagree across ranks")
  
  rowcheck = allreduce(NROW(Data))
  nrows = rowcheck
  
  return(nrows)
}



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
