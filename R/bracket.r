bracket.shaq = function(x, i, j)
{
  ret = x
  if (missing(i))
  {
    ret@Data = x@Data[, j, drop=FALSE]
    if (length(j) == 0 || (length(j) == 1 && j == 0))
      ret@ncols = 0
    else if (is.logical(j))
      ret@ncols = length(which(j))
    else if (j[1] > 0)
      ret@ncols = max(0, length(unique(j)))
    else # negative
      ret@ncols = max(0, ret@ncols - length(unique(j)))
  }
  else
    comm.stop("not yet implemented for i non-missing")
  
  ret
}



#' @export
setMethod("[", signature(x="shaq"), bracket.shaq)
