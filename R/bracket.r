#' subsetting
#' 
#' Subsetting via \code{`[`} for shaq objects.
#' 
#' @param x
#' A shaq.
#' @param i,j
#' Indices.  NOTE currently only implemented for \code{j} values.
#' 
#' @return
#' A shaq.
#' 
#' @name bracket
#' @rdname bracket
NULL



bracket.shaq = function(x, i, j)
{
  if (missing(i))
  {
    Data = Data(x)[, j, drop=FALSE]
    if (length(j) == 0 || (length(j) == 1 && j == 0))
      ncols = 0
    else if (is.logical(j))
      ncols = length(which(j))
    else if (j[1] > 0)
      ncols = max(0, length(unique(j)))
    else # negative
      ncols = max(0, ncol(x) - length(unique(j)))
  }
  else # FIXME
    comm.stop("not yet implemented for i non-missing")
  
  shaq(Data, nrow(x), ncols)
}



#' @rdname bracket
#' @export
setMethod("[", signature(x="shaq"), bracket.shaq)
