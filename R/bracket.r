#' subsetting
#' 
#' Subsetting via \code{`[`} for shaq objects.
#' 
#' @section Communication:
#' Each operation is completely local.
#' 
#' @param x
#' A shaq.
#' @param i,j
#' Indices.  NOTE currently only implemented for \code{j} values.
#' @param value
#' Replacement value(s) for the \code{[<-} method.  This can either be an
#' appropriately sized numeric value or a shaq.  See the details section for
#' more information.
#' @param ...
#' Ignored.
#' 
#' @return
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' y = x[, -1]
#' y
#' 
#' finalize()
#' }
#' 
#' @name bracket
#' @rdname bracket
NULL



bracket.shaq = function(x, i, j)
{
  if (missing(i) && missing(j))
    return(x)
  
  
  if (missing(i))
  {
    Data = DATA(x)[, j, drop=FALSE]
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



bracket_replace.shaq = function(x, i, j, ..., value)
{
  if (is.shaq(value))
    val = DATA(value)
  else
    val = value
  
  if (missing(i) && missing(j))
    DATA(x) = val
  else if (missing(i))
    DATA(x)[, j] = val
  else # FIXME
    comm.stop("not yet implemented for i non-missing")
  
  x
}

#' @rdname bracket
#' @export
setReplaceMethod("[", signature(x ="shaq", value="ANY"), bracket_replace.shaq)
