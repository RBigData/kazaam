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
  else
    comm.stop("not yet implemented for i non-missing")
  
  shaq(Data, nrow(x), ncols)
}



#' @export
setMethod("[", signature(x="shaq"), bracket.shaq)



# #' @export
# setReplaceMethod("[", signature(x ="shaq", value="ANY"), bracket_assign.shaq)
# 
# bracket_assign.shaq = function(x, i, j, ..., value) 
# {
#   if (missing(i))
#     i <- 1L:x@dim[1L]
#   if (missing(j))
#     j <- 1L:x@dim[2L]
#   
# }
