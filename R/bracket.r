bracket.shaq = function(x, i, j)
{
  ret = x
  if (missing(i))
  {
    DATA(ret) = Data(x)[, j, drop=FALSE]
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
