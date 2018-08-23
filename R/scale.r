#' Scale
#' 
#' Centers and/or scales the columns of a distributed matrix.
#' 
#' @section Communication:
#' The communication consists of two allreduce calls, each quadratic on the
#' number of columns.
#' 
#' @param x 
#' A shaq.
#' @param center 
#' logical value, determines whether or not columns are zero centered
#' @param scale 
#' logical value, determines whether or not columns are rescaled to unit variance
#' 
#' @return 
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(rnorm, 10, 3, mean=30, sd=10)
#' 
#' x
#' scale(x)
#' 
#' finalize()
#' }
#' 
#' @name scale
#' @rdname scale
NULL



scale.shaq = function(x, center=TRUE, scale=TRUE)
{
  if (!center && !scale) # u dum
    return(x)
  
  cm = colMeans(x)
  
  if (center)
  {
    Data = scale(DATA(x), center=cm, scale=FALSE)
    attr(Data, "scaled:center") = cm
  }
  
  if (scale) # this is disgusting, but it's the only way to do this without a million copies
  {
    if (!center)
      Data = DATA(x)
    
    csd = sqrt(allreduce(colSums(Data*Data)) / (nrow(x)-1))
    
    Data = scale(if (center) Data else DATA(x), center=FALSE, scale=csd)
    attr(Data, "scaled:scale") = csd
  }
  
  
  shaq(Data, nrow(x), ncol(x), checks=FALSE)
}



#' @rdname scale
#' @export
setMethod("scale", signature(x="shaq"), scale.shaq)
