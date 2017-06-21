#' Scale
#' 
#' Centers and/or scales the columns of a distributed matrix.
#' 
#' @param x 
#' A shaq.
#' @param center 
#' logical value, determines whether or not columns are zero centered
#' @param scale 
#' logical value, determines whether or not columns are rescaled to unit variance
#' 
#' @return 
#' Returns a distributed matrix.
#' 
#' @name scale
#' @rdname scale
NULL



scale.shaq = function(x, center=TRUE, scale=TRUE)
{
  if (center && !scale)
  {
    cm = colMeans(x)
    Data = base::scale(Data(x), center=cm, scale=FALSE)
    attr(Data, "scaled:center") = cm
    
    ret = shaq(Data, nrow(x), ncol(x), checks=FALSE)
  }
  
  ret
}


#' @export
setGeneric(name="scale", useAsDefault=base::scale, package="pbdSHAQ")

#' @export
setMethod("scale", signature(x="shaq", center="logical", scale="logical"), scale.shaq)
