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
