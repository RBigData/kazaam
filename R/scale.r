scale.shaq = function(x, center=TRUE, scale=TRUE)
{
  if (center && !scale)
  {
    cm = colMeans(x)
    Data = base::scale(x@Data, center=cm, scale=FALSE)
    attr(Data, "scaled:center") = cm
    
    ret = shaq(Data, x@nrows, x@ncols, checks=FALSE)
  }
  
  ret
}


#' @export
setGeneric(name="scale", useAsDefault=base::scale, package="pbdSHAQ")

#' @export
setMethod("scale", signature(x="shaq", center="logical", scale="logical"), scale.shaq)
