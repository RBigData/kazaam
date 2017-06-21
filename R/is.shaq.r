#' is.shaq
#' 
#' Test if an object is a shaq.
#' 
#' @param x
#' An R object.
#' 
#' @return
#' A logical value, indicating whether or not the input is a shaq.
#' 
#' @export
is.shaq = function(x)
{
  is(x, "shaq")
}
