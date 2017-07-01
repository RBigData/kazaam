#' is.shaq
#' 
#' Test if an object is a shaq.
#' 
#' @section Communication:
#' The operation is completely local.
#' 
#' @param x
#' An R object.
#' 
#' @return
#' A logical value, indicating whether or not the input is a shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' comm.print(is.shaq(x))
#' 
#' finalize()
#' }
#' 
#' @export
is.shaq = function(x)
{
  is(x, "shaq")
}
