#' is.shaq
#' 
#' Test if an object is a shaq or tshaq.
#' 
#' @section Communication:
#' The operation is completely local.
#' 
#' @param x
#' An R object.
#' 
#' @return
#' A logical value, indicating whether or not the input is a shaq/tshaq.
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
#' @name is.shaq
#' @rdname is.shaq
NULL

#' @rdname is.shaq
#' @export
is.shaq = function(x)
{
  is(x, "shaq")
}

#' @rdname is.shaq
#' @export
is.tshaq = function(x)
{
  is(x, "tshaq")
}
