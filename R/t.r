#' t
#' 
#' Transpose a shaq or tshaq distributed matrix.
#' 
#' @section Communication:
#' The operation is completely local.
#' 
#' @param x
#' A shaq or tshaq.
#' 
#' @return 
#' A tshaq or shaq (opposite of what was input).
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' t(x)
#' 
#' finalize()
#' }
#' 
#' @method t shaq
#' @method t tshaq
#' @name t
#' @rdname t
NULL



#' @rdname t
#' @export
t.shaq = function(x)
{
  data = t(DATA(x))
  tshaq(data, ncol(x), nrow(x), checks=FALSE)
}



#' @rdname t
#' @export
t.tshaq = function(x)
{
  data = t(DATA(x))
  shaq(data, ncol(x), nrow(x), checks=FALSE)
}
