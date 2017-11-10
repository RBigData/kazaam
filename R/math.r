#' math
#' 
#' Basic math operations, like trig, rounding, exp/logs.
#' 
#' @details
#' In each case, the corresponding base R function is called on the local data.
#' 
#' @section Communication:
#' Each operation is completely local.
#' 
#' @param x
#' A shaq or a numeric vector.
#' 
#' @return 
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' y = ranshaq(runif, 10, 3)
#' 
#' sqrt(x)
#' log(y)
#' 
#' finalize()
#' }
#' 
#' @name math
#' @rdname math
NULL



ply_simple = function(x, fun, ...)
{
  DATA(x) = fun(DATA(x), ...)
  x
}

# ------------------------------------------------------------------------------
# exp and log
# ------------------------------------------------------------------------------

#' @rdname math
#' @export
setMethod("sqrt", signature(x="shaq"), function(x) ply_simple(x, sqrt))

#' @rdname math
#' @export
setMethod("abs", signature(x="shaq"), function(x) ply_simple(x, abs))

#' @rdname math
#' @export
setMethod("exp", signature(x="shaq"), function(x) ply_simple(x, exp))

#' @rdname math
#' @export
setMethod("log", signature(x="shaq"), function(x, base=exp(1)) ply_simple(x, log, base=base))

#' @rdname math
#' @export
setMethod("log2", signature(x="shaq"), function(x) ply_simple(x, log2))

#' @rdname math
#' @export
setMethod("log10", signature(x="shaq"), function(x) ply_simple(x, log10))

# ------------------------------------------------------------------------------
# trig
# ------------------------------------------------------------------------------

#' @rdname math
#' @export
setMethod("sin", signature(x="shaq"), function(x) ply_simple(x, sin))

#' @rdname math
#' @export
setMethod("cos", signature(x="shaq"), function(x) ply_simple(x, cos))

#' @rdname math
#' @export
setMethod("tan", signature(x="shaq"), function(x) ply_simple(x, tan))

#' @rdname math
#' @export
setMethod("asin", signature(x="shaq"), function(x) ply_simple(x, asin))

#' @rdname math
#' @export
setMethod("acos", signature(x="shaq"), function(x) ply_simple(x, acos))

#' @rdname math
#' @export
setMethod("atan", signature(x="shaq"), function(x) ply_simple(x, atan))

#' @rdname math
#' @export
setMethod("sinh", signature(x="shaq"), function(x) ply_simple(x, sinh))

#' @rdname math
#' @export
setMethod("cosh", signature(x="shaq"), function(x) ply_simple(x, cosh))

#' @rdname math
#' @export
setMethod("tanh", signature(x="shaq"), function(x) ply_simple(x, tanh))

#' @rdname math
#' @export
setMethod("asinh", signature(x="shaq"), function(x) ply_simple(x, asinh))

#' @rdname math
#' @export
setMethod("acosh", signature(x="shaq"), function(x) ply_simple(x, acosh))

#' @rdname math
#' @export
setMethod("atanh", signature(x="shaq"), function(x) ply_simple(x, atanh))

# ------------------------------------------------------------------------------
# rounding
# ------------------------------------------------------------------------------

#' @rdname math
#' @export
setMethod("ceiling", signature(x="shaq"), function(x) ply_simple(x, ceiling))

#' @rdname math
#' @export
setMethod("floor", signature(x="shaq"), function(x) ply_simple(x, floor))

#' @rdname math
#' @export
setMethod("trunc", signature(x="shaq"), function(x, ...) ply_simple(x, trunc, ...))

#' @rdname math
#' @export
setMethod("round", signature(x="shaq"), function(x, digits=0) ply_simple(x, round, digits=digits))
