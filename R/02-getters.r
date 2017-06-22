#' getters
#' 
#' Getters for shaq objects.
#' 
#' @param x
#' A shaq.
#' 
#' @name getters
#' @rdname getters
NULL



nrows.shaq = function(x) x@nrows

#' @rdname getters
#' @export
setMethod("nrow", signature(x="shaq"), nrows.shaq)

#' @rdname getters
#' @export
setMethod("NROW", signature(x="shaq"), nrows.shaq)

nrows.local.shaq = function(x) NROW(Data(x))

#' @rdname getters
#' @export
setGeneric(name="nrow.local", useAsDefault=nrows.local.shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("nrow.local", signature(x="shaq"), nrows.local.shaq)



ncols.shaq = function(x) x@ncols

#' @rdname getters
#' @export
setMethod("ncol", signature(x="shaq"), ncols.shaq)

#' @rdname getters
#' @export
setMethod("NCOL", signature(x="shaq"), ncols.shaq)

ncols.local.shaq = function(x) NCOL(Data(x))

#' @rdname getters
#' @export
setGeneric(name="ncol.local", useAsDefault=ncols.local.shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("ncol.local", signature(x="shaq"), ncols.local.shaq)



#' @rdname getters
#' @export
setMethod("length", signature(x="shaq"), function(x) nrow(x)*ncol(x))




Data.shaq = function(x) x@Data

#' @rdname getters
#' @export
setGeneric(name="Data", useAsDefault=Data.shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("Data", signature(x="shaq"), Data.shaq)
