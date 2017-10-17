#' getters
#' 
#' Getters for shaq objects.
#' 
#' @details
#' Functions to return the number of rows (\code{nrow()} and \code{NROW()}),
#' the number of columns (\code{ncol()} and \code{NCOL()}), the length - or
#' product of the number of rows and cols - (\code{length()}), and the local
#' submatrix (\code{DATA()}).
#' 
#' @section Communication:
#' Each operation is completely local.
#' 
#' @param x
#' A shaq.
#' 
#' @seealso \code{\link{setters}}
#' @name getters
#' @rdname getters
NULL



nrows_shaq = function(x) x@nrows

#' @rdname getters
#' @export
setMethod("nrow", signature(x="shaq"), nrows_shaq)

#' @rdname getters
#' @export
setMethod("NROW", signature(x="shaq"), nrows_shaq)

nrows.local_shaq = function(x) NROW(DATA(x))

#' @rdname getters
#' @export
setGeneric(name="nrow.local", useAsDefault=nrows.local_shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("nrow.local", signature(x="shaq"), nrows.local_shaq)



ncols_shaq = function(x) x@ncols

#' @rdname getters
#' @export
setMethod("ncol", signature(x="shaq"), ncols_shaq)

#' @rdname getters
#' @export
setMethod("NCOL", signature(x="shaq"), ncols_shaq)

ncols.local_shaq = function(x) NCOL(DATA(x))

#' @rdname getters
#' @export
setGeneric(name="ncol.local", useAsDefault=ncols.local_shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("ncol.local", signature(x="shaq"), ncols.local_shaq)



#' @rdname getters
#' @export
setMethod("length", signature(x="shaq"), function(x) nrow(x)*ncol(x))



DATA_get_shaq = function(x) x@Data

#' @rdname getters
#' @export
setGeneric(name="DATA", useAsDefault=DATA_get_shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("DATA", signature(x="shaq"), DATA_get_shaq)
