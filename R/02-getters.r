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



nrows.shaq = function(x) x@nrows

#' @rdname getters
#' @export
setMethod("nrow", signature(x="gbd1d"), nrows.shaq)

#' @rdname getters
#' @export
setMethod("NROW", signature(x="gbd1d"), nrows.shaq)

nrows.local.shaq = function(x) NROW(DATA(x))

#' @rdname getters
#' @export
setGeneric(name="nrow.local", useAsDefault=nrows.local.shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("nrow.local", signature(x="gbd1d"), nrows.local.shaq)



ncols.shaq = function(x) x@ncols

#' @rdname getters
#' @export
setMethod("ncol", signature(x="gbd1d"), ncols.shaq)

#' @rdname getters
#' @export
setMethod("NCOL", signature(x="gbd1d"), ncols.shaq)

ncols.local.shaq = function(x) NCOL(DATA(x))

#' @rdname getters
#' @export
setGeneric(name="ncol.local", useAsDefault=ncols.local.shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("ncol.local", signature(x="gbd1d"), ncols.local.shaq)



#' @rdname getters
#' @export
setMethod("length", signature(x="gbd1d"), function(x) nrow(x)*ncol(x))



DATA.get.shaq = function(x) x@Data

#' @rdname getters
#' @export
setGeneric(name="DATA", useAsDefault=DATA.get.shaq, package="kazaam")

#' @rdname getters
#' @export
setMethod("DATA", signature(x="shaq"), DATA.get.shaq)
