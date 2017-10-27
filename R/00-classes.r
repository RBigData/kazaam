#' Class shaq
#' 
#' An S4 container for a distributed tall/skinny matrix.
#' 
#' @details
#' The (conceptual) global (non-distributed) matrix should be distributed by
#' row, meaning that each submatrix should own all of the columns of the global
#' matrix.  Most methods assume no other real structure, however for best
#' performance (and for the methods which require it), one should try to
#' organize their distributed data in a particular way.
#' 
#' First, adjacent MPI ranks should hold adjacent rows.  So if the last row that
#' rank \code{k} owns is \code{i}, then the first row that rank \code{k+1} owns 
#' should be row \code{i+1}.  Additionally, any method that operates on two (or
#' more) shaq objects, the two shaqs should be distributed identically.  By this
#' we mean that if the number of rows shaq \code{A} owns on rank \code{k} is
#' \code{k_i}, then the number of rows shaq \code{B} owns on rank \code{k}
#' should also be \code{k_i}.
#' 
#' Finally, for best performance, one should generally try to keep the number of
#' rows "balanced" (roughly equal) across processes, with perhaps the last "few"
#' having one less row than the others.
#' 
#' @slot DATA
#' The local submatrix.
#' @slot nrows, ncols
#' The global matrix dimension.
#' 
#' @seealso
#' \code{\link{shaq}}
#' 
#' @name shaq-class
#' @rdname shaq-class
#' @docType class
NULL



check_shaq = function(object)
{
  if (is.na(object@nrows) || is.na(object@ncols) || object@nrows < 0 || object@ncols < 0)
    return(paste0("impossible dimensions: given nrows=", object@nrows, " ncols=", object@ncols))
  if (object@nrows < object@ncols)
    return("must have nrows >= ncols for class shaq")
}

check_tshaq = function(object)
{
  if (is.na(object@nrows) || is.na(object@ncols) || object@nrows < 0 || object@ncols < 0)
    return(paste0("impossible dimensions: given nrows=", object@nrows, " ncols=", object@ncols))
  if (object@nrows > object@ncols)
    return("must have nrows <= ncols for class tshaq")
}



#' @rdname shaq-class
setClass(
  Class = "gbd1d",
  representation = representation(
    Data = "matrix",
    nrows = "numeric",
    ncols = "numeric"
    # balanced="logical"
  ),
  
  prototype=prototype(
    Data = matrix(nrow=0, ncol=0),
    nrows = 0L,
    ncols = 0L
    # balanced=NA
  )
)

#' @rdname shaq-class
setClass(
  Class = "shaq",
  contains = "gbd1d",
  validity = check_shaq
)

#' @rdname shaq-class
setClass(
  Class = "tshaq",
  contains = "gbd1d",
  validity = check_tshaq
)
