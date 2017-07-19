#' rcond
#' 
#' Reciprocal condition number estimate.
#' 
#' @details
#' The estimate is computed by first forming the R matrix from a QR.  Currently
#' this involves first computing a crossproduct, so the estimate can be thought
#' of as being fairly liberal.  Afterwards, the condition number is estimated by
#' calling \code{base::rcond()} on the local R matrix.
#' 
#' @section Communication:
#' The operation is completely local except for forming the crossproduct, which
#' is an \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' A shaq.
#' @param norm,triangular,...
#' Arguments passed to \code{base::rcond()}.
#' 
#' @return 
#' A number.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' rc = rcond(x)
#' comm.print(rc)
#' 
#' x = expand(matrix(1:30, 10))
#' rc = rcond(x)
#' comm.print(rc)
#' 
#' finalize()
#' }
#' 
#' @name rcond
#' @rdname rcond
NULL



rcond.shaq = function(x, norm=c("O", "I", "1"), triangular=FALSE, ...)
{
  norm = pbdMPI::comm.match.arg(toupper(norm), c("O", "I", "1"))
  R = qr_R(x)
  base::rcond(R, norm=norm, triangular=TRUE, ...)
}



#' @rdname rcond
#' @export
setGeneric(name="rcond", useAsDefault=base::rcond, package="kazaam")

#' @rdname rcond
#' @export
setMethod("rcond", signature(x="shaq"), rcond.shaq)
