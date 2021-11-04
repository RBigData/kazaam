#' QR Decomposition Methods
#' 
#' QR factorization.
#' 
#' @details
#' \eqn{R} is formed by first forming the crossproduct \eqn{X^T X} and taking
#' its Cholesky factorization.  But then \eqn{Q = X R^{-1}}.  Inverting \eqn{R}
#' is handled by an efficient triangular inverse routine.
#' 
#' @section Communication:
#' The operation is completely local except for forming the crossproduct, which
#' is an \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' A shaq.
#' @param R
#' A regular matrix. This argument is optional, in that if it is not supplied
#' explicitly, then it will be computed in the background.  But if have already
#' computed R, supplying it here will improve performance (by avoiding
#' needlessly recomputing it).
#' 
#' @return 
#' Q (a shaq) or R (a regular matrix).
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' R = qr_R(x)
#' comm.print(R)
#' 
#' Q = qr_Q(x, R)
#' Q
#' 
#' finalize()
#' }
#' 
#' @name qr
#' @rdname qr
NULL



#' @useDynLib kazaam R_trinv
.trinv = function(x)
{
  .Call(R_trinv, x, 'U')
}



#' @rdname qr
#' @export
qr_R = function(x)
{
  check.is.shaq(x)
  
  cp = cp.shaq(x)
  R = chol(cp)
  
  R
}



#' @rdname qr
#' @export
qr_Q = function(x, R)
{
  check.is.shaq(x)
  
  if (missing(R))
    R = qr_R(x)
  else
    check.is.matrix(R)
  
  Q.local = DATA(x) %*% .trinv(R)
  
  shaq(Q.local, nrow(x), ncol(x), checks=FALSE)
}
