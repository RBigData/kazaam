#' Linear Model Coefficients
#' 
#' Coefficients of the linear model.
#' 
#' @details
#' The model is fit using a QR factorization of the input \code{x}. At this
#' time, that means 
#' 
#' Both of \code{x} and \code{y} must be distributed in an identical fashion.
#' This means that the number of rows owned by each MPI rank should match, and
#' the data rows \code{x} and labels \code{y} should be aligned.  Additionally,
#' each MPI rank should own at least one row.  Ideally they should be load
#' balanced, so that each MPI rank owns roughly the same amount of data.
#' 
#' @section Communication:
#' The operation has the same communication as 
#' 
#' @param x,y
#' The input data \code{x} and response \code{y}.  Each must be a shaq, and
#' each must be distributed in an identical fashion.  See the details section
#' for more information.
#' @param tol
#' Numerical tolerance for deciding rank.
#' 
#' @return
#' A regular vector.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' comm.set.seed(1234, diff=TRUE)
#' 
#' x = ranshaq(rnorm, 10, 3)
#' y = ranshaq(runif, 10)
#' 
#' fit = lm_coefs(x, y)
#' comm.print(fit)
#' 
#' finalize()
#' }
#' 
#' @seealso \code{\link{glms}}
#' @name lm_coefs
#' @export
lm_coefs = function(x, y, tol=1e-7)
{
  check.is.shaq(x)
  check.is.shaq(y)
  
  R = qr_R(x)
  Q = qr_Q(x, R)
  
  ### NOTE here we assume that x (and hence Q) and y are distributed in identical fashion!
  qty = MPI_Allreduce(crossprod(DATA(Q), DATA(y)))
  rank = max(which(abs(diag(R)) > tol))
  
  if (rank < ncol(x))
  {
    ind = 1:rank
    coefs = solve(R[ind, ind], qty[ind])
    coefs = c(coefs, rep(NA_real_, ncol(x)-rank))
  }
  else
    coefs = solve(R, qty)
  
  if (ncol(y) == 1)
  {
    dim(coefs) = NULL
    names(coefs) = paste0("x", 1:ncol(x))
  }
  else
    rownames(coefs) = paste0("x", 1:ncol(x))
  
  coefs
}
