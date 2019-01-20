#' svm
#' 
#' Support vector machine.  The internals are nearly identical to that of the
#' logistic regression fitter, except that here we use the "hinged loss".
#' 
#' @details
#' The optimization uses Nelder-Mead.
#' 
#' Both of \code{x} and \code{y} must be distributed in an identical fashion.
#' This means that the number of rows owned by each MPI rank should match, and
#' the data rows \code{x} and response rows \code{y} should be aligned.
#' Additionally, each MPI rank should own at least one row.  Ideally they should
#' be load balanced, so that each MPI rank owns roughly the same amount of data.
#' 
#' @section Communication:
#' The communication consists of an allreduce of 1 double (the local
#' cost/objective function value) at each iteration of the optimization.
#' 
#' @param x,y
#' The input data \code{x} and response \code{y}.  Each must be a shaq, and
#' each must be distributed in an identical fashion.  See the details section
#' for more information.
#' @param maxiter
#' The maximum number of iterations.
#' 
#' @return
#' The return is the output of an \code{optim()} call.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' comm.set.seed(1234, diff=TRUE)
#' 
#' x = ranshaq(rnorm, 10, 3)
#' y = ranshaq(function(i) sample(0:1, size=i, replace=TRUE), 10)
#' 
#' fit = svm(x, y)
#' comm.print(fit)
#' 
#' finalize()
#' }
#' 
#' @references
#' Efron, B. and Hastie, T., 2016. Computer Age Statistical Inference (Vol. 5).
#' Cambridge University Press.
#' 
#' @seealso
#' \code{\link{glm}}
#' 
#' @name svm
#' @rdname svm
NULL



norm2 = function(x) sum(x*x)

hinge_loss = function(z) pmax(0, z)

cost_svm = function(w, x, y)
{
  m = nrow(x)
  J.local = 1/m * sum(hinge_loss(1.0 - DATA(y)*matmult(DATA(x), w)))
  
  allreduce_dbl(J.local) + 1/m * 0.5 * norm2(w)
}



#' @rdname svm
#' @export
svm = function(x, y, maxiter=500)
{
  check.is.shaq(x)
  check.is.shaq(y)
  check.is.posint(maxiter)
  
  control = list(maxit=maxiter)
  w = numeric(ncol(x))
  optim(par=w, fn=cost_svm, x=x, y=y, method="Nelder-Mead", control=control)
}
