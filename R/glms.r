#' Generalized Linear Model Fitters
#' 
#' Linear regression (Gaussian GLM), logistic regression, and poisson
#' regression model fitters.
#' 
#' @details
#' Each function is implemented with gradient descent using the conjugate
#' gradients method ("CG") of the \code{optim()} function.
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
#' fit = logistic.fit(x, y)
#' comm.print(fit)
#' 
#' finalize()
#' }
#' 
#' @references
#' McCullagh, P. and Nelder, J.A., 1989. Generalized Linear Models, no. 37 in
#' Monograph on Statistics and Applied Probability.
#' 
#' Duda, R.O., Hart, P.E. and Stork, D.G., 1973. Pattern classification (pp.
#' 526-528). Wiley, New York.
#' 
#' @seealso
#' \code{\link{lm_coefs}}
#' 
#' @name glms
#' @rdname glms
NULL



# linkinv_gaussian = identity

cost_gaussian = function(theta, x, y)
{
  m = nrow(x)
  J.local = (1/(2*m))*sum((DATA(x)%*%theta - DATA(y))^2)
  
  as.double(allreduce_dbl(J.local))
}

#' @rdname glms
#' @export
reg.fit = function(x, y, maxiter=100)
{
  check.is.shaq(x)
  check.is.shaq(y)
  check.is.posint(maxiter)
  
  control = list(maxit=maxiter)
  theta = numeric(ncol(x))
  optim(par=theta, fn=cost_gaussian, x=x, y=y, method="CG", control=control)
}



linkinv_logistic = binomial(logit)$linkinv

cost_logistic = function(theta, x, y)
{
  m = nrow(x)
  eta = DATA(x)%*%theta
  h = linkinv_logistic(eta)
  J.local = (1/m)*sum((-DATA(y)*log(h)) - ((1-DATA(y))*log(1-h)))
  
  allreduce_dbl(J.local)
}

#' @rdname glms
#' @export
logistic.fit = function(x, y, maxiter=100)
{
  check.is.shaq(x)
  check.is.shaq(y)
  check.is.posint(maxiter)
  
  control = list(maxit=maxiter)
  theta = numeric(ncol(x))
  optim(par=theta, fn=cost_logistic, x=x, y=y, method="CG", control=control)
}



linkinv_poisson = poisson(log)$linkinv

cost_poisson = function(theta, x, y)
{
  eta = DATA(x)%*%theta
  J.local = -sum(DATA(y) * eta - linkinv_poisson(eta))
  
  allreduce_dbl(J.local)
}

#' @rdname glms
#' @export
poisson.fit = function(x, y, maxiter=100)
{
  check.is.shaq(x)
  check.is.shaq(y)
  check.is.posint(maxiter)
  
  control = list(maxit=maxiter)
  theta = numeric(ncol(x))
  optim(par=theta, fn=cost_poisson, x=x, y=y, method="CG", control=control)
}
