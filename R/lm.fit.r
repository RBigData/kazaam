#' lm_coefs
#' 
#' Coefficients of the linear model.
#' 
#' @param x
#' A shaq (the input data).
#' @param y
#' A shaq (the response).
#' 
#' @return
#' A regular vector.
#' 
#' @export
lm_coefs = function(x, y, tol=1e-7)
{
  check.is.shaq(x)
  check.is.shaq(y)
  
  R = qr_R(x)
  Q = qr_Q(x, R)
  
  ### NOTE here we assume that x (and hence Q) and y are distributed in identical fashion!
  qty = allreduce(crossprod(Data(Q), Data(y)))
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
