#' Singular Value Decomposition
#' 
#' SVD for distributed matrices with R-like syntax, with
#' calculations performed by the PBLAS and ScaLAPACK libraries.
#' 
#' Extensions of R linear algebra functions.
#' 
#' @param x
#' numeric distributed matrices.
#' @param nu 
#' number of left singular vectors to return when calculating
#' singular values.
#' @param nv 
#' number of right singular vectors to return when calculating
#' singular values.
#' @param LINPACK
#' Ignored.
#' 
#' @return 
#' \code{La.svd()} performs singular value decomposition, and returns the
#' transpose of right singular vectors if any are requested. Singular values
#' are stored as a global R vector. Left and right singular vectors are unique
#' up to sign. Sometimes core R (via LAPACK) and ScaLAPACK will disagree as to
#' what the left/right singular vectors are, but the disagreement is always
#' only up to sign.
#' 
#' \code{svd()} performs singular value decomposition. Differs from
#' \code{La.svd()} in that the right singular vectors, if requested, are
#' returned non-transposed. Singular values are stored as a global R vector.
#' Sometimes core R (via LAPACK) and ScaLAPACK will disagree as to what the
#' left/right singular vectors are, but the disagreement is always only up to
#' sign.
#' 
#' @export
setMethod("svd", signature(x="shaq"),
  function(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)
  {
    n <- nrow(x)
    p <- ncol(x)
    
    retu = nu > 0
    retv= nv > 0
    
    ret = svd.shaq(x, retu, retv)
    if (nu && ncol(ret$u) > nu)
      ret$u = ret$u[, 1:nu]
    
    if (nv && NCOL(ret$v) > nv)
      ret$v = ret$v[, 1:nv]
    
    ret
  }
)

 

svd.shaq = function(x, retu=FALSE, retv=FALSE)
{
  cp = cp.shaq(x)
  if (!retu && !retv)
    only.values = TRUE
  else
    only.values = FALSE
  
  ev = eigen(cp, only.values=only.values, symmetric=TRUE)
  
  d = sqrt(ev$values)
  
  if (retu)
  {
    u.local = ev$vectors %*% diag(1/d)
    u.local = Data(x) %*% u.local
    u = shaq(u.local, nrow(x), ncol(x), checks=FALSE)
  }
  else
    u = NULL
  
  if (retv)
    v = ev$vectors
  else
    v = NULL
  
  list(d=d, u=u, v=v)
}
