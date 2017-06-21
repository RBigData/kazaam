#' svd
#' 
#' Singular value decomposition.
#' 
#' @param x
#' A shaq.
#' @param nu 
#' number of left singular vectors to return.
#' @param nv 
#' number of right singular vectors to return.
#' @param LINPACK
#' Ignored.
#' 
#' @return 
#' A list of elements \code{d}, \code{u}, and \code{v}, as with R's own
#' \code{svd()}.  The elements are, respectively, a regular vector, a shaq, and
#' a regular matrix.
#' 
#' @rdname svd
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
