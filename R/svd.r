#' svd
#' 
#' Singular value decomposition.
#' 
#' @details
#' The factorization works by first forming the crossproduct \eqn{X^T X}
#' and then taking its eigenvalue decomposition.  In this case, the square root
#' of the eigenvalues are the singular values.  If the left/right singular
#' vectors \eqn{U} or \eqn{V} are desired, then in either case, \eqn{V} is
#' computed (the eigenvectors).  From these, \eqn{U} can be reconstructed, since
#' if \eqn{X = U\Sigma V^T}, then \eqn{U = XV\Sigma^{-1}}.
#' 
#' @section Communication:
#' The operation is completely local except for forming the crossproduct, which
#' is an \code{allreduce()} call, quadratic on the number of columns.
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
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' svd = svd(x)
#' comm.print(svd$d) # a globally owned vector
#' svd$u # a shaq
#' comm.print(svd$v) # a globally owned matrix
#' 
#' finalize()
#' }
#' 
#' @name svd
#' @rdname svd
NULL



utils::globalVariables(c("n", "p"))



svd.shaq = function(x, retu=FALSE, retv=FALSE)
{
  only.values = !retu && !retv
  
  cp = cp.shaq(x)
  
  ev = eigen(cp, only.values=only.values, symmetric=TRUE)
  
  d = sqrt(ev$values)
  
  if (retu)
  {
    # u.local = ev$vectors %*% diag(1/d)
    u.local = sweep(ev$vectors, STATS=1/d, MARGIN=2, FUN="*")
    u.local = DATA(x) %*% u.local
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

#' @rdname svd
#' @export
setMethod("svd", signature(x="shaq"),
  function(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)
  {
    n = nrow(x)
    p = ncol(x)
    
    check.is.natnum(nu)
    check.is.natnum(nv)
    
    retu = nu > 0
    retv = nv > 0
    
    ret = svd.shaq(x, retu, retv)
    
    if (nu && ncol(ret$u) > nu)
      ret$u = ret$u[, 1:nu]
    
    if (nv && NCOL(ret$v) > nv)
      ret$v = ret$v[, 1:nv]
    
    ret
  }
)



svd.tshaq = function(x, retu=FALSE, retv=FALSE)
{
  only.values = !retu && !retv
  
  cp = tcp.shaq(x)
  
  ev = eigen(cp, only.values=only.values, symmetric=TRUE)
  
  d = sqrt(ev$values)
  
  if (retu)
    u = ev$vectors
  else
    u = NULL
  
  if (retv)
  {
    v.local = sweep(ev$vectors, STATS=1/d, MARGIN=2, FUN="*")
    v.local = v.local %*% DATA(x)
    v = shaq(v.local, nrow(x), ncol(x), checks=FALSE)
  }
  else
    v = NULL
  
  list(d=d, u=u, v=v)
}

#' @rdname svd
#' @export
setMethod("svd", signature(x="tshaq"),
  function(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)
  {
    n = nrow(x)
    p = ncol(x)
    
    check.is.natnum(nu)
    check.is.natnum(nv)
    
    if (nv && ncol(x) > nv)
      comm.stop("argument 'nv' must be 0 or ncol(x)")
    
    retu = nu > 0
    retv = nv > 0
    
    ret = svd.tshaq(x, retu, retv)
    
    if (nu && NCOL(ret$u) > nu)
      ret$u = ret$u[, 1:nu]
    
    ret
  }
)
