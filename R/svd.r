#' @export
setMethod("svd", signature(x="shaq"),
  function(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)
  {
    n <- x@nrows
    p <- x@ncols
    
    retu = nu > 0
    retv= nv > 0
    
    ret = svd.shaq(x, retu, retv)
    if (nu && ret$u@ncols > nu)
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
    u.local = x@Data %*% u.local
    u = shaq(u.local, x@nrows, x@ncols, checks=FALSE)
  }
  else
    u = NULL
  
  if (retv)
    v = ev$vectors
  else
    v = NULL
  
  list(d=d, u=u, v=v)
}
