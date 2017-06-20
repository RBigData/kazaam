#' @export
SVD = function(x, retu=FALSE, retv=FALSE)
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
    u = new("shaq", Data=u.local, nrows=x@nrows, ncols=x@ncols)
  }
  else
    u = NULL
  
  if (retv)
    v = ev$vectors
  else
    v = NULL
  
  list(d=d, u=u, v=v)
}
