#' @export
qr_R = function(x)
{
  cp = cp.shaq(x)
  R = chol(cp)
  
  R
}



#' @export
qr_Q = function(x, R)
{
  if (missing(R))
    R = qr_R(x)
  
  Q.local = x@Data %*% solve(R)
  
  Q = new("shaq", Data=Q.local, nrows=x@nrows, ncols=x@ncols)
  Q
}
