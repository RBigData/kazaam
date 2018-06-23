#' rsvd
#' 
#' Randomized singular value decomposition.
#' 
#' @details
#' TODO
#' 
#' @section Communication:
#' The operation is completely local except for forming the crossproduct, which
#' is an \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' The input data matrix.
#' @param k
#' The number of singular values and/or left/right singular vectors
#' to estimate.
#' @param q
#' An integer exponent, say 1, 2, or 3.  See the paper for details.
#' @param retu
#' Logical; should the left singular vectors ("U") be returned?
#' @param retv
#' Logical; should the right singular vectors ("V") be returned?
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
#' svd = rsvd(x)
#' comm.print(svd$d) # a globally owned vector
#' svd$u # a shaq
#' comm.print(svd$v) # a globally owned matrix
#' 
#' finalize()
#' }
#' 
#' @references
#' Halko, Martinsson, and Tropp. 2011. Finding structure with randomness:
#' probabilistic algorithms for constructing approximate matrix decompositions.
#' SIAM Review 53 217-288.
#' 
#' @export
rsvd <- function(x, k=1, q=2, retu=TRUE, retv=TRUE)
{
  if (!is.shaq(x))
    comm.stop("input 'x' must be a shaq")
  
  ### Stage A from the paper
  n = ncol(x)
  Omega = runif(n * 2*k) # nx(2k)
  dim(Omega) = c(n, 2*k)
  
  Y = x %*% Omega # mx(2k)
  Q = qr_Q(Y) # mx(2k)
  
  for (i in 1:q)
  {
    Y = crossprod(x, Q) # nx(2k)
    Q = qr.Q(qr(Y)) # nx(2k)
    Y = x %*% Q # mx(2k)
    Q = qr_Q(Y) # mx(2k)
  }
  
  ### Stage B
  B = crossprod(Q, x) # (2k)xn
  
  if (!retu)
    nu = 0
  else
    nu = min(dim(B))
  
  if (!retv)
    nv = 0
  else
    nv = min(dim(B))
  
  svd.B = svd(B, nu, nv)
  
  ind = 1L:k
  d = (svd.B$d)[ind]
  
  
  # Produce u/v as desired
  if (retu)
  {
    u = Q %*% svd.B$u
    u = u[, ind]
  }
  
  if (retv)
    v = svd.B$v[, ind, drop=FALSE]
  
  
  # wrangle return
  if (retu)
  {
    if (retv)
      svd <- list(d=d, u=u, v=v)
    else
      svd <- list(d=d, u=u)
  }
  else
  {
    if (retv)
      svd <- list(d=d, v=v)
    else
      svd <- list(d=d)
  }
  
  svd
}
