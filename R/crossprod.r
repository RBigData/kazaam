#' Matrix Multiplication
#' 
#' Conceptually, this computes \code{t(x) \%*\% x} for a shaq \code{x}.
#' 
#' @section Communication:
#' The operation consists of a local crossproduct, followed by an
#' \code{allreduce()} call, quadratic on the number of columns.
#' 
#' @param x
#' A shaq.
#' @param y
#' A shaq or \code{NULL}.
#' 
#' @return 
#' A regular matrix.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' cp = crossprod(x)
#' comm.print(cp)
#' 
#' finalize()
#' }
#' 
#' @name crossprod
#' @rdname crossprod
NULL



cp.internal = function(x, alpha)
{
  data = DATA(x)
  alpha = as.double(alpha)
  
  if (is.float(data))
  {
    ret = .Call(R_float_mpicrossprod, DATA(data), alpha)
    float32(ret) 
  }
  else
  {
    if (!is.double(data))
      storage.mode(data) = "double"
    
    .Call(R_mpicrossprod, data, alpha)
  }
}

cp.shaq = function(x, y = NULL)
{
  if (!is.null(y))
  {
    if (is.shaq(y))
    {
      cp.local = crossprod(DATA(x), DATA(y))
      MPI_Allreduce(cp.local)
    }
    else
      comm.stop("argument 'y' must be a shaq or NULL")
  }
  else
  {
    # cp.local = base::crossprod(DATA(x))
    # allreduce(cp.local)
    cp.internal(x, 1.0)
  }
}

tcp.shaq = function(x, y = NULL)
{
  if (!is.null(y))
  {
    if (is.tshaq(y))
    {
      tcp.local = tcrossprod(DATA(x), DATA(y))
      MPI_Allreduce(tcp.local)
    }
    else
      comm.stop("argument 'y' must be a tshaq or NULL")
  }
  else
  {
    # cp.local = base::crossprod(DATA(x))
    # allreduce(cp.local)
    cp.internal(x, 1.0)
  }
}



#' @rdname crossprod
#' @export
setMethod("crossprod", signature(x="shaq", y="ANY"), cp.shaq)

#' @rdname crossprod
#' @export
setMethod("tcrossprod", signature(x="tshaq", y="ANY"), tcp.shaq)
