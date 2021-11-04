#' Matrix Multiplication
#' 
#' Conceptually, this computes \code{t(x) \%*\% x} for a shaq \code{x}.
#' 
#' @section Communication:
#' The operation consists of a local crossproduct, followed by an
#' \code{allreduce()} call, quadratic on the number of columns.
#' 
#' For \code{crossprod()}, if the matrix distribution is poorly balanced
#' (specifically, if any rank has fewer rows than columns), then an inefficient
#' method is used. Similarly for \code{tcrossprod()} if the number of local rows
#' is greater than the number of local columns.
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
#' @useDynLib kazaam R_mpicrossprod
#' @useDynLib kazaam R_float_mpicrossprod
#' 
#' @name crossprod
#' @rdname crossprod
NULL



cp.internal = function(x, alpha)
{
  comm_ptr = pbdMPI::get.mpi.comm.ptr(.pbd_env$SPMD.CT$comm)
  data = DATA(x)
  alpha = as.double(alpha)
  
  if (is.float(data))
  {
    ret = .Call(R_float_mpicrossprod, DATA(data), alpha, comm_ptr)
    float32(ret) 
  }
  else
  {
    if (!is.double(data))
      storage.mode(data) = "double"
    
    .Call(R_mpicrossprod, data, alpha, comm_ptr)
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
    m = nrow(DATA(x))
    n = ncol(DATA(x))
    check = MPI_Allreduce(as.integer(m<n))
    if (check)
    {
      cp.local = crossprod(DATA(x))
      MPI_Allreduce(cp.local)
    }
    else
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
    m = nrow(DATA(x))
    n = ncol(DATA(x))
    check = MPI_Allreduce(as.integer(m<n))
    if (check)
    {
      tcp.local = tcrossprod(DATA(x))
      MPI_Allreduce(tcp.local)
    }
    else
      cp.internal(x, 1.0)
  }
}



#' @rdname crossprod
#' @export
setMethod("crossprod", signature(x="shaq", y="ANY"), cp.shaq)

#' @rdname crossprod
#' @export
setMethod("tcrossprod", signature(x="tshaq", y="ANY"), tcp.shaq)
