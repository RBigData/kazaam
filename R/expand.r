#' expand
#' 
#' Expand a regular matrix owned on MPI rank 0 into a shaq.
#' 
#' @param x
#' A regular matrix.
#' 
#' @return
#' A shaq.
#' 
#' @export
expand = function(x)
{
  if (comm.rank() == 0)
  {
    if (!is.matrix(x))
    {
      if (is.atomic(x))
        dim(x) = c(length(x), 1L)
      else
        stop("RANK 0: argument 'x' must be a matrix")
    }
    
    
    dim = dim(x)
    bcast(dim)
    
    size = comm.size()
    id = get.jid(NROW(x), all=TRUE)
    
    if (size > 1)
    {
      for (i in 1:(size - 1L))
      {
        x.local = x[id[[i+1]], , drop=FALSE]
        isend(x.local, rank.dest=i)
      }
    }
    
    x.local = x[id[[1]], , drop=FALSE]
  }
  else
  {
    dim = bcast()
    x.local = irecv(rank.source=0)
  }
  
  shaq(x.local, dim[1L], dim[2L], checks=FALSE)
}
