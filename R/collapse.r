#' collapse
#' 
#' Collapse a shaq into a regular matrix.
#' 
#' @details
#' Only rank 0 will own the matrix on return.
#' 
#' @param x
#' A shaq.
#' 
#' @return
#' A regular matrix (rank 0) or \code{NULL} (everyone else).
#' 
#' @export
collapse = function(x)
{
  check.is.shaq(x)
  
  if (comm.rank() == 0)
  {
    size = comm.size()
    ret = matrix(0, nrow(x), ncol(x))
    
    top = nrow.local(x)
    ret[1:top, ] = Data(x)
    
    if (size > 1)
    {
      for (i in 1:(size - 1L))
      {
        x.local = recv(rank.source=i)
        size = NROW(x.local)
        
          ret[top + (1:size), ] = x.local
          top = top + size
      }
    }
    
    # preserve attributes
    attrs = attributes(Data(x))[-which(names(attributes(Data(x)))=="dim")]
    if (length(attrs))
      attributes(ret) = c(attributes(ret), attrs)
    
    ret
  }
  else
  {
    send(Data(x), rank.dest=0)
    
    invisible(NULL)
  }
}
