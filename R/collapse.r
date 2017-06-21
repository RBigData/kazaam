#' @export
collapse = function(x)
{
  if (comm.rank() == 0)
  {
    size = comm.size()
    ret = matrix(0, x@nrows, x@ncols)
    
    top = NROW(x@Data)
    ret[1:top, ] = x@Data
    
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
    attrs = attributes(x@Data)[-which(names(attributes(x@Data))=="dim")]
    if (length(attrs))
      attributes(ret) = c(attributes(ret), attrs)
    
    ret
  }
  else
  {
    send(x@Data, rank.dest=0)
    
    invisible(NULL)
  }
}
