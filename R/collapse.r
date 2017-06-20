#' @export
collapse = function(x)
{
  if (comm.rank() == 0)
  {
    size = comm.size()
    id = get.jid(x@nrows, all=TRUE)
    ret = matrix(0, x@nrows, x@ncols)
    
    ret[id[[1]], ] = x@Data
    
    for (i in 1:(size - 1L))
    {
      x.local = recv(rank.source=i)
      ret[id[[i+1]], ] = x.local
    }
    
    ret
  }
  else
  {
    send(x@Data, rank.dest=0)
    
    invisible(NULL)
  }
}
