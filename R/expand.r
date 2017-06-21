#' @export
expand = function(x)
{
  if (comm.rank() == 0)
  {
    dim = dim(x)
    bcast(dim)
    
    size = comm.size()
    id = get.jid(NROW(x), all=TRUE)
    
    if (size > 1)
    {
      for (i in 1:(size - 1L))
      {
        x.local = x[id[[i+1]], ]
        send(x.local, rank.dest=i)
      }
    }
    
    x.local = x[id[[1]], ]
  }
  else
  {
    dim = bcast()
    x.local = recv(rank.source=0)
  }
  
  shaq(x.local, dim[1L], dim[2L], checks=FALSE)
}
