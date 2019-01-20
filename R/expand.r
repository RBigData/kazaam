#' expand
#' 
#' Expand a regular matrix owned on MPI rank 0 into a shaq.
#' 
#' @section Communication:
#' Short answer: quite a bit.  Each local submatrix has to be received from rank
#' 0.
#' 
#' @param x
#' A regular matrix.
#' @param class
#' 'shaq' or 'tshaq'
#' 
#' @return
#' A shaq or a tshaq.
#' 
#' @examples
#' spmd.code = "
#'   library(kazaam)
#'   if (comm.rank() == 0){
#'     x = matrix(runif(30), 10, 3)
#'   } else {
#'     x = NULL
#'   }
#'   
#'   dx = expand(x)
#'   dx
#'   
#'   finalize()
#' "
#' 
#' pbdMPI::execmpi(spmd.code=spmd.code, nranks=2)
#' 
#' @name expand
#' @rdname expand
NULL



expander = function(x, trans=FALSE)
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
    if (!trans)
      n = NROW(x)
    else
      n = NCOL(x)
    
    id = pbdMPI::get.jid(n, all=TRUE)
    
    if (!trans)
    {
      if (size > 1)
      {
        for (i in 1:(size - 1L))
        {
          x.local = x[id[[i+1]], , drop=FALSE]
          send(x.local, rank.dest=i)
        }
      }
      
      x.local = x[id[[1]], , drop=FALSE]
    }
    else
    {
      if (size > 1)
      {
        for (i in 1:(size - 1L))
        {
          x.local = x[, id[[i+1]], drop=FALSE]
          send(x.local, rank.dest=i)
        }
      }
      
      x.local = x[, id[[1]], drop=FALSE]
    }
  }
  else
  {
    dim = bcast()
    x.local = recv(rank.source=0)
  }
  
  shaq(x.local, dim[1L], dim[2L], checks=FALSE)
}

expand.shaq = function(x)
{
  expander(x, trans=FALSE)
}

expand.tshaq = function(x)
{
  expander(x, trans=TRUE)
}



#' @rdname expand
#' @export
expand = function(x, class="shaq")
{
  class = pbdMPI::comm.match.arg(tolower(class), c("shaq", "tshaq"))
  if (class == "shaq")
    expand.shaq(x)
  else
    expand.tshaq(x)
}
