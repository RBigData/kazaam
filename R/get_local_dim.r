#' get_local_dim
#' 
#' Given a global number of rows or columns, get the corresponding local number
#' of rows or columns. Pass the number of rows for a \code{shaq} and the number
#' of columns for a \code{tshaq}.
#' 
#' @details
#' This assumes that the data is distributed across ranks in roughly equal
#' pieces. Modify the communicator by setting \code{.pbd_env$SPMD.CT$comm}.
#' 
#' @param dim
#' Either the global number of rows or the global number of columns.
#' 
#' @return
#' The number of rows/columns which is local to the calling rank.
#' 
#' @export
get_local_dim = function(dim)
{
  dim = as.double(dim)
  
  size = comm.size()
  local = dim %/% size
  rem = dim - local*size
  if (comm.rank()+1 <= rem)
    local = local + 1
  
  local
}
