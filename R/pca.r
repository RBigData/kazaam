#' Principal Components Analysis
#' 
#' Performs the principal components analysis.
#' 
#' @details
#' \code{prcomp()} performs the principal components analysis on the data
#' matrix by taking the SVD. Sometimes core R and kazaam will disagree
#' slightly in what the rotated variables are because of how the SVD is
#' caluclated.
#' 
#' @section Communication:
#' The communication is an \code{allreduce()} call, quadratic on the number of
#' columns.  Most of the run time should be dominated by relatively expensive
#'  local operations.
#' 
#' @param x 
#' A shaq.
#' @param center 
#' Should columns are zero centered?
#' @param scale. 
#' Should columns are rescaled to unit variance?
#' @param retx 
#' Should the rotated variables be returned?
#' @param tol 
#' The cutoff for components.
#' @param ...
#' Ignored.
#' 
#' @return 
#' A list of elements \code{sdev}, \code{rotation}, \code{center}, \code{scale},
#' and \code{x}, as with R's own \code{prcomp()}.  The elements are,
#' respectively, a regular vector, a regular matrix, a regular vector, a regular
#' vector, and a shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' pca = prcomp(x)
#' 
#' comm.print(pca)
#' 
#' finalize()
#' }
#' 
#' @name prcomp
#' @rdname prcomp
#' @export
prcomp.shaq = function(x, retx=TRUE, center=TRUE, scale.=FALSE, tol=NULL, ...)
{
  x <- scale(x, center=center, scale=scale.)
  x.center <- attr(DATA(x), "scaled:center")
  x.scale <- attr(DATA(x) , "scaled:scale")
  if (any(x.scale == 0))
      comm.stop("cannot rescale a constant/zero column to unit variance")
  
  s <- svd(x, nu=0)
  s$d <- s$d/sqrt(max(1, nrow(x) - 1))
  
  if (!is.null(tol))
  {
    rank <- max(as.integer(sum(s$d > (s$d[1L] * tol))), 1L)
    if (rank < ncol(x))
    {
      s$v <- s$v[, 1L:rank, drop=FALSE]
      s$d <- s$d[1L:rank]
    }
  }
  
  if (is.null(x.center))
    center = FALSE
  else
    center = x.center
  
  if (is.null(x.scale))
    scale = FALSE
  else
    scale = x.scale
  
  r <- list(sdev=s$d, rotation=s$v, center=center, scale=scale)
  if (retx)
    r$x <- x %*% s$v
  
  class(r) <- "prcomp"
  
  return(r)
}
