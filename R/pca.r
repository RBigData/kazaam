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
#' @param x 
#' A shaq.
#' @param center 
#' Should columns are zero centered?
#' @param scale. 
#' Should columns are rescaled to unit variance?
#' @param retx 
#' Should the rotated variables be returned?
#' @param tol 
#' Ignored.
#' @param ...
#' Ignored.
#' 
#' @return 
#' A list of elements \code{sdev}, \code{rotation}, \code{center}, \code{scale},
#' and \code{x}, as with R's own \code{prcomp()}.  The elements are,
#' respectively, a regular vector, a regular matrix, a regular vector, a regular
#' vector, and a shaq.
#' 
#' @name prcomp
#' @rdname prcomp
#' @export
prcomp.shaq = function(x, retx=TRUE, center=TRUE, scale.=FALSE, tol=NULL, ...)
{
  x <- scale(x, center=center, scale=scale.)
  cen <- attr(Data(x), "scaled:center")
  sc <- attr(Data(x) , "scaled:scale")
  if (any(sc == 0))
      comm.stop("cannot rescale a constant/zero column to unit variance")
  
  s <- svd(x, nu=0)
  s$d <- s$d/sqrt(max(1, nrow(x) - 1))
  # if (!is.null(tol)) {
  #     rank <- max(sum(s$d > (s$d[1L] * tol)), 1)
  #     if (rank < ncol(x)) {
  #         s$v <- s$v[, 1L:rank]
  #         s$d <- s$d[1L:rank]
  #     }
  # }
  center = if (is.null(cen)) FALSE else cen
  scale = if (is.null(sc)) FALSE else sc
  r <- list(sdev=s$d, rotation=s$v, center=center, scale=scale)
  if (retx)
    r$x <- x %*% s$v
  
  class(r) <- "prcomp"
  
  return(r)
}
