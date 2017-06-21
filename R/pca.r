#' Principal Components Analysis
#' 
#' Performs the principal components analysis.
#' 
#' @details
#' \code{prcomp()} performs the principal components analysis on the data
#' matrix by taking the SVD. Sometimes core R and pbdSHAQ will disagree
#' slightly in what the rotated variables are because of how the SVD is
#' caluclated.
#' 
#' @param x 
#' A shaq.
#' @param center 
#' logical value, determines whether or not columns are zero centered
#' @param scale. 
#' logical value, determines whether or not columns are rescaled to unit variance
#' @param retx 
#' logical value, indicates whether the rotated variables should be returned
#' @param tol 
#' a value indicating the magnitude below which components should be
#' omitted. (Components are omitted if their standard deviations are less than
#' or equal to \code{tol} times the standard deviation of the first component.)
#' With the default null setting, no components are omitted.  Other settings
#' for tol could be \code{tol = 0} or \code{tol = sqrt(.Machine$double.eps)},
#' which would omit essentially constant components
#' @param ...
#' Ignored.
#' 
#' @return 
#' TODO 
#' 
#' @name prcomp
#' @rdname prcomp
NULL



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



#' @rdname prcomp
#' @export
setGeneric(name="prcomp", useAsDefault=stats::prcomp, package="pbdSHAQ")

#' @rdname prcomp
#' @export
setMethod("prcomp", signature(x="shaq"), prcomp.shaq)
