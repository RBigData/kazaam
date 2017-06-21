prcomp.shaq = function(x, retx=TRUE, center=TRUE, scale.=FALSE, tol=NULL, ...)
{
  x <- scale(x, center=center, scale=scale.)
  cen <- attr(x@Data, "scaled:center")
  sc <- attr(x@Data , "scaled:scale")
  if (any(sc == 0))
      comm.stop("cannot rescale a constant/zero column to unit variance")
  
  s <- svd(x, nu=0)
  s$d <- s$d/sqrt(max(1, x@nrows - 1))
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



#' @export
setGeneric(name = "prcomp", useAsDefault = stats::prcomp, package="pbdSHAQ")

#' @export
setMethod("prcomp", signature(x="shaq"), prcomp.shaq)
