#' QR Decomposition Methods
#' 
#' 
#' @param x
#' A shaq.
#' @param R
#' A regular matrix. This argument is optional, in that if it is not supplied
#' explicitly, then it will be computed in the background.  So if you already
#' have R lying around, supplying it here will improve performance.
#' 
#' @return 
#' Q (a shaq) or R (a regular matrix).
#' 
#' @name qr
#' @rdname qr
NULL



#' @rdname qr
#' @export
qr_R = function(x)
{
  cp = cp.shaq(x)
  R = chol(cp)
  
  R
}



#' @rdname qr
#' @export
qr_Q = function(x, R)
{
  if (missing(R))
    R = qr_R(x)
  
  Q.local = Data(x) %*% solve(R)
  
  shaq(Q.local, nrow(x), ncol(x), checks=FALSE)
}
