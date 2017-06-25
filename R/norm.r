#' norm
#' 
#' Implementation of R's \code{norm()} function for shaq objects.
#' 
#' @details
#' TODO
#' 
#' @section Communication:
#' TODO
#' 
#' @param x
#' A shaq
#' @param type
#' The type of norm: one, infinity, frobenius, max-modulus, and spectral.
#' 
#' @return
#' A number (length 1 regular vector).
#' 
#' @examples
#' \dontrun{
#' TODO
#' }
#' 
#' @name norm
#' @rdname norm
NULL



norm.shaq = function(x, type = c("O", "I", "F", "M", "2"))
{
  type = toupper(type)
  
  if (type %in% c("O", "1"))
  {
    cs_abs = base::colSums(abs(Data(x)))
    max(allreduce(cs_abs))
  }
  else if (type == "I")
  {
    comm.max(norm(Data(x), type="I"))
  }
  else if (type == "F")
  {
    tmp = norm(Data(x), type="F")
    sqrt(allreduce(tmp*tmp))
  }
  else if (type == "M")
  {
    comm.max(max(abs(Data(x))))
  }
  else if (type == "2")
    svd(x, 0, 0)$d[1L]
  else
    comm.stop("")
}



#' @rdname norm
#' @export
setMethod("norm", signature(x="shaq"), norm.shaq)
