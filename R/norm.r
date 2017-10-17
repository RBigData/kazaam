#' norm
#' 
#' Implementation of R's \code{norm()} function for shaq objects.
#' 
#' @details
#' If \code{type == "O"} then the norm is calculated as the maximum of the
#' column sums.
#' 
#' If \code{type == "I"} then the norm is calculated as the maximum absolute
#' value of the row sums.
#' 
#' If \code{type == "F"} then the norm is calculated as the square root of the
#' sum of the square of the values of the matrix.
#' 
#' If \code{type == "M"} then the norm is calculated as the max of the absolute
#' value of the values of the matrix.
#' 
#' If \code{type == "2"} then the norm is calculated as the largest singular
#' value.
#' 
#' @section Communication:
#' If \code{type == "O"} then the communication consists of an allreduce,
#' quadratic on the number of columns.
#' 
#' If \code{type == "I"} then the communication conists of an allgather.
#' 
#' If \code{type == "F"} then the communication is an allreduce, quadratic on
#' the number of columns.
#' 
#' If \code{type == "M"} then the communication consists of an allgather.
#' 
#' If \code{type == "2"} then the communication consists of the same as that of
#' an \code{svd()} call:  an allreduce, quadratic on the number of columns.
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
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' 
#' nm = norm(x)
#' comm.print(nm)
#' 
#' finalize()
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
    cs_abs = base::colSums(abs(DATA(x)))
    max(allreduce(cs_abs))
  }
  else if (type == "I")
  {
    comm.max(norm(DATA(x), type="I"))
  }
  else if (type == "F")
  {
    tmp = norm(DATA(x), type="F")
    sqrt(allreduce(tmp*tmp))
  }
  else if (type == "M")
  {
    comm.max(max(abs(DATA(x))))
  }
  else if (type == "2")
    svd(x, 0, 0)$d[1L]
  else
    comm.stop("")
}



#' @rdname norm
#' @export
setMethod("norm", signature(x="shaq"), norm.shaq)
