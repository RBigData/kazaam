#' setters
#' 
#' Setter functions for shaq objects.  Generally not recommended unless you are
#' sure you know what you're doing.
#' 
#' @details
#' \code{Data<-} will perform checks on the inserted data and ensure that the
#' number of columns match across processors (requiring communication).  It will
#' also udpate the number of rows as necessary.
#' 
#' \code{DATA<-} will perform no checks, so use only if you're really sure that
#' you know what you're doing.
#' 
#' @section Communication:
#' With \code{Data<-}, a check on the global number of rows is performed.  This
#' amounts to an allgather operation on a logical value (the local dimension
#' check).
#' 
#' @param x
#' A shaq.
#' @param value
#' The new data.
#' 
#' @seealso \code{\link{getters}}, \code{\link{bracket}}
#' @name setters
#' @rdname setters
NULL



setter = function(x, value, checks)
{
  if (checks)
  {
    nrows = check_shaq(DATA(x), nrow(x), ncol(x))
    x@nrows = nrows
  }
  
  x@Data = value
  
  x
}

Data_set_shaq = function(x, value) setter(x, value, checks=TRUE)

DATA_set_shaq = function(x, value) setter(x, value, checks=FALSE)



#' @rdname setters
#' @export
setGeneric(name="DATA<-", useAsDefault=DATA_set_shaq, package="kazaam")

#' @rdname setters
#' @export
setMethod("DATA<-", signature(x="shaq"), DATA_set_shaq)
