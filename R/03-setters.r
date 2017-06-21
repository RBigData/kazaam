#' setters
#' 
#' Setter functions for shaq objects.
#' 
#' @details
#' \code{Data<-} will perform checks on the inserted data and ensure that the
#' number of columns match across processors (requiring communication).  It will
#' also udpate the number of rows as necessary.
#' 
#' \code{DATA<-} will perform no checks, so use with caution.
#' 
#' @param x
#' A shaq.
#' @param value
#' The new data.
#' 
#' @name setters
#' @rdname setters
NULL



setter = function(x, value, checks)
{
  if (checks)
  {
    nrows = check.shaq(Data(x), nrows(x), ncols(x))
    x@nrows = nrows
  }
  
  x@Data = value
}

Data_set.shaq = function(x, value) setter(x, value, checks=TRUE)


DATA_set.shaq = function(x, value) setter(x, value, checks=FALSE)

#' @rdname setters
#' @export
setGeneric(name="Data<-", useAsDefault=Data_set.shaq, package="pbdSHAQ")

#' @rdname setters
#' @export
setMethod("Data<-", signature(x="shaq"), Data_set.shaq)

#' @rdname setters
#' @export
setGeneric(name="DATA<-", useAsDefault=DATA_set.shaq, package="pbdSHAQ")

#' @rdname setters
#' @export
setMethod("DATA<-", signature(x="shaq"), DATA_set.shaq)
