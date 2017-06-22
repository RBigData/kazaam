#' Arithmetic Operators
#' 
#' Some binary arithmetic operations for shaqs.  All operations are
#' vector-shaq or shaq-vector, but not shaq-shaq.  See details section for more
#' information.
#' 
#' @details
#' Because the rules imposed on shaq objects are so minimal, it is next to
#' impossible to do some fairly basic things, like adding two shaqs together.
#' If you have two shaqs which are distributed \textit{identically} that you
#' wish to add, you can do so manually with something like
#' \code{DATA(c) <- Data(a) + Data(b)}
#' 
#' @param e1,e2
#' A shaq or a numeric vector.
#' 
#' @return 
#' A shaq.
#' 
#' @name arithmetic
#' @rdname arithmetic
NULL



bounds.check = function(shaq, vec)
{
  if (length(vec) != 1)
    comm.stop("invalid shaq-vector operation: vector must be length 1")
}



#' @rdname arithmetic
#' @export
setMethod("+", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = Data(e1) + e2
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("+", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    e2 + e1
  }
)



#' @rdname arithmetic
#' @export
setMethod("-", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = Data(e1) - e2
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("-", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    bounds.check(e2, e1)
    
    DATA(e2) = e1 - Data(e2)
    e2
  }
)



#' @rdname arithmetic
#' @export
setMethod("*", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = Data(e1) * e2
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("*", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    e2 * e1
  }
)



#' @rdname arithmetic
#' @export
setMethod("/", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = Data(e1) / e2
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("/", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    bounds.check(e2, e1)
    
    DATA(e2) = e1 / Data(e2)
    e2
  }
)
