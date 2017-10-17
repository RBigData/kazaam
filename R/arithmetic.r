#' Arithmetic Operators
#' 
#' Some binary arithmetic operations for shaqs.  All operations are
#' vector-shaq or shaq-vector, but not shaq-shaq.  See details section for more
#' information.
#' 
#' @details
#' For binary operations involving two shaqs, they must be distributed
#' \emph{identically}.
#' 
#' @section Communication:
#' Each operation is completely local.
#' 
#' @param e1,e2
#' A shaq or a numeric vector.
#' 
#' @return 
#' A shaq.
#' 
#' @examples
#' \dontrun{
#' library(kazaam)
#' x = ranshaq(runif, 10, 3)
#' y = ranshaq(runif, 10, 3)
#' 
#' x + y
#' x / 2
#' y + 1
#' 
#' finalize()
#' }
#' 
#' @name arithmetic
#' @rdname arithmetic
NULL



bounds.check = function(shaq, vec)
{
  if (length(vec) != 1)
    comm.stop("invalid shaq-vector operation: vector must be length 1")
}

shaqshaq.check = function(s1, s2)
{
  if (nrow(s1) != nrow(s1)  ||  ncol(s1) != ncol(s2))
    stop("non-conformable arrays")
  
  if (nrow(DATA(s1)) != nrow(DATA(s2))  ||  ncol(DATA(s1)) != ncol(DATA(s2)))
    stop("shaqs not distributed identically")
}



#' @rdname arithmetic
#' @export
setMethod("+", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) + DATA(e2)
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("+", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) + e2
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
setMethod("-", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) - DATA(e2)
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("-", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) - e2
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("-", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    bounds.check(e2, e1)
    
    DATA(e2) = e1 - DATA(e2)
    e2
  }
)



#' @rdname arithmetic
#' @export
setMethod("*", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) * DATA(e2)
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("*", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) * e2
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
setMethod("/", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) / DATA(e2)
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("/", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) / e2
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("/", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    bounds.check(e2, e1)
    
    DATA(e2) = e1 / DATA(e2)
    e2
  }
)



#' @rdname arithmetic
#' @export
setMethod("^", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) ^ DATA(e2)
    e1
  }
)

#' @rdname arithmetic
#' @export
setMethod("^", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) ^ e2
    e1
  }
)
