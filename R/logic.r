#' Logical Operators
#' 
#' Binary logical operators.
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
#' x < y
#' 
#' finalize()
#' }
#' 
#' @name logic
#' @rdname logic
NULL



#' @rdname logic
#' @export
setMethod("<", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    binary.shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) < DATA(e2)
    e1
  }
)

#' @rdname logic
#' @export
setMethod("<", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    binary.bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) < e2
    e1
  }
)

#' @rdname logic
#' @export
setMethod("<", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    binary.bounds.check(e2, e1)
    
    DATA(e2) = e1 < DATA(e2)
    e2
  }
)



#' @rdname logic
#' @export
setMethod(">", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    binary.shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) > DATA(e2)
    e1
  }
)

#' @rdname logic
#' @export
setMethod(">", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    binary.bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) > e2
    e1
  }
)

#' @rdname logic
#' @export
setMethod(">", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    binary.bounds.check(e2, e1)
    
    DATA(e2) = e1 > DATA(e2)
    e2
  }
)



#' @rdname logic
#' @export
setMethod("<=", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    binary.shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) <= DATA(e2)
    e1
  }
)

#' @rdname logic
#' @export
setMethod("<=", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    binary.bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) <= e2
    e1
  }
)

#' @rdname logic
#' @export
setMethod("<=", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    binary.bounds.check(e2, e1)
    
    DATA(e2) = e1 <= DATA(e2)
    e2
  }
)



#' @rdname logic
#' @export
setMethod(">=", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    binary.shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) >= DATA(e2)
    e1
  }
)

#' @rdname logic
#' @export
setMethod(">=", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    binary.bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) >= e2
    e1
  }
)

#' @rdname logic
#' @export
setMethod(">=", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    binary.bounds.check(e2, e1)
    
    DATA(e2) = e1 >= DATA(e2)
    e2
  }
)



#' @rdname logic
#' @export
setMethod("==", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    binary.shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) == DATA(e2)
    e1
  }
)

#' @rdname logic
#' @export
setMethod("==", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    binary.bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) == e2
    e1
  }
)

#' @rdname logic
#' @export
setMethod("==", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    e2 == e1
  }
)



#' @rdname logic
#' @export
setMethod("!=", signature(e1="shaq", e2="shaq"), 
  function(e1, e2)
  {
    binary.shaqshaq.check(e1, e2)
    
    DATA(e1) = DATA(e1) != DATA(e2)
    e1
  }
)

#' @rdname logic
#' @export
setMethod("!=", signature(e1="shaq", e2="numeric"), 
  function(e1, e2)
  {
    binary.bounds.check(e1, e2)
    
    DATA(e1) = DATA(e1) != e2
    e1
  }
)

#' @rdname logic
#' @export
setMethod("!=", signature(e1="numeric", e2="shaq"), 
  function(e1, e2)
  {
    binary.bounds.check(e2, e1)
    
    DATA(e2) = e1 != DATA(e2)
    e2
  }
)
