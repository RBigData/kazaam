#' rand_measure
#' 
#' Returns the Rand measure, which is a statistic for comparing two data
#' clusterings.
#' 
#' @param l1,l2
#' Two vectors or shaqs (with 1 column each) of the same length. If shaqs, they
#' must be distributed in the exact same way.
#' 
#' @return
#' The rand measure (global).
#' 
#' @references
#' Rand Index \url{https://en.wikipedia.org/wiki/Rand_index}
#' 
#' @export
rand_measure = function(l1, l2)
{
  if (is.shaq(l1) && is.shaq(l2))
    R = rand_measure_shaq(l1, l2)
  else if (isavec(l1) && isavec(l2))
    R = rand_measure_atomic(l1, l2)
  else
    comm.stop("arguments 'l1' and 'l2' should either both be numeric vectors or both be shaqs")
  
  R
}



isavec = function(x) (is.atomic(x) && is.vector(x))



rand_measure_ab = function(n, l1, l2)
{
  a = b = 0.0
  
  for (i in 1:n)
  {
    for (j in (i+1L):n)
    {
      if (j > n) # R indexing is stupid
        break
      
      same1 = (l1[i] == l1[j])
      same2 = (l2[i] == l2[j])
      
      if (same1 && same2)
        a = a + 1.0
      else if (!same1 && !same2)
        b = b + 1.0
    }
  }
  
  c(a, b)
}



rand_measure_atomic = function(l1, l2)
{
  n = length(l1)
  if (n != length(l2))
    comm.stop("'l1' and 'l2' must be the same length")
  
  ab = rand_measure_ab(n, l1, l2)
  
  a = ab[1]
  b = ab[2]
  
  R = (a + b) / choose(n, 2)
  R
}



rand_measure_shaq = function(l1, l2)
{
  n = nrow(l1)
  if (nrow(l2) != n || ncol(l1) != 1 || ncol(l2) != 2)
    comm.stop("")
  
  ab = rand_measure_ab(n, l1, l2)
  ab = allreduce_dbl(ab)
  
  a = ab[1]
  b = ab[2]
  
  R = (a + b) / choose(n, 2)
  R
}
