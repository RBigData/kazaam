add1 = function(x) .Call(R_add1, x)

memcpy = function(dest, src) .Call(R_memcpy, dest, src)

get_numbefore = function(x)
{
  allm.local = unlist(pbdMPI::allgather(nrow(Data(x))))
  numbefore = c(0, cumsum(allm.local)[1:(length(allm.local)-1)])
  numbefore[comm.rank() + 1L]
}

get_random_seed = function()
{
  if (comm.rank() == 0)
    seed = bitwXor(Sys.getpid(), as.integer(Sys.time()))
  else
    seed = 0L
  
  allreduce(seed)
}



km.assign = function(x, centers, labels)
{
  .Call(R_km_assign, Data(x), centers, labels)
}



km.update = function(x, centers, labels)
{
  .Call(R_km_update, Data(x), centers, labels)
}



# take random points from x
km.init = function(x, k, numbefore)
{
  m = nrow(x)
  m.local = nrow(Data(x))
  
  rows = sort(sample(1:m, size=k, replace=FALSE))
  centers.rows.local = numbefore < rows & rows <= numbefore+m.local
  
  x.local.ind = rows[which(centers.rows.local)] - numbefore
  centers.local = t(Data(x)[x.local.ind, ])
  
  centers = matrix(0, ncol(x), k)
  centers[, centers.rows.local] = centers.local
  allreduce(centers)
}



#' km
#' 
#' k-means via Lloyd's Algorithm.
#' 
#' Note that the function does not respect \code{set.seed()} or
#' \code{comm.set.seed()}.  For managing random seeds, use the \code{seed}
#' parameter.
#' 
#' @details
#' The iterations stop either when the maximum number of iterations have been
#' achieved, or when the centers in the current iteration are basically the same
#' (within \code{1e-8}) as the centers from the previous iteration.
#' 
#' For best performance, the data should be as balanced as possible across all
#' MPI ranks.
#' 
#' @section Communication:
#' Most of the computation is local. However, at each iteration there is a
#' length \code{n*k} and a length \code{k} allreduce call to update the centers.
#' There is also a check at the beginning of the call to find out how many
#' observations come before the current process's data, which is an allgather
#' operation.
#' 
#' @param x
#' A shaq.
#' @param k
#' The 'k' in k-means.
#' @param maxiter
#' The maximum number of iterations possible.
#' @param seed
#' A seed for determining the (random) initial centroids.  Each process has to
#' use the same seed or very strange things may happen.  If you do not provide
#' a seed, a good initial seed will be chosen.
#' 
#' @return
#' A list containing the cluster centers (global), the observation labels i.e.
#' the assignments to clusters (distributed shaq), and the total number of
#' iterations (global).
#' 
#' @examples
#' \dontrun{
#' suppressMessages(library(kazaam))
#' set.seed(1234)
#' 
#' m.local = 10
#' n = 2
#' k = comm.size()
#' data = matrix(rnorm(m.local*n, mean=10*comm.rank()), m.local, n)
#' x = shaq(data)
#' 
#' cl = km(x, k=k)
#' cl
#' 
#' finalize()
#' }
#' 
#' @references
#' TODO
#' 
#' @export
km = function(x, k=2, maxiter=100, seed=get_random_seed())
{
  pbdMPI::comm.set.seed(seed, diff=FALSE)
  
  numbefore = get_numbefore(x)
  
  centers = km.init(x, k, numbefore)
  centers.old = matrix(0.0, nrow(centers), ncol(centers))
  memcpy(centers.old, centers)
  
  labels = integer(nrow(Data(x)))
  km.assign(x, centers, labels)
  
  for (iter in 1:maxiter)
  {
    km.update(x, centers, labels)
    km.assign(x, centers, labels)
    
    if (isTRUE(all.equal(centers, centers.old)))
      break
    
    memcpy(centers.old, centers)
  }
  
  add1(labels)
  
  labels.shaq = shaq(matrix(labels), ncols=1L, checks=FALSE)
  list(centers=centers, labels=labels.shaq, iterations=iter)
}
