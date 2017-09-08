euc_norm = function(x)
{
  dim(x) = c(length(x), 1L) # eye rolling emoji
  norm(x, type="F")
}

get_numbefore = function(x)
{
  allm.local = unlist(allgather(nrow(Data(x))))
  numbefore = c(0, cumsum(allm.local)[1:(length(allm.local)-1)])
  numbefore[comm.rank() + 1L]
}

find_index_of_closest = function(x, Y)
{
  dists = sapply(1:ncol(Y), function(j) euc_norm(x - Y[, j]))
  which.min(dists)
}



km.assign = function(x, centers)
{
  sapply(1:nrow(Data(x)), function(i) find_index_of_closest(Data(x)[i, ], centers))
}



km.update = function(x, centers, labels)
{
  k = ncol(centers)
  
  n.labels = sapply(1:k, function(i) length(which(labels == i)))
  n.labels = allreduce(n.labels)
  
  centers = matrix(0, ncol(x), k)
  for (lab in 1:k)
  {
    cs = colSums(Data(x)[which(labels==lab), , drop=FALSE])
    if (length(cs) > 0)
      centers[, lab] = cs
  }
  
  centers = allreduce(centers)
  sweep(centers, MARGIN=2, STATS=n.labels, FUN='/')
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
km = function(x, k=2, maxiter=100)
{
  
  numbefore = get_numbefore(x)
  
  centers = centers.old = km.init(x, k, numbefore)
  labels = km.assign(x, centers)
  
  for (iter in 1:maxiter)
  {
    centers = km.update(x, centers, labels)
    labels = km.assign(x, centers)
    if (isTRUE(all.equal(centers, centers.old)))
      break
    centers.old = centers
  }
  
  labels = shaq(labels, length(labels), 1L, checks=FALSE)
  list(centers=centers, labels=labels, iterations=iter)
}
