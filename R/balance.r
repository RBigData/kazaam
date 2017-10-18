#' Load Balancing
#' 
#' Re-distribute a shaq so that the data is "balanced" (in one of two ways; see
#' Details section for more information). This can be useful when, for example,
#' reading data onto multiple MPI ranks from a parallel file system, and then
#' broadcasting/redistributing the data out to the other ranks.
#' 
#' @details
#' There are two separate interfaces. The functions \code{balance()} and
#' \code{unbalance()} are the "high level" interface, and are appropriate for
#' most use cases. However, for custom redistributions, the functions
#' \code{balance.info()} and \code{.balance()} can be used. Use at your own
#' risk.
#' 
#' \code{balance()} will rearrange the data distributed across the MPI ranks in
#' one of two ways. The first is in a "locally load balanced" (llb) way. This
#' means that each rank is given roughly the same amount of data. For example,
#' balancing 6 rows across 4 MPI ranks would leave 2 rows on ranks 0 and 1 and
#' 1 row on ranks 3 and 4. The other method of distribution is "block-cyclic"
#' (bc). This will put the data into a 1-dimensional block-cyclic distribution
#' so that the data could be handed off to pbdDMAT. See that package's
#' documentation for more details.
#' 
#' @param x
#' A shaq.
#' @param X.gbd
#' A matrix.
#' @param comm 
#' An MPI communicator number.
#' @param bal.info
#' A returned object from \code{balance.info()}.
#' @param gbd.major
#' 1 for row-major storage, 2 for column-major.
#' @param method
#' One of "llb" (locally load balanced) "bc" (block-cyclic).
#' 
#' @examples
#' \dontrun{
#' # Run with 4 MPI ranks
#' suppressMessages(library(kazaam, quietly=TRUE))
#' comm.set.seed(1234, diff=TRUE)
#' 
#' n = sample(10, size=1)
#' d = matrix(comm.rank(), n, 3)
#' 
#' x = shaq(d)
#' x
#' 
#' y = balance(x)
#' y
#' 
#' finalize()
#' }
#' 
#' @author Wei-Chen Chen and Drew Schmidt
#' 
#' @name load_balance
#' @rdname load_balance
NULL



#' @rdname load_balance
#' @export
balance.info <- function(X.gbd, comm=0, gbd.major=1L, method="llb")
{
  method = pbdMPI::comm.match.arg(method, c("llb", "bc"))
  
  COMM.SIZE <- comm.size(comm)
  COMM.RANK <- comm.rank(comm)
  
  if (gbd.major == 1)
    N.gbd <- nrow(X.gbd)
  else if (gbd.major == 2)
    N.gbd <- ncol(X.gbd)
  else
    comm.stop("gbd.major = 1 or 2.", comm = comm)
  
  N.allgbd <- pbdMPI::spmd.allgather.integer(as.integer(N.gbd), integer(COMM.SIZE), comm = comm)
  N <- sum(N.allgbd)
  
  if (method[1] == "bc")
  {
    n <- ceiling(N / COMM.SIZE)
    rep.n <- N %/% n
    new.N.allgbd <- rep(n, rep.n)
    if (n * rep.n < N)
      new.N.allgbd <- c(new.N.allgbd, (N - n * rep.n))
    
    if (length(new.N.allgbd) < COMM.SIZE)
      new.N.allgbd <- c(new.N.allgbd, rep(0, COMM.SIZE - length(new.N.allgbd)))
    
    rank.belong <- rep(0:(COMM.SIZE - 1), new.N.allgbd) 
  }
  else if (method[1] == "llb")
  {
    n <- floor(N / COMM.SIZE)
    n.residual <- N %% COMM.SIZE
    new.N.allgbd <- rep(n, COMM.SIZE) + rep(c(1, 0), c(n.residual, COMM.SIZE - n.residual))
    rank.belong <- rep(0:(COMM.SIZE - 1), new.N.allgbd)
  }
  
  rank.org <- rep(0:(COMM.SIZE - 1), N.allgbd)
  
  ### Build send and recv information if any.
  send.info <- data.frame(
    org = rank.org[rank.org == COMM.RANK],
    belong = rank.belong[rank.org == COMM.RANK]
  )
  recv.info <- data.frame(
    org = rank.org[rank.belong == COMM.RANK],
    belong = rank.belong[rank.belong == COMM.RANK]
  )
  
  list(
    comm = comm,
    send = send.info,
    recv = recv.info,
    N.allgbd = N.allgbd,
    new.N.allgbd = new.N.allgbd,
    gbd.major = gbd.major
  )
}



#' @rdname load_balance
#' @export
.balance <- function(X.gbd, bal.info=NULL, gbd.major=1L)
{
  if (is.null(bal.info))
    bal.info <- balance.info(X.gbd, gbd.major = gbd.major)
  
  COMM.RANK <- comm.rank()
  comm = bal.info$comm
  
  if (!is.matrix(X.gbd))
    X.gbd <- as.matrix(X.gbd)
  
  if (gbd.major == 1)
    p <- ncol(X.gbd)
  else if (gbd.major == 2)
    p <- nrow(X.gbd)
  else
    comm.stop("gbd.major = 1 or 2.", comm = comm)
  
  storage.mode(X.gbd) <- "double"
  
  send.to <- as.integer(unique(bal.info$send$belong))
  if (length(send.to) > 0)
  {
    if (gbd.major == 1)
    {
      for (i in send.to)
      {
        if (i != COMM.RANK)
        {
          tmp <- matrix(X.gbd[bal.info$send$belong == i,], ncol = p)
          pbdMPI::spmd.isend.double(tmp, rank.dest = i, tag = COMM.RANK, comm = comm)
        }
      }
    }
    else
    {
      for (i in send.to)
      {
        if (i != COMM.RANK)
        {
          tmp <- matrix(X.gbd[, bal.info$send$belong == i], nrow = p)
          pbdMPI::spmd.isend.double(tmp, rank.dest = i, tag = COMM.RANK, comm = comm)
        }
      }
    }
  }
  
  recv.from <- as.integer(unique(bal.info$recv$org))
  if (length(recv.from) > 0)
  {
    ret <- NULL
    if (gbd.major == 1)
    {
      for (i in recv.from)
      {
        if (i != COMM.RANK)
        {
          total.row <- sum(bal.info$recv$org == i)
          tmp <- pbdMPI::spmd.recv.double(double(total.row * p),
                                  rank.source = i, tag = i, comm = comm)
          dim(tmp) <- c(total.row, p)
        }
        else
          tmp <- matrix(X.gbd[bal.info$send$belong == i,], ncol = p)
        
        ret <- base::rbind(ret, tmp)
      }
    }
    else
    {
      for (i in recv.from)
      {
        if (i != COMM.RANK)
        {
          total.column <- sum(bal.info$recv$org == i)
          tmp <- pbdMPI::spmd.recv.double(double(total.column * p), rank.source = i, tag = i, comm = comm)
          dim(tmp) <- c(p, total.column)
        }
        else
          tmp <- matrix(X.gbd[, bal.info$send$belong == i], nrow = p)
        
        ret <- base::cbind(ret, tmp)
      }
    }
  }
  else
    ret <- X.gbd
  
  
  if (bal.info$new.N.allgbd[pbdMPI::spmd.comm.rank(comm) + 1] == 0)
  {
    if (gbd.major == 1)
      ret <- matrix(0, nrow = 0, ncol = p)
    else
      ret <- matrix(0, nrow = p, ncol = 0)
  }
  
  pbdMPI::wait()
  
  ret
}



#' @rdname load_balance
#' @export
unbalance <- function(x, bal.info)
{
  if (missing(bal.info))
    comm.stop("argument 'bal.info' is missing, with no default")
  if (!is.shaq(x))
    comm.stop("argument 'x' must be a shaq")
  
  rev.bal.info <- list(
    send = data.frame(
      org = bal.info$recv$belong,
      belong = bal.info$recv$org
    ),
    recv = data.frame(
      org = bal.info$send$belong,
      belong = bal.info$send$org
    ),
    N.allgbd = bal.info$new.N.allgbd,
    new.N.allgbd = bal.info$N.allgbd,
    gbd.major = bal.info$gbd.major
  )
  
  .balance(DATA(x), bal.info = rev.bal.info)
}



#' @rdname load_balance
#' @export
balance = function(x, method="llb")
{
  if (!is.shaq(x))
    comm.stop("argument 'x' must be a shaq")
  
  bal.info = balance.info(DATA(x), comm=0L, gbd.major=1L, method=method)
  Data = .balance(DATA(x), bal.info=bal.info, gbd.major=1L)
  
  shaq(Data, NROW(x), NCOL(x), checks=FALSE)
}
