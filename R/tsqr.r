exchange = function(Rin, rank, partner)
{
  if (rank < partner)
    isend(Rin, rank.dest=partner)
  else
    x.new = rbind(Rin, irecv(rank.source=partner))
  
  if (rank > partner)
    isend(Rin, rank.dest=partner)
  else
    x.new = rbind(Rin, irecv(rank.source=partner))
  
  Rout = qr.R(qr(x.new))
  Rout
}



tsqr_R = function(x)
{
  check.is.shaq(x)
  
  rank = comm.rank()
  size = comm.size() # FIXME assume power of 2 for now
  
  R = qr.R(qr(Data(x)))
  
  for (iter in 2^(0:(log2(size)-1)))
  {
    partner = bitwXor(rank, iter)
    
    R = exchange(R, rank, partner)
  }
  
  R
}
