# the generic allreduce() is very expensive for iterative components like our
# svm and glm fitters
allreduce_dbl = function(x, x.buffer=numeric(length(x)), op="sum", comm=0)
{
  ret = pbdMPI::spmd.allreduce.double(x, x.buffer, op, comm)
  if (is.matrix(x))
    dim(ret) = dim(x)
  
  ret
}

allreduce_int = function(x, x.buffer=integer(length(x)), op="sum", comm=0)
{
  ret = pbdMPI::spmd.allreduce.integer(x, x.buffer, op, comm)
  if (is.matrix(x))
    dim(ret) = dim(x)
  
  ret
}

# allreduce_float = function(x, x.buffer=fl(integer(length(x))), op="sum", comm=0)
# {
#   ret = spmd.allreduce.float(x, x.buffer, op, comm)
#   if (is.matrix(x))
#     dim(ret) = dim(x)
#   
#   ret
# }

MPI_Allreduce = function(x, x.buffer, op="sum", comm=0)
{
  if (is.double(x))
  {
    if (missing(x.buffer))
      x.buffer = numeric(length(x))
    
    allreduce_dbl(x, x.buffer, op, comm)
  }
  else if (is.integer(x))
  {
   if (missing(x.buffer))
      x.buffer = integer(length(x))
    
    allreduce_int(x, x.buffer, op, comm)
  }
  # else if (is.float(x))
  # {
  #   if (missing(x.buffer))
  #     x.buffer = fl(integer(length(x)))
  #   
  #   allreduce_float(x, x.buffer, op, comm)
  # }
}
