suppressPackageStartupMessages(library(kazaam))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
} else {
  x = NULL
}

tester = function(x, i, j)
{
  if (comm.rank() == 0)
    x_true = x[, j, drop=FALSE]
  dx = expand(x)
  x_test = collapse(dx[, j])
  comm.print(all.equal(x_true, x_test))
}


tester(x, j=-1)
tester(x, j=c(-1, -3))

tester(x, j=2)
tester(x, j=c(1, 3))

tester(x, j=c(F, F, F))
tester(x, j=c(T, F, T))



tester = function(x, i, j, value)
{
  if (comm.rank() == 0)
  {
    x_true = x
    x_true[, j] <- value
  }
  dx = expand(x)
  ### some care is needed here for reasons I don't entirely understand
  x_test = dx
  dx[, j] <- value
  x_test = collapse(dx)
  comm.print(all.equal(x_true, x_test))
}

tester(x, j=-1, value=1)
tester(x, j=1, value=1)
tester(x, j=c(1, 3), value=1)
tester(x, j=c(-1, -3), value=1)


finalize()
