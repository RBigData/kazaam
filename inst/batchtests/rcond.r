suppressPackageStartupMessages(library(kazaam))
comm.set.seed(1234)

tester = function(x, dx)
{
  norm = "O"
  test = rcond(dx, norm)
  comm.print(all.equal(rcond(x, norm), test))
  
  norm = "I"
  test = rcond(dx, norm)
  comm.print(all.equal(rcond(x, norm), test))
  
  norm = "1"
  test = rcond(dx, norm)
  comm.print(all.equal(rcond(x, norm), test))
}

dx = ranshaq(runif, 10, 3)
x = collapse(dx)
tester(x, dx)

dx = expand(matrix(1:30, 10))
x = collapse(dx)
tester(x, dx)

finalize()
