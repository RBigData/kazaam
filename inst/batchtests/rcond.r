suppressPackageStartupMessages(library(kazaam))
comm.set.seed(1234)

tester = function(x, dx)
{
  comm.print(all.equal(rcond(x), rcond(dx)))
  norm = "O"
  comm.print(all.equal(rcond(x, norm), rcond(dx, norm)))
  norm = "I"
  comm.print(all.equal(rcond(x, norm), rcond(dx, norm)))
  norm = "1"
  comm.print(all.equal(rcond(x, norm), rcond(dx, norm)))
}

dx = ranshaq(runif, 10, 3)
x = collapse(dx)
tester(x, dx)

dx = expand(matrix(1:30, 10))
x = collapse(dx)
tester(x, dx)

finalize()
