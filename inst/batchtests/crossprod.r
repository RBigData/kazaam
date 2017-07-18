suppressPackageStartupMessages(library(kazaam))
comm.set.seed(1234)

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  y = matrix(rnorm(40), 10)
  cp_x = crossprod(x)
  cp_xy = crossprod(x, y)
} else {
  x = NULL
  y = NULL
}

dx = expand(x)
dy = expand(y)

### Tests
test = crossprod(dx)
comm.print(all.equal(cp_x, test))

test = crossprod(dx, dy)
comm.print(all.equal(cp_xy, test))

finalize()
