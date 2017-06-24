suppressPackageStartupMessages(library(kazaam))


if (comm.rank() == 0){
  m = 10
  x = matrix(1:20, m)
  y = matrix(1:30, m)
  z = matrix(1:10, m)
  c = cbind(x, y, z)
} else {
  x = NULL
  y = NULL
  z = NULL
}

dx = expand(x)
dy = expand(y)
dz = expand(z)

dc = cbind(dx, dy, dz)
c_test = collapse(dc)

comm.print(all.equal(c, c_test))


finalize()
