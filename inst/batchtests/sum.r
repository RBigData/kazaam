suppressPackageStartupMessages(library(kazaam))

y = 1:5 # deliberately global

if (comm.rank() == 0){
  x = matrix(1:30, 10)
  z = matrix(rnorm(14), 2)
} else {
  x = NULL
}


dx = expand(x)
dz = expand(z)

test = sum(dx)
comm.print(all.equal(test, sum(x)))

test = sum(dx, y)
comm.print(all.equal(test, sum(x, y)))

test = sum(dx, y, dz)
comm.print(all.equal(test, sum(x, y, z)))

finalize()
