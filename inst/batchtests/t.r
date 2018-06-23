suppressPackageStartupMessages(library(kazaam))

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
} else {
  x = NULL
}


dx = expand(x)
tdx = t(dx)
tx = collapse(tdx)

comm.print(all.equal(tx, t(x)))

finalize()
