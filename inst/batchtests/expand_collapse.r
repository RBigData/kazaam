suppressPackageStartupMessages(library(kazaam))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
} else {
  x = NULL
}


dx = expand(x)
y = collapse(dx)

comm.print(all.equal(x, y))



finalize()
