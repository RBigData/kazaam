suppressPackageStartupMessages(library(kazaam))

y = matrix(c(-1, 1, 0, 1), 2)


if (comm.rank() == 0){
  x = matrix(rnorm(30), 15)
  mm = x %*% y
} else {
  x = NULL
}



dx = expand(x)

mm_test = collapse(dx %*% y)

comm.print(all.equal(mm, mm_test))

finalize()
