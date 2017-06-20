library(pbdSHAQ)
library(pbdMPI)

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  cp = crossprod(x)
} else {
  x = NULL
}


dx = expand(x)

cp_test = crossprod(dx)

comm.print(all.equal(cp, cp_test))

finalize()
