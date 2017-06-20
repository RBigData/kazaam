library(pbdSHAQ)
library(pbdMPI)

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  svd = svd(x)
  d = svd$d
  u = svd$u
  v = svd$v
} else {
  x = NULL
}


dx = expand(x)

svd_test = SVD(dx, TRUE, TRUE)

d_test = svd_test$d
u_test = collapse(svd_test$u)
v_test = svd_test$v

comm.print(all.equal(d, d_test))

# the direction of the vectors can differ, so we have to account for sign
comm.print(all.equal(abs(u), abs(u_test)))
comm.print(all.equal(abs(v), abs(v_test)))

finalize()
