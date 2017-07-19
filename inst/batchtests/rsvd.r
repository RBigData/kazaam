suppressPackageStartupMessages(library(kazaam))
comm.set.seed(1234)

if (comm.rank() == 0){
  tol = 1e-2
  
  x = matrix(rnorm(30), 10)
  svd = svd(x)
  d = svd$d
  u = svd$u
  v = svd$v
} else {
  x = NULL
}


dx = expand(x)
k = 1

svd_test = rsvd(dx, k=k)
u_test = collapse(svd_test$u)
comm.print(all.equal(svd_test$d, svd$d[1:k], tol=tol))
comm.print(all.equal(abs(u_test), abs(svd$u[, 1:k, drop=FALSE]), tol=tol))
comm.print(all.equal(abs(svd_test$v), abs(svd$v[, 1:k, drop=FALSE]), tol=tol))

finalize()
