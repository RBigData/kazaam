suppressPackageStartupMessages(library(kazaam))

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

svd_test = svd(dx)

d_test = svd_test$d
u_test = collapse(svd_test$u)
v_test = svd_test$v

comm.print(all.equal(d, d_test))

# the direction of the vectors can differ, so we have to account for sign
comm.print(all.equal(abs(u), abs(u_test)))
comm.print(all.equal(abs(v), abs(v_test)))



x = rantshaq(stats::rnorm, 3, 10)
v = svd(x)$v
vt = La.svd(x)$vt
comm.print(all.equal(abs(DATA(v)), abs(DATA(t(vt)))))

x = t(x)
v = svd(x)$v
vt = La.svd(x)$vt
comm.print(all.equal(abs(v), abs(t(vt))))


finalize()
