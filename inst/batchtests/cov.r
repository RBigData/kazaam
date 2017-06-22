suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  set.seed(1234)
  x = matrix(rnorm(30), 10)
  cv = cov(x)
  cr = cor(x)
} else {
  x = NULL
}


dx = expand(x)

cv_test = cov(dx)
cr_test = cor(dx)

comm.print(all.equal(cv, cv_test))
comm.print(all.equal(cr, cr_test))


finalize()
