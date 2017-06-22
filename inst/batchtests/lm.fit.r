suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  y = runif(10)
  
  x_degen = x
  x_degen[, 3] = x[, 1]
  
  lm = lm.fit(x, y)$coefficients
  lm_degen = lm.fit(x_degen, y)$coefficients
} else {
  x = NULL
  x_degen = NULL
  y = NULL
}


dx = expand(x)
dx_degen = expand(x_degen)
dy = expand(y)

lm_test = lm_coefs(dx, dy)
### TODO
# lm_degen_test = lm_coefs(dx_degen, dy)

comm.print(lm_test)

comm.print(all.equal(lm, lm_test))
# comm.print(all.equal(lm_degen, lm_degen_test))

finalize()
