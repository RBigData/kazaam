suppressPackageStartupMessages(library(kazaam))

m = 10L
n = 3L

x = rnorm(m*n)
dim(x) = c(m, n)

R = qr.R(qr(x))

inv.test = kazaam:::.trinv(R)
inv.true = solve(R)
stopifnot(all.equal(inv.test, inv.true))
