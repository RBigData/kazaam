suppressPackageStartupMessages(library(kazaam))
comm.set.seed(1234, diff=TRUE)

tol = 5e-4

dx = ranshaq(rnorm, 10, 3)
dy_gaussian = ranshaq(runif, 10)
dy_logistic = ranshaq(function(i) sample(0:1, size=i, replace=TRUE), 10)
dy_poisson = ranshaq(function(i) sample(1:5, size=i, replace=TRUE), 10)

fit_gaussian = reg.fit(dx, dy_gaussian)
fit_logistic = logistic.fit(dx, dy_logistic)
fit_poisson = poisson.fit(dx, dy_poisson)

x = collapse(dx)
y_gaussian = collapse(dy_gaussian)
y_logistic = collapse(dy_logistic)
y_poisson = collapse(dy_poisson)

comm.print(all.equal(fit_gaussian$par, lm.fit(x, y_gaussian)$coefficients, check.attributes=FALSE, tol=tol))
comm.print(all.equal(fit_logistic$par, glm.fit(x, y_logistic, family=binomial(logit))$coefficients, check.attributes=FALSE, tol=tol))
comm.print(all.equal(fit_poisson$par, glm.fit(x, y_poisson, family=poisson(log))$coefficients, check.attributes=FALSE, tol=tol))


finalize()
