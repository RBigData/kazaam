suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  R = qr.R(qr(x))
  Q = qr.Q(qr(x))
} else {
  x = NULL
}


dx = expand(x)

R_test = qr_R(dx)
dQ = qr_Q(dx, R_test)

Q_test = collapse(dQ)

# the direction of the vectors can differ, so we have to account for sign
comm.print(all.equal(abs(R), abs(R_test)))
comm.print(all.equal(abs(Q), abs(Q_test)))

finalize()
