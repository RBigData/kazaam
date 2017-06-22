suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  n_o = norm(x, "O")
  n_i = norm(x, "F")
  n_f = norm(x, "I")
  n_m = norm(x, "M")
  n_2 = norm(x, "2")
} else {
  x = NULL
}


dx = expand(x)

n_o_t = norm(dx, "O")
n_i_t = norm(dx, "F")
n_f_t = norm(dx, "I")
n_m_t = norm(dx, "M")
n_2_t = norm(dx, "2")

comm.print(all.equal(n_o, n_o_t))
comm.print(all.equal(n_i, n_i_t))
comm.print(all.equal(n_f, n_f_t))
comm.print(all.equal(n_m, n_m_t))
comm.print(all.equal(n_2, n_2_t))


finalize()
