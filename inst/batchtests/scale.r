suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
  c_tf = scale(x, TRUE, FALSE)
  s_tf = scale(x, FALSE, TRUE)
  cs_tf = scale(x, TRUE, TRUE)
} else {
  x = NULL
}


dx = expand(x)

c_tf_test = collapse(scale(dx, TRUE, FALSE))
s_tf_test = collapse(scale(dx, FALSE, TRUE))
cs_tf_test = collapse(scale(dx, TRUE, TRUE))

comm.print(all.equal(c_tf, c_tf_test))
comm.print(all.equal(s_tf, s_tf_test))
comm.print(all.equal(cs_tf, cs_tf_test))


finalize()
