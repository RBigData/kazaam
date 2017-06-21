suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
  s_tf = scale(x, TRUE, FALSE)
} else {
  x = NULL
}


dx = expand(x)
s_tf_test = collapse(scale(dx, TRUE, FALSE))

comm.print(all.equal(s_tf, s_tf_test))


finalize()
