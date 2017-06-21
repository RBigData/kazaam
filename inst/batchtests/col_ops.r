suppressPackageStartupMessages(library(pbdSHAQ))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
  cs = colSums(x)
  cm = colMeans(x)
} else {
  x = NULL
}


dx = expand(x)
cs_test = colSums(dx)
cm_test = colMeans(dx)

comm.print(all.equal(cs, cs_test))
comm.print(all.equal(cm, cm_test))


finalize()
