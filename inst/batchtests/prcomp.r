suppressPackageStartupMessages(library(kazaam))

if (comm.rank() == 0){
  x = matrix(rnorm(30), 10)
  pca = prcomp(x)
  sdev = pca$sdev
  rotation = pca$rotation
  center = pca$center
  scale = pca$scale
} else {
  x = NULL
}


dx = expand(x)
pca_test = prcomp(dx)

x_test = collapse(pca_test$x)

comm.print(all.equal(sdev, pca_test$sdev, check.attributes=F))
comm.print(all.equal(abs(rotation), abs(pca_test$rotation), check.attributes=F))
comm.print(all.equal(center, pca_test$center, check.attributes=F))
comm.print(all.equal(scale, pca_test$scale, check.attributes=F))
comm.print(all.equal(abs(pca$x), abs(x_test), check.attributes=F))

finalize()
