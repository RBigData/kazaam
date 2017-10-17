suppressMessages(library(kazaam))

m.local = 10
m = m.local*comm.size()
n = 2

k = comm.size()
data = matrix(rnorm(m.local*n, mean=10*comm.rank()), m.local, n)
x = shaq(data, m, n, checks=FALSE)

cl = km(x, k=k, seed=1234)

test = allreduce(sum(diff(DATA(cl$labels))))
comm.print(all.equal(test, 0.0))

# y = collapse(x)
# if (comm.rank() == 0){
#   set.seed(1234)
#   km = kmeans(y, centers=k, algorithm="Lloyd")
#   print(km$centers)
#   print(km$cluster)
#   print(km$iter)
# }

finalize()
