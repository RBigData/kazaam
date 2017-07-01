suppressPackageStartupMessages(library(kazaam))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
  
  p1 = x + 1
  p2 = 1 + x
  p3 = x + x
  
  m1 = x - 1
  m2 = 1 - x
  
  t1 = x * 2
  t2 = 2 * x
  
  d1 = x / 2
  d2 = 2 / x
} else {
  x = NULL
}


dx = expand(x)

p1_t = collapse(dx + 1)
p2_t = collapse(1 + dx)
p3_t = collapse(dx + dx)
comm.print(all.equal(p1, p1_t))
comm.print(all.equal(p2, p2_t))
comm.print(all.equal(p3, p3_t))

m1_t = collapse(dx - 1)
m2_t = collapse(1 - dx)
comm.print(all.equal(m1, m1_t))
comm.print(all.equal(m2, m2_t))

t1_t = collapse(dx * 2)
t2_t = collapse(2 * dx)
comm.print(all.equal(t1, t1_t))
comm.print(all.equal(t2, t2_t))

d1_t = collapse(dx / 2)
d2_t = collapse(2 / dx)
comm.print(all.equal(d1, d1_t))
comm.print(all.equal(d2, d2_t))


finalize()
