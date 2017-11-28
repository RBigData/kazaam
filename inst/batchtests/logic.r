suppressPackageStartupMessages(library(kazaam))

if (comm.rank() == 0){
  x = matrix(1:30, 10)
  
  lt1 = x < 10
  lt2 = 10 < x
  lt3 = x < x
  
  gt1 = x > 10
  gt2 = 10 > x
  gt3 = x > x
  
  le1 = x <= 10
  le2 = 10 <= x
  le3 = x <= x
  
  ge1 = x >= 10
  ge2 = 10 >= x
  ge3 = x >= x
  
  eq1 = x == 10
  eq2 = 10 == x
  
  ne1 = x != 10
  ne2 = 10 != x
  ne3 = x != x
} else {
  x = NULL
}


dx = expand(x)

lt1_t = collapse(dx < 10)
lt2_t = collapse(10 < dx)
lt3_t = collapse(dx < dx)
comm.print(all.equal(lt1, lt1_t))
comm.print(all.equal(lt2, lt2_t))
comm.print(all.equal(lt3, lt3_t))

gt1_t = collapse(dx > 10)
gt2_t = collapse(10 > dx)
gt3_t = collapse(dx > dx)
comm.print(all.equal(gt1, gt1_t))
comm.print(all.equal(gt2, gt2_t))
comm.print(all.equal(gt3, gt3_t))

le1_t = collapse(dx <= 10)
le2_t = collapse(10 <= dx)
le3_t = collapse(dx <= dx)
comm.print(all.equal(le1, le1_t))
comm.print(all.equal(le2, le2_t))
comm.print(all.equal(le3, le3_t))

ge1_t = collapse(dx >= 10)
ge2_t = collapse(10 >= dx)
ge3_t = collapse(dx >= dx)
comm.print(all.equal(ge1, ge1_t))
comm.print(all.equal(ge2, ge2_t))
comm.print(all.equal(ge3, ge3_t))

eq1_t = collapse(dx == 10)
eq2_t = collapse(10 == dx)
comm.print(all.equal(eq1, eq1_t))
comm.print(all.equal(eq2, eq2_t))

ne1_t = collapse(dx != 10)
ne2_t = collapse(10 != dx)
ne3_t = collapse(dx != dx)
comm.print(all.equal(ne1, ne1_t))
comm.print(all.equal(ne2, ne2_t))
comm.print(all.equal(ne3, ne3_t))


finalize()
