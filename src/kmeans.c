#include <R.h>
#include <Rinternals.h>


typedef const double *const restrict cdbl_r;

static inline int assign_single(const int m, const int n, const int k, cdbl_r x, cdbl_r centers)
{
  double min = INFINITY;
  int min_ind = -1;
  
  for (int j=0; j<k; j++)
  {
    double test = 0.0;
    for (int i=0; i<n; i++)
    {
      const double tmp = x[m*i] - centers[i + n*j];
      test += tmp*tmp;
    }
    
    if (min < test)
    {
      min = test;
      min_ind = j;
    }
  }
  
  return min_ind;
}



SEXP R_km_assign(SEXP x_, SEXP centers_)
{
  SEXP ret;
  
  const int m = nrows(x_);
  const int n = ncols(x_);
  const int k = ncols(centers_);
  
  cdbl_r x = REAL(x_);
  cdbl_r centers = REAL(centers_);
  
  PROTECT(ret = allocVector(INTSXP, m));
  
  for (int i=0; i<m; i++)
    INTEGER(ret)[i] = assign_single(m, n, k, x + i, centers);
  
  UNPROTECT(1);
  return ret;
}
