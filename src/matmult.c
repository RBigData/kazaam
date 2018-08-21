// Modified from the coop package. Copyright (c) 2016-2017 Drew Schmidt
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>


void dgemm_(const char *transa, const char *transb, const int *m, const int *n,
  const int *k, const double *restrict alpha, const double *restrict a,
  const int *lda, const double *restrict b, const int *ldb,
  const double *beta, double *restrict c, const int *ldc);

static inline void matmult(const bool transx, const bool transy, const double alpha, const int mx, const int nx, const double *const restrict x, const int my, const int ny, const double *const restrict y, double *restrict ret)
{
  // m = # rows of op(x)
  // n = # cols of op(y)
  // k = # cols of op(x)
  int im, in, ik;
  char ctransx, ctransy;
  static const double zero = 0.;
  
  ctransx = transx ? 'T' : 'N';
  ctransy = transy ? 'T' : 'N';
  
  if (transx)
  {
    im = nx;
    ik = mx;
  }
  else
  {
    im = mx;
    ik = nx;
  }
  
  in = transy ? my : ny;
  
  dgemm_(&ctransx, &ctransy, &im, &in, &ik, &alpha, x, &mx, y, &my, &zero, ret, &im);
}



SEXP R_matmult_dgemm(SEXP x, SEXP y)
{
  SEXP ret;
  const int mx = nrows(x);
  const int nx = ncols(x);
  const int my = nrows(y);
  const int ny = ncols(y);
  
  PROTECT(ret = allocMatrix(REALSXP, mx, ny));
  
  matmult(false, false, 1.0, mx, nx, REAL(x), my, ny, REAL(y), REAL(ret));
  
  UNPROTECT(1);
  return ret;
}
