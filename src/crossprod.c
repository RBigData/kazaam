// NOTE: throughout we are assuming m > n

#include <R.h>
#include <Rinternals.h>


// upper triangle of t(x) %*% x
void dsyrk_(const char *const restrict uplo, const char *const restrict trans,
  const int *const restrict n, const int *const restrict k,
  const double *const restrict alpha, const double *const restrict a,
  const int *const restrict lda, const double *const restrict beta,
  double *const restrict c, const int *const restrict ldc);

static inline void crossprod(const int m, const int n, const double alpha, const double *const restrict x, double *const restrict c)
{
  dsyrk_(&(char){'L'}, &(char){'T'}, &n, &m, &alpha, x, &m, &(double){0.0}, c, &n);
}

// Copy upper triangle to lower
static inline void symmetrize(const int n, double *restrict x)
{
  const int blocksize = 8; // TODO check cache line explicitly
  
  // #pragma omp parallel for default(none) shared(x) schedule(dynamic, 1) if(n>OMP_MIN_SIZE)
  for (int j=0; j<n; j+=blocksize)
  {
    for (int i=j+1; i<n; i+=blocksize)
    {
      for (int col=j; col<j+blocksize && col<n; ++col)
      {
        for (int row=i; row<i+blocksize && row<n; ++row)
          x[col + n*row] = x[row + n*col];
      }
    }
  }
}



// crossproduct matrix, stored as a vector (diag+upper tri)
SEXP R_crossprod_uppertri(SEXP x)
{
  SEXP ret;
  const int m = nrows(x);
  const int n = ncols(x);
  const size_t len = (n*n) - (n*(n-1))/2;
  
  PROTECT(ret = allocVector(REALSXP, len));
  double *const restrict retpt = REAL(ret);
  
  double *tmp = malloc(n*n * sizeof(*tmp));
  if (tmp == NULL)
    error("OOM");
  
  crossprod(m, n, 1.0, REAL(x), tmp);
  
  size_t ret_pos = 0;
  for (int j=0; j<n; j++)
  {
    for (int i=j; i<n; i++)
      retpt[ret_pos++] = tmp[i + n*j];
  }
  
  
  free(tmp);
  UNPROTECT(1);
  return ret;
}



// given vector containing diag+upper tri, reconstruct full matrix
SEXP R_crossprod_reconstruct(SEXP x, SEXP n_)
{
  SEXP ret;
  const int n = INTEGER(n_)[0];
  
  PROTECT(ret = allocMatrix(REALSXP, n, n));
  double *const restrict retpt = REAL(ret);
  double *const restrict xpt = REAL(x);
  
  size_t x_pos = 0;
  for (int j=0; j<n; j++)
  {
    for (int i=j; i<n; i++)
      retpt[i + n*j] = xpt[x_pos++];
  }
  
  symmetrize(n, retpt);
  
  UNPROTECT(1);
  return ret;
}
