#include <mpi.h>
#include <R.h>
#include <Rinternals.h>

#include "mpi_utils.h"
#include "types.h"

#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define COMPACT_LEN(n) ((n*n) - (n*(n-1))/2)

void dsyrk_(cchar_r uplo, cchar_r trans, cint_r n, cint_r k, cdbl_r alpha,
  cdbl_r a, cint_r lda, cdbl_r beta, dbl_r c, cint_r ldc);

// lower triangle of x'x
static inline void crossprod(const int m, const int n, const double alpha, const double *const restrict x, double *const restrict c)
{
  dsyrk_(&(char){'L'}, &(char){'T'}, &n, &m, &alpha, x, &m, &(double){0.0}, c, &n);
}

// lower triangle of xx'
static inline void tcrossprod(const int m, const int n, const double alpha, const double * const restrict x, double *restrict c)
{
  dsyrk_(&(char){'L'}, &(char){'N'}, &m, &n, &alpha, x, &m, &(double){0.0}, c, &m);
}

// Copy lower triangle to upper
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



SEXP R_mpicrossprod(SEXP x, SEXP alpha_)
{
  SEXP ret;
  size_t pos = 0;
  const int m = nrows(x);
  const int n = ncols(x);
  const int minmn = MIN(m, n);
  const size_t compact_len = COMPACT_LEN(minmn);
  const double alpha = REAL(alpha_)[0];
  
  PROTECT(ret = allocMatrix(REALSXP, minmn, minmn));
  double *ret_pt = REAL(ret);
  
  // store the crossproduct compactly (diag + tri as an array)
  if (m >= n)
    crossprod(m, n, 1.0, REAL(x), ret_pt);
  else
    tcrossprod(m, n, 1.0, REAL(x), ret_pt);
  
  pos = minmn;
  for (int j=1; j<minmn; j++)
  {
    for (int i=j; i<minmn; i++)
      ret_pt[pos++] = ret_pt[i + minmn*j];
  }
  
  // combine packed crossproduct across MPI ranks
  int check = MPI_Allreduce(MPI_IN_PLACE, ret_pt, compact_len, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
  if (check != MPI_SUCCESS)
    R_mpi_throw_err(check);
  
  // reconstruct the crossproduct as a full matrix
  for (int j=minmn-1; j>0; j--)
  {
    for (int i=minmn-1; i>=j; i--)
    {
      ret_pt[i + minmn*j] = alpha * ret_pt[--pos];
    }
  }
  
  for (int i=0; i<minmn; i++)
    ret_pt[i] *= alpha;
  
  symmetrize(minmn, ret_pt);
  
  
  UNPROTECT(1);
  return ret;
}
