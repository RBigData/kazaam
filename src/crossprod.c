// NOTE: throughout we are assuming m > n

#include <R.h>
#include <Rinternals.h>
#include <mpi.h>


void dsyrk_(const char *const restrict uplo, const char *const restrict trans,
  const int *const restrict n, const int *const restrict k,
  const double *const restrict alpha, const double *const restrict a,
  const int *const restrict lda, const double *const restrict beta,
  double *const restrict c, const int *const restrict ldc);

// upper triangle of x'x
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



SEXP R_mpicrossprod(SEXP x)
{
  SEXP ret;
  size_t pos = 0;
  const int m = nrows(x);
  const int n = ncols(x);
  const size_t compact_len = (n*n) - (n*(n-1))/2;
  
  PROTECT(ret = allocMatrix(REALSXP, n, n));
  double *ret_pt = REAL(ret);
  
  double *compact = malloc(compact_len * sizeof(*compact));
  if (compact == NULL)
    error("OOM");
  
  
  // store the crossproduct compactly (diag + upper tri as an array)
  crossprod(m, n, 1.0, REAL(x), ret_pt);
  
  pos = 0;
  for (int j=0; j<n; j++)
  {
    for (int i=j; i<n; i++)
      compact[pos++] = ret_pt[i + n*j];
  }
  
  // combine packed crossproduct across MPI ranks
  int check = MPI_Allreduce(MPI_IN_PLACE, compact, compact_len, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
  if (check != MPI_SUCCESS)
    error("MPI_Allreduce returned error code %d\n", check);
  
  // reconstruct the crossproduct as a full matrix
  pos = 0;
  for (int j=0; j<n; j++)
  {
    for (int i=j; i<n; i++)
      ret_pt[i + n*j] = compact[pos++];
  }
  
  symmetrize(n, ret_pt);
  
  
  free(compact);
  UNPROTECT(1);
  return ret;
}
