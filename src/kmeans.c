#if _WIN32 || _WIN64
#include <stdint.h>
#endif

#include <mpi.h>
#include <R.h>
#include <Rinternals.h>

#include "mpi_utils.h"
#include "types.h"


// -----------------------------------------------------------------------------
// utils
// -----------------------------------------------------------------------------

SEXP R_add1(SEXP x_)
{
  PROTECT(x_);
  const size_t len = LENGTH(x_);
  
  if (TYPEOF(x_) == INTSXP)
  {
    int *const restrict x = INTEGER(x_);
    for (size_t i=0; i<len; i++)
      x[i] += 1;
  }
  else if (TYPEOF(x_) == REALSXP)
  {
    double *const restrict x = REAL(x_);
    for (size_t i=0; i<len; i++)
      x[i] += 1.0;
  }
  else
    error("unsupported type");
  
  UNPROTECT(1);
  return R_NilValue;
}

SEXP R_memcpy(SEXP dest_, SEXP src_)
{
  size_t len; 
  PROTECT(src_);
  PROTECT(dest_);
  
  if (isMatrix(src_))
    len = (size_t) nrows(src_)*ncols(src_);
  else
    len = (size_t) LENGTH(src_);
  
  if (TYPEOF(src_) == INTSXP || TYPEOF(src_) == LGLSXP)
    memcpy(INTEGER(dest_), INTEGER(src_), len*sizeof(int));
  else if (TYPEOF(dest_) == REALSXP)
    memcpy(REAL(dest_), REAL(src_), len*sizeof(double));
  else
    error("bad type");
  
  UNPROTECT(2);
  return R_NilValue;
}



// -----------------------------------------------------------------------------
// kmeans
// -----------------------------------------------------------------------------

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
    
    if (test < min || min_ind == -1)
    {
      min = test;
      min_ind = j;
    }
  }
  
  return min_ind;
}



SEXP R_km_assign(SEXP x_, SEXP centers_, SEXP labels_)
{
  const int m = nrows(x_);
  const int n = ncols(x_);
  const int k = ncols(centers_);
  
  PROTECT(labels_);
  
  cdbl_r x = REAL(x_);
  cdbl_r centers = REAL(centers_);
  int_r labels = INTEGER(labels_);
  
  for (int i=0; i<m; i++)
    labels[i] = assign_single(m, n, k, x+i, centers);
  
  UNPROTECT(1);
  return R_NilValue;
}



SEXP R_km_update(SEXP x_, SEXP centers_, SEXP labels_)
{
  int check;
  const int m = nrows(x_);
  const int n = ncols(x_);
  const int k = ncols(centers_);
  cdbl_r x = REAL(x_);
  dbl_r centers = REAL(centers_);
  int_r labels = INTEGER(labels_);
  
  PROTECT(centers_);
  
  int *nlabels = malloc(k * sizeof(*nlabels));
  if (nlabels == NULL)
    error("OOM");
  
  memset(centers, 0, n*k*sizeof(double));
  for (int j=0; j<n; j++)
  {
    for (int i=0; i<m; i++)
      centers[j + n*labels[i]] += x[i + m*j];
  }
  
  check = MPI_Allreduce(MPI_IN_PLACE, centers, n*k, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
  if (check != MPI_SUCCESS)
  {
    free(nlabels);
    R_mpi_throw_err(check);
  }
  
  
  memset(nlabels, 0, k*sizeof(*nlabels));
  
  for (int i=0; i<m; i++)
    nlabels[labels[i]]++;
  
  check = MPI_Allreduce(MPI_IN_PLACE, nlabels, k, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD);
  if (check != MPI_SUCCESS)
  {
    free(nlabels);
    R_mpi_throw_err(check);
  }
    
  for (int j=0; j<k; j++)
  {
    for (int i=0; i<n; i++)
      centers[i + n*j] /= (double)nlabels[j];
  }
  
  free(nlabels);
  UNPROTECT(1);
  return R_NilValue;
}
