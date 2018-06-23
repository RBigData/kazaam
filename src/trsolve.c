#include <R.h>
#include <Rinternals.h>

#include "types.h"


#define STR(x) ((char*)CHAR(STRING_ELT(x,0)))

void dtrtri_(cchar_r uplo, cchar_r diag, cint_r n, dbl_r a, cint_r lda, int_r info);

// type checks disabled because the function isn't public-facing. Let's see if I
// regret this decision
SEXP R_trinv(SEXP x_, SEXP triang)
{
  SEXP inv;
  int info;
  const char uplo = *STR(triang);
  const char diag = 'N';
  const int n = nrows(x_);
  
  PROTECT(inv = allocMatrix(REALSXP, n, n));
  double *const restrict x = REAL(inv);
  memcpy(x, REAL(x_), n*n*sizeof(*x));
  
  dtrtri_(&uplo, &diag, &n, x, &n, &info);
  // if (info != 0)
  //   THROW_LAPACKERR(info);
  
  UNPROTECT(1);
  return inv;
}
