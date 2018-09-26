/* Automatically generated. Do not edit by hand. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP R_mpicrossprod(SEXP x, SEXP alpha_, SEXP comm_);
extern SEXP R_float_mpicrossprod(SEXP x, SEXP alpha_, SEXP comm_);
extern SEXP R_matmult_dgemm(SEXP x, SEXP y);
extern SEXP R_trinv(SEXP x_, SEXP triang);
extern SEXP R_add1(SEXP x_);
extern SEXP R_memcpy(SEXP in_, SEXP out_);
extern SEXP R_km_assign(SEXP x_, SEXP centers_, SEXP labels_);
extern SEXP R_km_update(SEXP x_, SEXP centers_, SEXP labels_);

static const R_CallMethodDef CallEntries[] = {
  {"R_mpicrossprod", (DL_FUNC) &R_mpicrossprod, 3},
  {"R_float_mpicrossprod", (DL_FUNC) &R_float_mpicrossprod, 3},
  {"R_matmult_dgemm", (DL_FUNC) &R_matmult_dgemm, 2},
  {"R_trinv", (DL_FUNC) &R_trinv, 2},
  {"R_add1", (DL_FUNC) &R_add1, 1},
  {"R_memcpy", (DL_FUNC) &R_memcpy, 2},
  {"R_km_assign", (DL_FUNC) &R_km_assign, 3},
  {"R_km_update", (DL_FUNC) &R_km_update, 3},
  {NULL, NULL, 0}
};

void R_init_kazaam(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
