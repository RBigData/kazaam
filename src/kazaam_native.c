/* Automatically generated. Do not edit by hand. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP R_mpicrossprod(SEXP x);
extern SEXP R_trinv(SEXP x_, SEXP triang);

static const R_CallMethodDef CallEntries[] = {
  {"R_mpicrossprod", (DL_FUNC) &R_mpicrossprod, 1},
  {"R_trinv", (DL_FUNC) &R_trinv, 2},
  {NULL, NULL, 0}
};

void R_init_kazaam(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
