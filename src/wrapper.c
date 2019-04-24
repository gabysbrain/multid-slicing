#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <Rinternals.h>
#include <stdlib.h>
#include <stdio.h>

// Import C headers for rust API
#include "rslice2d/api.h"

// Actual Wrappers
SEXP spi_wrapper(SEXP s, SEXP r, SEXP c, 
                 SEXP fp, SEXP n, 
                 SEXP d1, SEXP d2) {
  double* ms = REAL(s);
  double* vfp = REAL(fp);
  // R uses 1-based dimension indexing!
  SliceSeg* res = r_spi(ms, Rf_asInteger(r), Rf_asInteger(c), 
                        vfp, Rf_asInteger(n), 
                        Rf_asInteger(d1)-1, Rf_asInteger(d2)-1);
  if(res) {
    //printf("%f %f %f %f\n", res->x1, res->y1, res->x2, res->y2);
    SEXP lst = PROTECT(Rf_allocVector(VECSXP, 4));
    SET_VECTOR_ELT(lst, 0, Rf_ScalarReal(res->p1_1));
    SET_VECTOR_ELT(lst, 1, Rf_ScalarReal(res->p1_2));
    SET_VECTOR_ELT(lst, 2, Rf_ScalarReal(res->p2_1));
    SET_VECTOR_ELT(lst, 3, Rf_ScalarReal(res->p2_2));

    //free(res);

    UNPROTECT(1);
    return lst;
  } else {
    SEXP lst = PROTECT(Rf_allocVector(VECSXP, 0));

    UNPROTECT(1);
    return lst;
  }
}

// Standard R package stuff
static const R_CallMethodDef CallEntries[] = {
  {"spi_wrapper", (DL_FUNC) &spi_wrapper, 0},
  {NULL, NULL, 0}
};

void R_init_rslice2d(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
