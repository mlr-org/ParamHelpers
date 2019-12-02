#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP c_dfRowsToList(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP c_trafo_and_set_dep_to_na(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_dfRowsToList",            (DL_FUNC) &c_dfRowsToList,            6},
    {NULL, NULL, 0}
};

void R_init_ParamHelpers(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
