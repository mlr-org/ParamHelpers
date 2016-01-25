#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP c_addOptPathDF(SEXP s_df, SEXP s_k, SEXP s_el, SEXP s_y, SEXP s_env_dob, SEXP s_dob, SEXP s_env_eol, SEXP s_eol) {
    SEXP s_el_x;
    SEXP s_df_col;
    int k = asInteger(s_k);
    double* y = REAL(s_y);
    int ny = Rf_length(s_y);
    int i;
    int npars = Rf_length(s_el);
    int par_len;
    /* Rprintf("%i\n", len); */
    int par_index;
    int s_df_col_index = 0;
    int vecpar_index = 0;

    for (par_index = 0; par_index < npars; par_index++) {
        s_el_x = VECTOR_ELT(s_el, par_index);
        par_len = Rf_length(s_el_x);
        for (i = 0; i < par_len; i++) {
            s_df_col = VECTOR_ELT(s_df, s_df_col_index);
            switch(TYPEOF(s_df_col)) {
                case INTSXP:
                    INTEGER(s_df_col)[k] = INTEGER(s_el_x)[i];
                    break;
                case REALSXP:
                    REAL(s_df_col)[k] = REAL(s_el_x)[i];
                    break;
                case LGLSXP:
                    LOGICAL(s_df_col)[k] = LOGICAL(s_el_x)[i];
                    break;
                case STRSXP:
                    SET_STRING_ELT(s_df_col, k, STRING_ELT(s_el_x, i));
                    break;
            };
            s_df_col_index++;
        }
    }
    for (i = 0; i < ny; i++) {
        s_df_col = VECTOR_ELT(s_df, s_df_col_index);
        REAL(s_df_col)[k] = y[i];
        s_df_col_index++;
    }
    /* INTEGER(s_env_dob)[k] = INTEGER(s_dob)[0]; */
    /* INTEGER(s_env_eol)[k] = INTEGER(s_eol)[0]; */
    return R_NilValue;
}
