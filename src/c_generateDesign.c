#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


#define UNPACK_REAL_MATRIX(S, D, N, K) \
  double *D = REAL(S); \
  const R_len_t N = nrows(S); \
  const R_len_t K = ncols(S);

SEXP c_generateDesign(SEXP s_des, SEXP s_res, SEXP s_types,
  SEXP s_low, SEXP s_upp, SEXP s_values) {

  UNPACK_REAL_MATRIX(s_des, des, nrow_des, ncol_des);
  int *types = INTEGER(s_types);
  double *low = REAL(s_low);
  double *upp = REAL(s_upp);
  /* int *nlevs = INTEGER(s_nlevs); */
  int row, col; /* loop counters for rows, cols, params, vector param elements */
  int type; /* type of column we are currently handling */
  double span; /* = upper - lower */
  double *rescol_double; int *rescol_int; /* pointer to col in s_res */

  /* iterate thru design cols and scale values to correct bounds
   * factors are handled as ints with 0, ... , n.levs in C code here
   * and converted on R side */
  for (col = 0; col < ncol_des; col++) {
    SEXP s_rescol = VECTOR_ELT(s_res, col);
    type = types[col];
    /* numerics */
    if (type == 1) {
      rescol_double = REAL(s_rescol);
      span = upp[col] - low[col];
      for (row = 0; row < nrow_des; row++) {
        rescol_double[row] =  low[col] + span * des[row + col*nrow_des];
      }
    }
    /* integers */
    if (type == 2) {
      rescol_int = INTEGER(s_rescol);
      span = upp[col] - low[col] + 1;
      for (row = 0; row < nrow_des; row++) {
        rescol_int[row] =  floor(low[col] + span * des[row + col*nrow_des]);
      }
    }
    /* factors */
    if (type == 3) {
      /* SET_STRING_ELT(s_rescol, ); */
      SEXP s_values_col = VECTOR_ELT(s_values, col);
      int nlevs = LENGTH(s_values_col);
      for (row = 0; row < nrow_des; row++) {
        int val_index = floor(nlevs * des[row + col*nrow_des]);
        /* int val_index = des[row + col*nrow_des]; */
        SET_STRING_ELT(s_rescol, row, STRING_ELT(s_values_col, val_index));
      }
    }
    /* logicals */
    if (type == 4) {
      rescol_int  = LOGICAL(s_rescol);
      for (row = 0; row < nrow_des; row++) {
        rescol_int[row] =  round(des[row + col*nrow_des]);
      }
    }
  }

  return s_res;
}


SEXP c_trafo_and_set_dep_to_na(SEXP s_res, SEXP s_types,
  SEXP s_parnames, SEXP s_lens, SEXP s_trafos, SEXP s_requires, SEXP s_env) {

  int *types = INTEGER(s_types);
  int npars = LENGTH(s_lens);
  int *lens = INTEGER(s_lens);
  int nrow_res = LENGTH(VECTOR_ELT(s_res, 0));
  int row, par, k; /* loop counters for rows, cols, params, vector param elements */
  int type; /* type of column we are currently handling */
  int parlen; /* length of param we are currently handling */
  int colcount = 0; /* when we iterate params, what is the (first) column of s_res that corresponds? */
  SEXP s_trafo_fun, s_oldval, s_call, s_call_res; /* internal SEXPs */
  double *oldval_double; int *oldval_int;
  const char *parname; /* name of current param */
  Rboolean eval_res; /* result of requires expression */
  int we_have_requires = 0; /* do we have params with requires? */

  /* fun part I: apply R trafo functions
   * these are only defined for num(vecs) and int(vecs)
   * we iterate thru params, then rows.
   * then we handle vectors in blocks of the current row.
   * first, we copy the old param value, then trafo it,
   * then copy it back. */

  for (par = 0; par < npars; par++) {
    parlen = lens[par];
    s_trafo_fun = VECTOR_ELT(s_trafos, par);
    /* if there is a trafo for this param, use it */
    if (!isNull(s_trafo_fun)) {
      s_call = PROTECT(lang2(s_trafo_fun, R_NilValue));
      type = types[colcount];
      for (row = 0; row < nrow_res; row++) {
        /* copy old value */
        if (type == 1) { /* numerics */
          s_oldval = PROTECT(NEW_NUMERIC(parlen));
          oldval_double = REAL(s_oldval);
          for (k = 0; k < parlen; k++)
            oldval_double[k] = REAL(VECTOR_ELT(s_res, colcount+k))[row];
        } else { /* integers */
          s_oldval = PROTECT(NEW_INTEGER(parlen));
          oldval_int = INTEGER(s_oldval);
          for (k = 0; k < parlen; k++)
            oldval_int[k] = INTEGER(VECTOR_ELT(s_res, colcount+k))[row];
        }
        /* transform */
        SETCADR(s_call, s_oldval);
        s_call_res = PROTECT(eval(s_call, R_GlobalEnv));
        /* copy result back */
        if (type == 1) { /* numerics */
          for (k = 0; k < parlen; k++)
            REAL(VECTOR_ELT(s_res, colcount+k))[row] = REAL(s_call_res)[k];
        } else { /* integers */
          for (k = 0; k < parlen; k++)
            INTEGER(VECTOR_ELT(s_res, colcount+k))[row] = INTEGER(s_call_res)[k];
        }
        UNPROTECT(2); /* s_oldval, s_call_res */
      }
      UNPROTECT(1); /* s_call */
    }
    colcount += parlen;
  }

  /* fun part II: set dependent parameters to NA if requires not ok
   * we iterate thru rows then params.
   * for each row, all params are extracted and assigned to envir s_env
   * then we iterate again thru params and find out which are not ok */

  for (par = 0; par < npars; par++) {
    if (!isNull(VECTOR_ELT(s_requires, par)))
      we_have_requires = 1;
  }

  if (we_have_requires) {
    for (row = 0; row < nrow_res; row++) {
      SEXP s_parval;
      /* convert row to R objects and define them in envir s_env */
      colcount = 0;
      for (par = 0; par < npars; par++) {
        parlen = lens[par];
        type = types[colcount];
        parname = STRING_VALUE(STRING_ELT(s_parnames, par));
        if (type == 1) { /* numerics */
          s_parval = PROTECT(NEW_NUMERIC(parlen));
          for (k = 0; k < parlen; k++)
            REAL(s_parval)[k] = REAL(VECTOR_ELT(s_res, colcount+k))[row];
        } else if (type == 2 || type == 4) { /* integers, logical */
          s_parval = PROTECT(NEW_INTEGER(parlen));
          for (k = 0; k < parlen; k++)
            INTEGER(s_parval)[k] = INTEGER(VECTOR_ELT(s_res, colcount+k))[row];
        } else { /* factors */
          s_parval = PROTECT(NEW_CHARACTER(parlen));
          for (k = 0; k < parlen; k++)
            SET_STRING_ELT(s_parval, k, STRING_ELT(VECTOR_ELT(s_res, colcount+k), row));
        }
        defineVar(install(parname), s_parval, s_env);
        colcount += parlen;
        UNPROTECT(1); /* s_parval */
      }
      /* now eval every param in envir s_env
       * if requirements are not satisfied, set its values to NA in result */
      colcount = 0;
      for (par = 0; par < npars; par++) {
        parlen = lens[par];
        SEXP s_require = VECTOR_ELT(s_requires, par);
        type = types[colcount];
        if(!isNull(s_require)) {
          s_call_res = eval(s_require, s_env);
          eval_res = LOGICAL(s_call_res)[0];
          if (!eval_res) {
            if (type == 1) { /* numerics */
              for (k = 0; k < parlen; k++)
                REAL(VECTOR_ELT(s_res, colcount+k))[row] = NA_REAL;
            } else if (type == 2 || type == 4 ) { /* integers, logical */
              for (k = 0; k < parlen; k++)
                INTEGER(VECTOR_ELT(s_res, colcount+k))[row] = NA_INTEGER;
            } else if (type == 3) { /* factors */
              for (k = 0; k < parlen; k++)
                SET_STRING_ELT(VECTOR_ELT(s_res, colcount+k), row, NA_STRING);
            }
          }
        }
        colcount += parlen;
      }
    }
  }

  return s_res;
}
