#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void deterministic_apothecary_SEIR_initmod_desolve(void *);
extern void deterministic_apothecary_SEIR_output_dde(void *);
extern void deterministic_apothecary_SEIR_rhs_dde(void *);
extern void deterministic_apothecary_SEIR_rhs_desolve(void *);

/* .Call calls */
extern SEXP deterministic_apothecary_SEIR_contents(SEXP);
extern SEXP deterministic_apothecary_SEIR_create(SEXP);
extern SEXP deterministic_apothecary_SEIR_initial_conditions(SEXP, SEXP);
extern SEXP deterministic_apothecary_SEIR_metadata(SEXP);
extern SEXP deterministic_apothecary_SEIR_rhs_r(SEXP, SEXP, SEXP);
extern SEXP deterministic_apothecary_SEIR_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP deterministic_apothecary_SEIR_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"deterministic_apothecary_SEIR_initmod_desolve", (DL_FUNC) &deterministic_apothecary_SEIR_initmod_desolve, 1},
    {"deterministic_apothecary_SEIR_output_dde",      (DL_FUNC) &deterministic_apothecary_SEIR_output_dde,      1},
    {"deterministic_apothecary_SEIR_rhs_dde",         (DL_FUNC) &deterministic_apothecary_SEIR_rhs_dde,         1},
    {"deterministic_apothecary_SEIR_rhs_desolve",     (DL_FUNC) &deterministic_apothecary_SEIR_rhs_desolve,     1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"deterministic_apothecary_SEIR_contents",           (DL_FUNC) &deterministic_apothecary_SEIR_contents,           1},
    {"deterministic_apothecary_SEIR_create",             (DL_FUNC) &deterministic_apothecary_SEIR_create,             1},
    {"deterministic_apothecary_SEIR_initial_conditions", (DL_FUNC) &deterministic_apothecary_SEIR_initial_conditions, 2},
    {"deterministic_apothecary_SEIR_metadata",           (DL_FUNC) &deterministic_apothecary_SEIR_metadata,           1},
    {"deterministic_apothecary_SEIR_rhs_r",              (DL_FUNC) &deterministic_apothecary_SEIR_rhs_r,              3},
    {"deterministic_apothecary_SEIR_set_initial",        (DL_FUNC) &deterministic_apothecary_SEIR_set_initial,        4},
    {"deterministic_apothecary_SEIR_set_user",           (DL_FUNC) &deterministic_apothecary_SEIR_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_apothecary(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
