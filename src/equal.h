#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

typedef int (*vctrs_equal_scalar_fn_t)(SEXP, R_len_t, SEXP, R_len_t, bool);
vctrs_equal_scalar_fn_t get_equal_scalar_fn(SEXP x);

#endif
