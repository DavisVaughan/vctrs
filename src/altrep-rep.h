#ifndef VCTRS_ALTREP_REP_H
#define VCTRS_ALTREP_REP_H

bool vec_is_altrep_vctrs_compact_rep(SEXP x);

SEXP new_altrep_vctrs_compact_rep_int(int value, R_xlen_t size);
SEXP new_altrep_vctrs_compact_rep_dbl(double value, R_xlen_t size);

#endif
