#include "vctrs.h"
#include "comparator.h"

static SEXP vec_identify_runs(SEXP x);

// [[register()]]
SEXP vctrs_identify_runs(SEXP x) {
  return vec_identify_runs(x);
}

static SEXP vec_identify_runs(SEXP x) {
  int nprot = 0;

  R_len_t size = vec_size(x);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, size), &nprot);
  int* p_out = INTEGER(out);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, size), &nprot);

  R_len_t id = 0;

  struct comparator* p_comparator = new_comparator(x);
  PROTECT_COMPARATOR(p_comparator, &nprot);

  struct comparator_vec* p_comparator_vec = new_comparator_vec(x);
  PROTECT_COMPARATOR_VEC(p_comparator_vec, &nprot);
  const void* vec_p = p_comparator_vec->vec_p;

  for (R_len_t i = 0; i < size; ++i) {
    if (!p_comparator->equal(vec_p, i, vec_p, id)) {
      id = i;
    }

    p_out[i] = id + 1;
  }

  UNPROTECT(nprot);
  return out;
}

static SEXP vec_identify_runs2(SEXP x);

// [[register()]]
SEXP vctrs_identify_runs2(SEXP x) {
  return vec_identify_runs2(x);
}

static SEXP vec_identify_runs2(SEXP x) {
  int nprot = 0;

  R_len_t size = vec_size(x);

  SEXP out = PROTECT_N(Rf_allocVector(INTSXP, size), &nprot);
  int* p_out = INTEGER(out);

  x = PROTECT_N(vec_proxy_equal(x), &nprot);
  x = PROTECT_N(obj_maybe_translate_encoding(x, size), &nprot);

  R_len_t id = 0;

  const int* p_x = INTEGER(x);

  int ref = p_x[0];

  for (R_len_t i = 0; i < size; ++i) {
    int elt = p_x[i];

    if (elt != ref) {
      id = i;
      ref = elt;
    }

    p_out[i] = id + 1;
  }

  UNPROTECT(nprot);
  return out;
}
