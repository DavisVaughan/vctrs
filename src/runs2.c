#include "vctrs.h"

SEXP vec_identify_runs2(SEXP x);

// -----------------------------------------------------------------------------

static SEXP vec_locate_runs(SEXP x, bool start);

// [[register()]]
SEXP vctrs_locate_runs2(SEXP x, SEXP start) {
  bool c_start = (bool) r_bool_as_int(start);
  return vec_locate_runs(x, c_start);
}

static void vec_locate_run_starts(const int* p_id, r_ssize size, int* p_out);
static void vec_locate_run_ends(const int* p_id, r_ssize size, int* p_out);

static
SEXP vec_locate_runs(SEXP x, bool start) {
  SEXP id = PROTECT(vec_identify_runs2(x));
  const int* p_id = INTEGER(id);

  r_ssize size = r_length(id);

  int n = r_int_get(r_attrib_get(id, syms_n), 0);

  SEXP out = PROTECT(r_new_integer(n));
  int* p_out = INTEGER(out);

  if (n == 0) {
    UNPROTECT(2);
    return out;
  }

  if (start) {
    vec_locate_run_starts(p_id, size, p_out);
  } else {
    vec_locate_run_ends(p_id, size, p_out);
  }

  UNPROTECT(2);
  return out;
}

static
void vec_locate_run_starts(const int* p_id, r_ssize size, int* p_out) {
  r_ssize loc = 0;

  int ref = p_id[0];

  // Handle first case
  p_out[loc] = 1;
  ++loc;

  for (r_ssize i = 1; i < size; ++i) {
    p_out[loc] = i + 1;
    const int elt = p_id[i];
    loc += elt != ref;
    ref = elt;
  }
}

static
void vec_locate_run_ends(const int* p_id, r_ssize size, int* p_out) {
  r_ssize loc = 0;

  int ref = p_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    p_out[loc] = i;
    const int elt = p_id[i];
    loc += elt != ref;
    ref = elt;
  }

  // Handle last case
  p_out[loc] = size;
}

// -----------------------------------------------------------------------------

static SEXP vec_detect_runs(SEXP x, bool start);

// [[register()]]
SEXP vctrs_detect_runs2(SEXP x, SEXP start) {
  bool c_start = (bool) r_bool_as_int(start);
  return vec_detect_runs(x, c_start);
}

static void vec_detect_run_starts(const int* p_id, r_ssize size, int* p_out);
static void vec_detect_run_ends(const int* p_id, r_ssize size, int* p_out);

static
SEXP vec_detect_runs(SEXP x, bool start) {
  SEXP id = PROTECT(vec_identify_runs2(x));
  const int* p_id = INTEGER(id);

  r_ssize size = r_length(id);

  SEXP out = PROTECT(r_new_logical(size));
  int* p_out = LOGICAL(out);

  if (size == 0) {
    UNPROTECT(2);
    return out;
  }

  if (start) {
    vec_detect_run_starts(p_id, size, p_out);
  } else {
    vec_detect_run_ends(p_id, size, p_out);
  }

  UNPROTECT(2);
  return out;
}

static
void vec_detect_run_starts(const int* p_id, r_ssize size, int* p_out) {
  int ref = p_id[0];

  // Handle first case
  p_out[0] = 1;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];
    p_out[i] = elt != ref;
    ref = elt;
  }
}

static
void vec_detect_run_ends(const int* p_id, r_ssize size, int* p_out) {
  int ref = p_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];
    p_out[i - 1] = elt != ref;
    ref = elt;
  }

  // Handle last case
  p_out[size - 1] = 1;
}

// -----------------------------------------------------------------------------

// [[register()]]
SEXP vctrs_identify_runs2(SEXP x) {
  return vec_identify_runs2(x);
}

static int lgl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int int_identify_runs(SEXP x, R_len_t size, int* p_out);
static int dbl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int cpl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int chr_identify_runs(SEXP x, R_len_t size, int* p_out);
static int raw_identify_runs(SEXP x, R_len_t size, int* p_out);
static int list_identify_runs(SEXP x, R_len_t size, int* p_out);
static int df_identify_runs(SEXP x, R_len_t size, int* p_out);

// [[ include("vctrs.h") ]]
SEXP vec_identify_runs2(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_equal(x));
  R_len_t size = vec_size(proxy);
  proxy = PROTECT(vec_normalize_encoding(proxy));

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // Handle size 0 up front.
  // All implementations assume at least 1 element.
  if (size == 0) {
    SEXP n = PROTECT(r_int(0));
    r_attrib_poke(out, syms_n, n);
    UNPROTECT(4);
    return out;
  }

  enum vctrs_type type = vec_proxy_typeof(proxy);

  int n;

  switch (type) {
  case VCTRS_TYPE_logical: n = lgl_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_integer: n = int_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_double: n = dbl_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_complex: n = cpl_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_character: n = chr_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_raw: n = raw_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_list: n = list_identify_runs(proxy, size, p_out); break;
  case VCTRS_TYPE_dataframe: n = df_identify_runs(proxy, size, p_out); break;
  default: stop_unimplemented_vctrs_type("vec_identify_runs", type);
  }

  SEXP r_n = PROTECT(r_int(n));
  r_attrib_poke(out, syms_n, r_n);

  UNPROTECT(4);
  return out;
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL) {  \
  int id = 1;                                                    \
  const CTYPE* p_x = CONST_DEREF(x);                             \
                                                                 \
  /* Handle first case */                                        \
  CTYPE ref = p_x[0];                                            \
  p_out[0] = id;                                                 \
                                                                 \
  for (R_len_t i = 1; i < size; ++i) {                           \
    const CTYPE elt = p_x[i];                                    \
    id += !EQUAL_NA_EQUAL(elt, ref);                             \
    p_out[i] = id;                                               \
    ref = elt;                                                   \
  }                                                              \
                                                                 \
  return id;                                                     \
}

static
int lgl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(int, LOGICAL_RO, lgl_equal_na_equal);
}
static
int int_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(int, INTEGER_RO, int_equal_na_equal);
}
static
int dbl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(double, REAL_RO, dbl_equal_na_equal);
}
static
int cpl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(Rcomplex, COMPLEX_RO, cpl_equal_na_equal);
}
static
int chr_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(SEXP, STRING_PTR_RO, chr_equal_na_equal);
}
static
int raw_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(Rbyte, RAW_RO, raw_equal_na_equal);
}
static
int list_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(SEXP, VECTOR_PTR_RO, list_equal_na_equal);
}

#undef VEC_IDENTIFY_RUNS

// -----------------------------------------------------------------------------

static inline r_ssize col_identify_runs(SEXP x,
                                        r_ssize* v_loc,
                                        r_ssize loc_size);

static
int df_identify_runs(SEXP x, R_len_t size, int* p_out) {
  R_len_t n_col = Rf_length(x);

  // Define 0 column case to be a single run
  if (n_col == 0) {
    int id = 1;
    r_p_int_fill(p_out, id, size);
    return id;
  }

  const SEXP* p_x = VECTOR_PTR_RO(x);

  // A location vector to track rows where we still need to check for runs.
  // After we iterate through all columns, `v_loc` points to the run continuations.
  // First row is known to be a run start so we don't include it.
  r_ssize loc_size = size;
  r_obj* loc_shelter = PROTECT(r_alloc_raw(loc_size * sizeof(r_ssize)));
  r_ssize* v_loc = (r_ssize*) r_raw_begin(loc_shelter);

  v_loc[0] = 0;
  for (r_ssize i = 1; i < loc_size; ++i) {
    v_loc[i] = 1;
  }

  for (R_len_t i = 0; i < n_col; ++i) {
    SEXP col = p_x[i];

    col_identify_runs(col, v_loc, loc_size);

    // All rows are unique
    if (loc_size == 0) {
      break;
    }
  }

  int id = 1;
  p_out[0] = id;

  for (r_ssize i = 1; i < size; ++i) {
    id += !v_loc[i];
    p_out[i] = id;
  }

  // r_obj* where = PROTECT(r_alloc_logical(size));
  // int* v_where = r_lgl_begin(where);
  // r_p_int_fill(v_where, 1, size);
  //
  // for (r_ssize i = 0; i < loc_size; ++i) {
  //   v_where[v_loc[i]] = 0;
  // }

  // int id = 1;
  // r_ssize j = 0;
  //
  // for (r_ssize i = 0; i < loc_size; ++i) {
  //   v_loc[i]
  // }

  // for (r_ssize i = 0; i < size; ++i) {
  //   id += v_where[i];
  //   p_out[i] = id;
  // }

  UNPROTECT(1);
  return 1;
}

// -----------------------------------------------------------------------------

static r_ssize lgl_identify_runs_col(SEXP x,
                                     r_ssize* v_loc,
                                     r_ssize loc_size);
static r_ssize int_identify_runs_col(SEXP x,
                                     r_ssize* v_loc,
                                     r_ssize loc_size);
static r_ssize dbl_identify_runs_col(SEXP x,
                                     r_ssize* v_loc,
                                     r_ssize loc_size);
static r_ssize cpl_identify_runs_col(SEXP x,
                                     r_ssize* v_loc,
                                     r_ssize loc_size);
static r_ssize chr_identify_runs_col(SEXP x,
                                     r_ssize* v_loc,
                                     r_ssize loc_size);
static r_ssize raw_identify_runs_col(SEXP x,
                                     r_ssize* v_loc,
                                     r_ssize loc_size);
static r_ssize list_identify_runs_col(SEXP x,
                                      r_ssize* v_loc,
                                      r_ssize loc_size);

static inline
r_ssize col_identify_runs(SEXP x,
                          r_ssize* v_loc,
                          r_ssize loc_size) {
  switch (vec_proxy_typeof(x)) {
  case VCTRS_TYPE_logical: return lgl_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_integer: return int_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_double: return dbl_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_complex: return cpl_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_character: return chr_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_raw: return raw_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_list: return list_identify_runs_col(x, v_loc, loc_size);
  case VCTRS_TYPE_dataframe: r_stop_internal("Data frame columns should be flattened.");
  case VCTRS_TYPE_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_identify_runs()`");
  default: Rf_error("Unimplemented type in `vec_identify_runs()`");
  }
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS_COL(CTYPE, CONST_DEREF, EQUAL_NA_EQUAL) { \
  CTYPE const* v_x = CONST_DEREF(x);                                \
  CTYPE ref = v_x[0];                                               \
                                                                    \
  for (r_ssize i = 1; i < loc_size; ++i) {                          \
    const CTYPE elt = v_x[i];                                       \
    v_loc[i] = v_loc[i] && EQUAL_NA_EQUAL(ref, elt);                \
    ref = elt;                                                      \
  }                                                                 \
                                                                    \
  return 1;                                                         \
}

static
r_ssize lgl_identify_runs_col(SEXP x,
                              r_ssize* v_loc,
                              r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(int, LOGICAL_RO, lgl_equal_na_equal);
}
static
r_ssize int_identify_runs_col(SEXP x,
                              r_ssize* v_loc,
                              r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(int, INTEGER_RO, int_equal_na_equal);
}
static
r_ssize dbl_identify_runs_col(SEXP x,
                              r_ssize* v_loc,
                              r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(double, REAL_RO, dbl_equal_na_equal);
}
static
r_ssize cpl_identify_runs_col(SEXP x,
                              r_ssize* v_loc,
                              r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(Rcomplex, COMPLEX_RO, cpl_equal_na_equal);
}
static
r_ssize chr_identify_runs_col(SEXP x,
                              r_ssize* v_loc,
                              r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(SEXP, STRING_PTR_RO, chr_equal_na_equal);
}
static
r_ssize raw_identify_runs_col(SEXP x,
                              r_ssize* v_loc,
                              r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(Rbyte, RAW_RO, raw_equal_na_equal);
}
static
r_ssize list_identify_runs_col(SEXP x,
                               r_ssize* v_loc,
                               r_ssize loc_size) {
  VEC_IDENTIFY_RUNS_COL(SEXP, VECTOR_PTR_RO, list_equal_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL
