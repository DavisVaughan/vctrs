#include "vctrs.h"
#include "utils.h"
#include "equal.h"
#include "translate.h"
#include "type-data-frame.h"
#include "order-groups.h"

static SEXP vec_identify_runs(SEXP x);

// -----------------------------------------------------------------------------

static SEXP vec_locate_runs(SEXP x, bool start);

// [[register()]]
SEXP vctrs_locate_runs(SEXP x, SEXP start) {
  bool c_start = (bool) r_bool_as_int(start);
  return vec_locate_runs(x, c_start);
}

static void vec_locate_run_starts(const int* p_id, r_ssize size, int* p_out);
static void vec_locate_run_ends(const int* p_id, r_ssize size, int* p_out);

static
SEXP vec_locate_runs(SEXP x, bool start) {
  SEXP id = PROTECT(vec_identify_runs(x));
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

  // Handle first case
  int ref = p_id[0];
  p_out[loc] = 1;
  ++loc;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[loc] = i + 1;
    ++loc;
  }
}

static
void vec_locate_run_ends(const int* p_id, r_ssize size, int* p_out) {
  r_ssize loc = 0;

  int ref = p_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[loc] = i;
    ++loc;
  }

  // Handle last case
  p_out[loc] = size;
}

// -----------------------------------------------------------------------------

static SEXP vec_detect_runs(SEXP x, bool start);

// [[register()]]
SEXP vctrs_detect_runs(SEXP x, SEXP start) {
  bool c_start = (bool) r_bool_as_int(start);
  return vec_detect_runs(x, c_start);
}

static void vec_detect_run_starts(const int* p_id, r_ssize size, int* p_out);
static void vec_detect_run_ends(const int* p_id, r_ssize size, int* p_out);

static
SEXP vec_detect_runs(SEXP x, bool start) {
  SEXP id = PROTECT(vec_identify_runs(x));
  const int* p_id = INTEGER(id);

  r_ssize size = r_length(id);

  SEXP out = PROTECT(r_new_logical(size));
  int* p_out = LOGICAL(out);
  memset(p_out, 0, size * sizeof(int));

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
  // Handle first case
  int ref = p_id[0];
  p_out[0] = 1;

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[i] = 1;
  }
}

static
void vec_detect_run_ends(const int* p_id, r_ssize size, int* p_out) {
  int ref = p_id[0];

  for (r_ssize i = 1; i < size; ++i) {
    const int elt = p_id[i];

    if (elt == ref) {
      continue;
    }

    ref = elt;
    p_out[i - 1] = 1;
  }

  // Handle last case
  p_out[size - 1] = 1;
}

// -----------------------------------------------------------------------------

// [[register()]]
SEXP vctrs_identify_runs(SEXP x) {
  return vec_identify_runs(x);
}

static int lgl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int int_identify_runs(SEXP x, R_len_t size, int* p_out);
static int dbl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int cpl_identify_runs(SEXP x, R_len_t size, int* p_out);
static int chr_identify_runs(SEXP x, R_len_t size, int* p_out);
static int raw_identify_runs(SEXP x, R_len_t size, int* p_out);
static int list_identify_runs(SEXP x, R_len_t size, int* p_out);
static int df_identify_runs(SEXP x, R_len_t size, int* p_out);

static
SEXP vec_identify_runs(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_equal(x));
  R_len_t size = vec_size(proxy);
  proxy = PROTECT(vec_normalize_encoding(proxy));

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // Handle size 0 up front.
  // All implementations assume at least 1 element.
  if (size == 0) {
    r_attrib_poke(out, syms_n, r_int(0));
    UNPROTECT(3);
    return out;
  }

  enum vctrs_type type = vec_proxy_typeof(proxy);

  int n;

  switch (type) {
  case vctrs_type_logical: n = lgl_identify_runs(proxy, size, p_out); break;
  case vctrs_type_integer: n = int_identify_runs(proxy, size, p_out); break;
  case vctrs_type_double: n = dbl_identify_runs(proxy, size, p_out); break;
  case vctrs_type_complex: n = cpl_identify_runs(proxy, size, p_out); break;
  case vctrs_type_character: n = chr_identify_runs(proxy, size, p_out); break;
  case vctrs_type_raw: n = raw_identify_runs(proxy, size, p_out); break;
  case vctrs_type_list: n = list_identify_runs(proxy, size, p_out); break;
  case vctrs_type_dataframe: n = df_identify_runs(proxy, size, p_out); break;
  default: stop_unimplemented_vctrs_type("vec_identify_runs", type);
  }

  r_attrib_poke(out, syms_n, r_int(n));

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS(CTYPE, CONST_DEREF, SCALAR_EQUAL) {  \
  int id = 1;                                                  \
  const CTYPE* p_x = CONST_DEREF(x);                           \
                                                               \
  /* Handle first case */                                      \
  CTYPE ref = p_x[0];                                          \
  p_out[0] = id;                                               \
                                                               \
  for (R_len_t i = 1; i < size; ++i) {                         \
    const CTYPE elt = p_x[i];                                  \
                                                               \
    if (SCALAR_EQUAL(&elt, &ref) == 0) {                       \
      ++id;                                                    \
      ref = elt;                                               \
    }                                                          \
                                                               \
    p_out[i] = id;                                             \
  }                                                            \
                                                               \
  return id;                                                   \
}

static
int lgl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
}
static
int int_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(int, INTEGER_RO, int_equal_scalar_na_equal);
}
static
int dbl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(double, REAL_RO, dbl_equal_scalar_na_equal);
}
static
int cpl_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
}
static
int chr_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
}
static
int raw_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
}
static
int list_identify_runs(SEXP x, R_len_t size, int* p_out) {
  VEC_IDENTIFY_RUNS(SEXP, VECTOR_PTR_RO, list_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS

// -----------------------------------------------------------------------------

static inline void vec_identify_runs_col(SEXP x,
                                         r_ssize n_groups,
                                         const int* p_group_sizes,
                                         struct group_infos* p_group_infos);

static
int df_identify_runs(SEXP x, R_len_t size, int* p_out) {
  R_len_t n_col = Rf_length(x);

  // Define 0 column case to be a single run
  if (n_col == 0) {
    int id = 1;
    r_p_int_fill(p_out, id, size);
    return id;
  }

  int nprot = 0;

  struct group_info group_info0 = new_group_info();
  PROTECT_GROUP_INFO(&group_info0, &nprot);

  struct group_info group_info1 = new_group_info();
  PROTECT_GROUP_INFO(&group_info1, &nprot);

  struct group_info* p_p_group_info[2];
  p_p_group_info[0] = &group_info0;
  p_p_group_info[1] = &group_info1;

  struct group_infos group_infos = new_group_infos(
    p_p_group_info,
    size,
    true,
    false
  );

  // Initialize to 1 group of size `size`
  groups_size_push(size, &group_infos);

  const SEXP* p_x = VECTOR_PTR_RO(x);

  // Push groups in reverse column order
  for (R_len_t i = n_col - 1; i >= 0; --i) {
    SEXP col = p_x[i];

    const struct group_info* p_group_info = groups_current(&group_infos);
    const int* p_group_sizes = p_group_info->p_data;
    const r_ssize n_groups = p_group_info->n_groups;

    groups_swap(&group_infos);

    vec_identify_runs_col(
      col,
      n_groups,
      p_group_sizes,
      &group_infos
    );
  }

  const struct group_info* p_group_info = groups_current(&group_infos);
  const int* p_group_sizes = p_group_info->p_data;
  const r_ssize n_groups = p_group_info->n_groups;

  r_ssize k = 0;

  // Fill ids based on sizes
  for (r_ssize i = 0; i < n_groups; ++i) {
    const r_ssize id = i + 1;
    const r_ssize group_size = p_group_sizes[i];

    for (r_ssize j = 0; j < group_size; ++j, ++k) {
      p_out[k] = id;
    }
  }

  UNPROTECT(nprot);
  return n_groups;
}

// -----------------------------------------------------------------------------

static void lgl_identify_runs_col(SEXP x,
                                  r_ssize n_groups,
                                  const int* p_group_sizes,
                                  struct group_infos* p_group_infos);
static void int_identify_runs_col(SEXP x,
                                  r_ssize n_groups,
                                  const int* p_group_sizes,
                                  struct group_infos* p_group_infos);
static void dbl_identify_runs_col(SEXP x,
                                  r_ssize n_groups,
                                  const int* p_group_sizes,
                                  struct group_infos* p_group_infos);
static void cpl_identify_runs_col(SEXP x,
                                  r_ssize n_groups,
                                  const int* p_group_sizes,
                                  struct group_infos* p_group_infos);
static void chr_identify_runs_col(SEXP x,
                                  r_ssize n_groups,
                                  const int* p_group_sizes,
                                  struct group_infos* p_group_infos);
static void raw_identify_runs_col(SEXP x,
                                  r_ssize n_groups,
                                  const int* p_group_sizes,
                                  struct group_infos* p_group_infos);
static void list_identify_runs_col(SEXP x,
                                   r_ssize n_groups,
                                   const int* p_group_sizes,
                                   struct group_infos* p_group_infos);

static inline
void vec_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_logical: return lgl_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_integer: return int_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_double: return dbl_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_complex: return cpl_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_character: return chr_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_raw: return raw_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_list: return list_identify_runs_col(x, n_groups, p_group_sizes, p_group_infos);
  case vctrs_type_dataframe: stop_internal("vec_identify_runs_col", "Data frame columns should be flattened.");
  case vctrs_type_scalar: Rf_errorcall(R_NilValue, "Can't compare scalars with `vec_identify_runs()`");
  default: Rf_error("Unimplemented type in `vec_identify_runs()`");
  }
}

// -----------------------------------------------------------------------------

#define VEC_IDENTIFY_RUNS_COL(CTYPE, CONST_DEREF, EQUAL_SCALAR) { \
  const CTYPE* p_x = CONST_DEREF(x);                              \
  r_ssize k = 0;                                                  \
                                                                  \
  for (r_ssize i = 0; i < n_groups; ++i) {                        \
    r_ssize size = 1;                                             \
                                                                  \
    CTYPE ref = p_x[k++];                                         \
                                                                  \
    const r_ssize group_size = p_group_sizes[i];                  \
                                                                  \
    for (r_ssize j = 1; j < group_size; ++j) {                    \
      const CTYPE elt = p_x[k++];                                 \
                                                                  \
      /* Continue group run */                                    \
      if (EQUAL_SCALAR(&elt, &ref)) {                             \
        ++size;                                                   \
        continue;                                                 \
      }                                                           \
                                                                  \
      groups_size_push(size, p_group_infos);                      \
                                                                  \
      size = 1;                                                   \
      ref = elt;                                                  \
    }                                                             \
                                                                  \
    /* Push final group size */                                   \
    groups_size_push(size, p_group_infos);                        \
  }                                                               \
}

static
void lgl_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(int, LOGICAL_RO, lgl_equal_scalar_na_equal);
}
static
void int_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(int, INTEGER_RO, int_equal_scalar_na_equal);
}
static
void dbl_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(double, REAL_RO, dbl_equal_scalar_na_equal);
}
static
void cpl_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(Rcomplex, COMPLEX_RO, cpl_equal_scalar_na_equal);
}
static
void chr_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(SEXP, STRING_PTR_RO, chr_equal_scalar_na_equal);
}
static
void raw_identify_runs_col(SEXP x,
                           r_ssize n_groups,
                           const int* p_group_sizes,
                           struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(Rbyte, RAW_RO, raw_equal_scalar_na_equal);
}
static
void list_identify_runs_col(SEXP x,
                            r_ssize n_groups,
                            const int* p_group_sizes,
                            struct group_infos* p_group_infos) {
  VEC_IDENTIFY_RUNS_COL(SEXP, VECTOR_PTR_RO, list_equal_scalar_na_equal);
}

#undef VEC_IDENTIFY_RUNS_COL
