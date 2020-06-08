#include "vctrs.h"
#include "utils.h"

struct order_info {
  SEXP out;
  int* p_out;

  SEXP copy;
  int* p_copy;

  R_xlen_t* p_counts;

  uint8_t* p_bytes;

  uint8_t n_passes;
};

#define UINT8_MAX_SIZE (UINT8_MAX + 1)

// -----------------------------------------------------------------------------

// TODO: It is possible to map int32 to a uint32 rather than a uint64 since
// we know that it can fit completely in uint32. This would result in only
// needing 4 passes over the data (currently it uses 8 passes, but the last
// 4 are always skipped after computing the counts. This takes some time
// though). We have allocated memory in `p_counts` and `p_bytes` for 8 passes,
// which would be required for doubles, but integers could use that allocated
// memory and just store 4 passes worth of values. This requires changing
// `map_from_int32_to_uint64()` to return uint32, and changing anything in
// `int_radix_order()` that uses 64 -> 32. We would also explicitly set
// `n_passes` to 4.

static inline uint64_t map_from_int32_to_uint64(int32_t x);
static inline uint8_t extract_byte(uint64_t x, uint8_t pass);


static void int_radix_order(SEXP x,
                            bool na_last,
                            bool decreasing,
                            R_xlen_t size,
                            struct order_info* p_info) {
  const int* p_x = INTEGER_RO(x);

  SEXP out = p_info->out;
  int* p_out = p_info->p_out;
  SEXP copy = p_info->copy;
  int* p_copy = p_info->p_copy;
  R_xlen_t* p_counts = p_info->p_counts;
  uint8_t* p_bytes = p_info->p_bytes;
  const uint8_t n_passes = p_info->n_passes;

  // For jumping along the bytes/counts arrays
  R_xlen_t pass_start_bytes[n_passes];
  R_xlen_t pass_start_counts[n_passes];

  for (R_xlen_t pass = 0; pass < n_passes; ++pass) {
    pass_start_bytes[pass] = size * pass;
    pass_start_counts[pass] = UINT8_MAX_SIZE * pass;
  }

  // Rely on `NA_INTEGER == INT_MIN`, which is mapped to `0` as a `uint32_t`.
  const uint64_t na_uint64 = na_last ? UINT64_MAX : 0;

  // These values come from writing out the 4 combinations of na_last/decreasing
  // and seeing how adjustments have to be made to keep `NA` as the
  // largest/smallest value. They are ugly, but should be fast.
  const uint64_t adj_before = na_last ? UINT64_MAX - 1 : UINT64_MAX;
  const uint64_t adj_after = decreasing ? 1 : (na_last ? -1 : 0);

  // Build 4 histograms in one pass (one for each byte)
  for (R_xlen_t i = 0; i < size; ++i) {
    const int32_t elt_x = p_x[i];

    uint32_t elt_mapped;

    if (elt_x == NA_INTEGER) {
      elt_mapped = na_uint64;
    } else {
      elt_mapped = map_from_int32_to_uint64(elt_x);

      // Adjust based on combination of `na_last` and `decreasing`
      if (decreasing) {
        elt_mapped = adj_before - elt_mapped + adj_after;
      } else {
        elt_mapped += adj_after;
      }
    }

    for (uint8_t pass = 0; pass < n_passes; ++pass) {
      const uint8_t byte = extract_byte(elt_mapped, pass);
      p_bytes[pass_start_bytes[pass] + i] = byte;
      ++p_counts[pass_start_counts[pass] + byte];
    }
  }

  R_xlen_t p_offsets[UINT8_MAX_SIZE];

  for (uint8_t pass = 0; pass < n_passes; ++pass) {
    const R_xlen_t start_bytes = pass_start_bytes[pass];
    const R_xlen_t start_counts = pass_start_counts[pass];

    const uint8_t* p_bytes_pass = p_bytes + start_bytes;
    const R_xlen_t* p_counts_pass = p_counts + start_counts;

    R_xlen_t offset = 0;
    bool pass_skip = false;

    for (R_xlen_t i = 0; i < UINT8_MAX_SIZE; ++i) {
      const R_xlen_t count = p_counts_pass[i];

      // Skip this pass entirely if all bytes are identical, which happens
      // when a bucket holds a count equal to the number of values.
      if (count == size) {
        pass_skip = true;
        break;
      }

      p_offsets[i] = offset;
      offset += count;
    }

    if (pass_skip) {
      continue;
    }

    for (R_xlen_t i = 0; i < size; ++i) {
      const int32_t elt = p_out[i];
      const uint8_t loc = p_bytes_pass[elt - 1];

      p_copy[p_offsets[loc]++] = elt;
    }

    // Pointer swap before next pass
    SEXP temp = out;
    out = copy;
    copy = temp;
    p_out = INTEGER(out);
    p_copy = INTEGER(copy);
  }

  // Update pointers after all the swapping
  p_info->out = out;
  p_info->p_out = p_out;
  p_info->copy = copy;
  p_info->p_copy = p_copy;
}


#define HEX_UINT32_SIGN_BIT 0x80000000u

// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint64_t map_from_int32_to_uint64(int32_t x) {
  return (uint64_t) (x ^ HEX_UINT32_SIGN_BIT);
}

#undef HEX_UINT32_SIGN_BIT


static inline uint8_t extract_byte(uint64_t x, uint8_t pass) {
  return (x >> (8 * pass)) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

static void lgl_radix_order(SEXP x,
                            bool na_last,
                            bool decreasing,
                            R_xlen_t size,
                            struct order_info* p_info) {
  int_radix_order(x, na_last, decreasing, size, p_info);
}

// -----------------------------------------------------------------------------

static void vec_col_radix_order_switch(SEXP x,
                                       bool na_last,
                                       bool decreasing,
                                       R_xlen_t size,
                                       struct order_info* p_info);

// Specifically for a df-col. Slightly different from `df_radix_order()` in
// that `decreasing` is fixed for all columns.
static void df_col_radix_order(SEXP x,
                               bool na_last,
                               bool decreasing,
                               R_xlen_t size,
                               struct order_info* p_info) {
  R_xlen_t n_cols = Rf_xlength(x);

  // Iterate over columns backwards to sort correctly
  for (R_xlen_t i = 0; i < n_cols; ++i) {
    R_xlen_t j = n_cols - 1 - i;
    SEXP col = VECTOR_ELT(x, j);
    vec_col_radix_order_switch(col, na_last, decreasing, size, p_info);
  }
}

// -----------------------------------------------------------------------------

static void df_radix_order(SEXP x,
                           bool na_last,
                           SEXP decreasing,
                           R_xlen_t size,
                           struct order_info* p_info) {
  R_xlen_t n_cols = Rf_xlength(x);

  R_xlen_t n_decreasing = Rf_xlength(decreasing);
  int* p_decreasing = LOGICAL(decreasing);

  bool recycle;

  if (n_decreasing == 1) {
    recycle = true;
  } else if (n_decreasing == n_cols) {
    recycle = false;
  } else {
    Rf_errorcall(
      R_NilValue,
      "`decreasing` should have length 1 or length equal to the number of "
      "columns of `x` when `x` is a data frame."
    );
  }

  // Iterate over columns backwards to sort correctly
  for (R_xlen_t i = 0; i < n_cols; ++i) {
    R_xlen_t j = n_cols - 1 - i;
    SEXP col = VECTOR_ELT(x, j);
    bool col_decreasing = recycle ? p_decreasing[0] : p_decreasing[j];
    vec_col_radix_order_switch(col, na_last, col_decreasing, size, p_info);
  }
}

// -----------------------------------------------------------------------------

// Like `vec_radix_order_switch()`, but specifically for columns of a data
// frame where `decreasing` is known and is scalar
static void vec_col_radix_order_switch(SEXP x,
                                       bool na_last,
                                       bool decreasing,
                                       R_xlen_t size,
                                       struct order_info* p_info) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_integer: int_radix_order(x, na_last, decreasing, size, p_info); break;
  case vctrs_type_logical: lgl_radix_order(x, na_last, decreasing, size, p_info); break;
  case vctrs_type_dataframe: df_col_radix_order(x, na_last, decreasing, size, p_info); break;
  default: Rf_errorcall(R_NilValue, "This type is not supported by `vec_radix_order()`");
  }

  // Clear counts between columns.
  // Bytes will always be completely overwritten so we don't need to clear them.
  memset(p_info->p_counts, 0, UINT8_MAX_SIZE * p_info->n_passes * sizeof(R_xlen_t));
}

// -----------------------------------------------------------------------------

static void vec_radix_order_switch(SEXP x,
                                   bool na_last,
                                   SEXP decreasing,
                                   R_xlen_t size,
                                   struct order_info* p_info) {
  const enum vctrs_type type = vec_proxy_typeof(x);

  if (type == vctrs_type_dataframe) {
    df_radix_order(x, na_last, decreasing, size, p_info);
    return;
  }

  // We know it is logical with no missing values, but size hasn't been checked
  if (Rf_xlength(decreasing) != 1) {
    Rf_errorcall(R_NilValue, "`decreasing` must have length 1 when `x` is not a data frame.");
  }

  bool c_decreasing = LOGICAL(decreasing)[0];

  switch (type) {
  case vctrs_type_integer: int_radix_order(x, na_last, c_decreasing, size, p_info); return;
  case vctrs_type_logical: lgl_radix_order(x, na_last, c_decreasing, size, p_info); return;
  default: Rf_errorcall(R_NilValue, "This type is not supported by `vec_radix_order()`");
  }
}

// -----------------------------------------------------------------------------

static SEXP vec_radix_order(SEXP x, bool na_last, SEXP decreasing);

// [[ register() ]]
SEXP vctrs_radix_order_old(SEXP x, SEXP na_last, SEXP decreasing) {
  if (!r_is_bool(na_last)) {
    Rf_errorcall(R_NilValue, "`na_last` must be either `TRUE` or `FALSE`.");
  }

  bool c_na_last = LOGICAL(na_last)[0];

  return vec_radix_order(x, c_na_last, decreasing);
}

static SEXP vec_radix_order(SEXP x, bool na_last, SEXP decreasing) {
  // TODO:
  // x = PROTECT(vec_proxy_compare(x));

  // TODO:
  // Should proxy-compare flatten df-cols?
  // How to track vector of `decreasing` if so?

  static const uint8_t n_passes = 8;

  R_xlen_t size = vec_size(x);

  // Don't check length here. This might be vectorized if `x` is a data frame.
  if (TYPEOF(decreasing) != LGLSXP) {
    Rf_errorcall(R_NilValue, "`decreasing` must be logical");
  }
  if (lgl_any_na(decreasing)) {
    Rf_errorcall(R_NilValue, "`decreasing` must not contain missing values.");
  }

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_out = INTEGER(out);

  // Initialize `out` with sequential 1-based ordering
  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = i + 1;
  }

  // To store intermediate results after each pass
  SEXP copy = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_copy = INTEGER(copy);

  // Tracks the counts of each byte seen.
  // It is a long array that gets broken into `n_passes` parts.
  R_xlen_t* p_counts = (R_xlen_t*) R_alloc(UINT8_MAX_SIZE * n_passes, sizeof(R_xlen_t));
  memset(p_counts, 0, UINT8_MAX_SIZE * n_passes * sizeof(R_xlen_t));

  // Tracks the bytes themselves, since computing them requires some extra
  // work of mapping `x` to `uint32_t` before extracting bytes.
  uint8_t* p_bytes = (uint8_t*) R_alloc(size * n_passes, sizeof(uint8_t));

  struct order_info info = {
    .out = out,
    .p_out = p_out,
    .copy = copy,
    .p_copy = p_copy,
    .p_counts = p_counts,
    .p_bytes = p_bytes,
    .n_passes = n_passes
  };

  vec_radix_order_switch(x, na_last, decreasing, size, &info);

  UNPROTECT(2);
  return info.out;
}

// -----------------------------------------------------------------------------

#undef UINT8_MAX_SIZE
