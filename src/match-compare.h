#ifndef VCTRS_MATCH_COMPARE_H
#define VCTRS_MATCH_COMPARE_H

#include "compare.h"

/*
 * These comparison operators are designed to match the comparison order
 * returned from:
 * `vec_order(x, direction = "asc", na_value = "smallest", nan_distinct = nan_distinct)`
 *
 * They are intended for internal use in `vec_matches()`, which uses that
 * exact setup to call `vec_order()`.
 *
 * In particular, double and complex types match the ordering results from
 * using `nan_distinct`. If `false`, they are treated equally. If `true`,
 * since this is ascending order and `NA` values are the smallest value, it
 * places `NA` before `NaN` followed by real numbers to match `vec_order()`.
 */

// -----------------------------------------------------------------------------

static inline
int lgl_order_compare_na_equal(int x, int y, bool nan_distinct) {
  return lgl_compare_na_equal(x, y);
}
static inline
int int_order_compare_na_equal(int x, int y, bool nan_distinct) {
  return int_compare_na_equal(x, y);
}
static inline
int dbl_order_compare_na_equal(double x, double y, bool nan_distinct) {
  enum vctrs_dbl_class x_class = dbl_classify(x);
  enum vctrs_dbl_class y_class = dbl_classify(y);

  switch (x_class) {
  case vctrs_dbl_number: {
    switch (y_class) {
    case vctrs_dbl_number: return dbl_compare_scalar(x, y);
    case vctrs_dbl_missing: return 1;
    case vctrs_dbl_nan: return 1;
    }
  }
  case vctrs_dbl_missing: {
    switch (y_class) {
    case vctrs_dbl_number: return -1;
    case vctrs_dbl_missing: return 0;
    case vctrs_dbl_nan: return nan_distinct ? -1 : 0;
    }
  }
  case vctrs_dbl_nan: {
    switch (y_class) {
    case vctrs_dbl_number: return -1;
    case vctrs_dbl_missing: return nan_distinct ? 1 : 0;
    case vctrs_dbl_nan: return 0;
    }
  }
  }

  r_stop_unreached("dbl_order_compare_na_equal");
}
static inline
int cpl_order_compare_na_equal(r_complex_t x, r_complex_t y, bool nan_distinct) {
  int cmp = dbl_order_compare_na_equal(x.r, y.r, nan_distinct);
  if (cmp == 0) {
    return dbl_order_compare_na_equal(x.i, y.i, nan_distinct);
  } else {
    return cmp;
  }
}
static inline
int chr_order_compare_na_equal(r_obj* x, r_obj* y, bool nan_distinct) {
  return chr_compare_na_equal(x, y);
}

// -----------------------------------------------------------------------------

#endif
