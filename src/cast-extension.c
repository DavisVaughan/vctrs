#include "vctrs.h"
#include "utils.h"

static SEXP fct_as_character(SEXP x, struct vctrs_arg* x_arg);
static SEXP ord_as_character(SEXP x, struct vctrs_arg* x_arg);

static SEXP chr_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg);
static SEXP fct_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

static SEXP chr_as_ordered(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg);
static SEXP ord_as_ordered(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* x_arg, struct vctrs_arg* to_arg);

// -----------------------------------------------------------------------------

static SEXP vec_cast_switch_extension2(SEXP x, SEXP to,
                                       enum vctrs_type x_type,
                                       bool* lossy,
                                       struct vctrs_arg* x_arg,
                                       struct vctrs_arg* to_arg);

// [[ include("utils.h") ]]
SEXP vec_cast_switch_extension(SEXP x, SEXP to,
                               enum vctrs_type x_type,
                               enum vctrs_type to_type,
                               bool* lossy,
                               struct vctrs_arg* x_arg,
                               struct vctrs_arg* to_arg) {
  switch (to_type) {
  case vctrs_type_character:
    switch (class_type(x)) {
    case vctrs_class_bare_factor:
      return fct_as_character(x, x_arg);
    case vctrs_class_bare_ordered:
      return ord_as_character(x, x_arg);
    default:
      break;
    }
  case vctrs_type_s3:
    return vec_cast_switch_extension2(x, to, x_type, lossy, x_arg, to_arg);
  default:
    break;
  }

  return R_NilValue;
}

static SEXP vec_cast_switch_extension2(SEXP x, SEXP to,
                                       enum vctrs_type x_type,
                                       bool* lossy,
                                       struct vctrs_arg* x_arg,
                                       struct vctrs_arg* to_arg) {
  switch (class_type(to)) {
  case vctrs_class_bare_factor:
    switch (x_type) {
    case vctrs_type_character:
      return chr_as_factor(x, to, lossy, to_arg);

    case vctrs_type_s3:
      switch (class_type(x)) {
      case vctrs_class_bare_factor:
        return fct_as_factor(x, to, lossy, x_arg, to_arg);

      default:
        break;
      }

    default:
      break;
    }

  case vctrs_class_bare_ordered:
    switch (x_type) {
    case vctrs_type_character:
      return chr_as_ordered(x, to, lossy, to_arg);

    case vctrs_type_s3:
      switch (class_type(x)) {
      case vctrs_class_bare_ordered:
        return ord_as_ordered(x, to, lossy, x_arg, to_arg);

      default:
        break;
      }

    default:
      break;
    }

  default:
    break;
  }

  return R_NilValue;
}

// -----------------------------------------------------------------------------

static SEXP fct_as_character(SEXP x, struct vctrs_arg* x_arg) {
  SEXP levels = Rf_getAttrib(x, R_LevelsSymbol);

  if (TYPEOF(levels) != STRSXP) {
    stop_corrupt_factor_levels(x, x_arg);
  }

  return Rf_asCharacterFactor(x);
}

static SEXP ord_as_character(SEXP x, struct vctrs_arg* x_arg) {
  return fct_as_character(x, x_arg);
}

static void new_factor(SEXP x, SEXP levels);

SEXP vctrs_match(SEXP needles, SEXP haystack);

static SEXP chr_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg) {
  SEXP levels = Rf_getAttrib(to, R_LevelsSymbol);

  if (TYPEOF(levels) != STRSXP) {
    stop_corrupt_factor_levels(to, to_arg);
  }

  SEXP out = PROTECT(vctrs_match(x, levels));
  const int* p_out = INTEGER(out);

  R_len_t size = vec_size(x);
  const SEXP* p_x = STRING_PTR_RO(x);

  // - Check for `NA` match values that are actually no-matches
  // - `NA` values in `x` are allowed
  for (R_len_t i = 0; i < size; ++i) {
    if (p_out[i] != NA_INTEGER) {
      continue;
    }

    if (p_x[i] == NA_STRING) {
      continue;
    }

    *lossy = true;
    UNPROTECT(1);
    return R_NilValue;
  }

  new_factor(out, levels);

  UNPROTECT(1);
  return out;
}

static SEXP fct_as_factor(SEXP x, SEXP to,
                          bool* lossy,
                          struct vctrs_arg* x_arg,
                          struct vctrs_arg* to_arg) {
 return x;
}

static SEXP chr_as_ordered(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg) {
  return x;
}


static SEXP ord_as_ordered(SEXP x, SEXP to,
                           bool* lossy,
                           struct vctrs_arg* x_arg,
                           struct vctrs_arg* to_arg) {
  return x;
}



static void new_factor(SEXP x, SEXP levels) {
  if (TYPEOF(x) != INTSXP) {
    Rf_errorcall(R_NilValue, "Internal error: Only integers can be made into factors");
  }

  Rf_setAttrib(x, R_LevelsSymbol, levels);
  Rf_setAttrib(x, R_ClassSymbol, classes_factor);
}

