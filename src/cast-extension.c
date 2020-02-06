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

// Better strategy:
// - Take unique values of `x` with vec_unique()
// - Check if all of them are in `levels` with a `vec_match()`
// - We have to allow NA values in `x`, so loop over `vec_match()` results and
//   check for any `NA` matches. If we find an `NA` match, check if the actual
//   x value is `NA`. If it is, we are ok, if not error lossy.
// - The `vec_match()` results are the integer `out` that becomes the factor
static SEXP chr_as_factor(SEXP x, SEXP to, bool* lossy, struct vctrs_arg* to_arg) {
  SEXP levels = Rf_getAttrib(to, R_LevelsSymbol);

  if (TYPEOF(levels) != STRSXP) {
    stop_corrupt_factor_levels(to, to_arg);
  }

  // TODO Worry about encodings?

  // TODO - What happens when length(to_levels) == 0? vec_cast.factor.character
  // creates a factor from x's unique values, is this really what we want?

  const SEXP* p_levels = STRING_PTR_RO(levels);
  R_xlen_t levels_size = Rf_xlength(levels);

  const SEXP* p_x = STRING_PTR_RO(x);
  R_xlen_t x_size = Rf_xlength(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, x_size));
  int* p_out = INTEGER(out);

  for (R_xlen_t i = 0; i < x_size; ++i) {
    bool novel = true;
    const SEXP x_elt = p_x[i];

    if (x_elt == NA_STRING) {
      p_out[i] = NA_INTEGER;
      continue;
    }

    for (R_xlen_t j = 0; j < levels_size; ++j) {
      const SEXP levels_elt = p_levels[j];

      if (x_elt == levels_elt) {
        p_out[i] = j + 1;
        novel = false;
        break;
      }
    }

    if (novel) {
      *lossy = true;
      break;
    }
  }

  if (*lossy) {
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

