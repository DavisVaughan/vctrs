#include "vctrs.h"
#include "utils.h"

// -----------------------------------------------------------------------------
// Helpers for determining if UTF-8 translation is required for character
// vectors

// UTF-8 translation will be successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation will fail purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static bool chr_translation_required_impl(const SEXP* x, R_len_t size, cetype_t reference) {
  for (R_len_t i = 0; i < size; ++i, ++x) {
    if (Rf_getCharCE(*x) != reference) {
      return true;
    }
  }

  return false;
}

static bool chr_translation_required(SEXP x, R_len_t size) {
  if (size == 0) {
    return false;
  }

  const SEXP* p_x = STRING_PTR_RO(x);
  cetype_t reference = Rf_getCharCE(*p_x);

  return chr_translation_required_impl(p_x, size, reference);
}

// Check if `x` or `y` need to be translated to UTF-8, relative to each other
static bool chr_translation_required2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  const SEXP* p_x;
  const SEXP* p_y;

  bool x_empty = x_size == 0;
  bool y_empty = y_size == 0;

  if (x_empty && y_empty) {
    return false;
  }

  if (x_empty) {
    p_y = STRING_PTR_RO(y);
    return chr_translation_required_impl(p_y, y_size, Rf_getCharCE(*p_y));
  }

  if (y_empty) {
    p_x = STRING_PTR_RO(x);
    return chr_translation_required_impl(p_x, x_size, Rf_getCharCE(*p_x));
  }

  p_x = STRING_PTR_RO(x);
  cetype_t reference = Rf_getCharCE(*p_x);

  if (chr_translation_required_impl(p_x, x_size, reference)) {
    return true;
  }

  p_y = STRING_PTR_RO(y);

  if (chr_translation_required_impl(p_y, y_size, reference)) {
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities to check if any character elements of a list have a
// "known" encoding (UTF-8 or Latin1). This implies that we have to convert
// all character elements of the list to UTF-8. Only `list_any_known_encoding()`
// is ever called directly.

static bool chr_any_known_encoding(SEXP x, R_len_t size);
static bool list_any_known_encoding(SEXP x, R_len_t size);
static bool df_any_known_encoding(SEXP x, R_len_t size);

static bool obj_any_known_encoding(SEXP x, R_len_t size) {
  switch(TYPEOF(x)) {
  case STRSXP: return chr_any_known_encoding(x, size);
  case VECSXP: return is_data_frame(x) ? df_any_known_encoding(x, size) : list_any_known_encoding(x, size);
  default: return false;
  }
}

// For usage on list elements. They have unknown size, and might be scalars.
static bool elt_any_known_encoding(SEXP x) {
  switch(TYPEOF(x)) {
  case STRSXP: return chr_any_known_encoding(x, vec_size(x));
  case VECSXP: return is_data_frame(x) ? df_any_known_encoding(x, vec_size(x)) : list_any_known_encoding(x, Rf_length(x));
  default: return false;
  }
}

static bool chr_any_known_encoding(SEXP x, R_len_t size) {
  if (size == 0) {
    return false;
  }

  const SEXP* p_x = STRING_PTR_RO(x);

  for (int i = 0; i < size; ++i, ++p_x) {
    if (Rf_getCharCE(*p_x) != CE_NATIVE) {
      return true;
    }
  }

  return false;
}

static bool list_any_known_encoding(SEXP x, R_len_t size) {
  SEXP elt;

  for (int i = 0; i < size; ++i) {
    elt = VECTOR_ELT(x, i);

    if (elt_any_known_encoding(elt)) {
      return true;
    }
  }

  return false;
}

static bool df_any_known_encoding(SEXP x, R_len_t size) {
  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    if (obj_any_known_encoding(VECTOR_ELT(x, i), size)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------

static bool attr_any_known_encoding(SEXP x);
static bool list_attr_any_known_encoding(SEXP x, R_len_t size);
static bool df_attr_any_known_encoding(SEXP x, R_len_t size);

static bool obj_attr_any_known_encoding(SEXP x, R_len_t size) {
  if (TYPEOF(x) != VECSXP) {
    return attr_any_known_encoding(x);
  }

  if (is_data_frame(x)) {
    return df_attr_any_known_encoding(x, size);
  }

  return list_attr_any_known_encoding(x, size);
}

static bool elt_attr_any_known_encoding(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return attr_any_known_encoding(x);
  }

  if (is_data_frame(x)) {
    return df_attr_any_known_encoding(x, vec_size(x));
  }

  return list_attr_any_known_encoding(x, Rf_length(x));
}

// It is not possible for the attribute list to have names that are non UTF-8,
// as they are converted with `installTrChar()` on the way in.

static bool attr_any_known_encoding(SEXP x) {
  SEXP attrib = ATTRIB(x);

  if (attrib == R_NilValue) {
    return false;
  }

  for (SEXP node = attrib; node != R_NilValue; node = CDR(node)) {
    if (elt_any_known_encoding(CAR(node))) {
      return true;
    }
  }

  return false;
}

static bool list_attr_any_known_encoding(SEXP x, R_len_t size) {
  if (attr_any_known_encoding(x)) {
    return true;
  }

  SEXP elt;

  for (int i = 0; i < size; ++i) {
    if (elt_attr_any_known_encoding(VECTOR_ELT(x, i))) {
      return true;
    }
  }

  return false;
}

static bool df_attr_any_known_encoding(SEXP x, R_len_t size) {
  if (attr_any_known_encoding(x)) {
    return true;
  }

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    if (obj_attr_any_known_encoding(VECTOR_ELT(x, i), size)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities to translate all character vector elements of an object to UTF-8.
// This does not check if a translation is required.

static SEXP chr_translate_encoding(SEXP x, R_len_t size);
static SEXP list_translate_encoding(SEXP x, R_len_t size);
static SEXP df_translate_encoding(SEXP x, R_len_t size);

static SEXP obj_translate_encoding(SEXP x, R_len_t size) {
  switch (TYPEOF(x)) {
  case STRSXP: return chr_translate_encoding(x, size);
  case VECSXP: return is_data_frame(x) ? df_translate_encoding(x, size) : list_translate_encoding(x, size);
  default: return x;
  }
}

// For usage on list elements. They have unknown size, and might be scalars.
static SEXP elt_translate_encoding(SEXP x) {
  switch (TYPEOF(x)) {
  case STRSXP: return chr_translate_encoding(x, vec_size(x));
  case VECSXP: return is_data_frame(x) ? df_translate_encoding(x, vec_size(x)) : list_translate_encoding(x, Rf_length(x));
  default: return x;
  }
}

static SEXP chr_translate_encoding(SEXP x, R_len_t size) {
  if (size == 0) {
    return x;
  }

  const SEXP* p_x = STRING_PTR_RO(x);

  SEXP out = PROTECT(r_maybe_duplicate(x));
  SEXP* p_out = STRING_PTR(out);

  SEXP chr;
  const void *vmax = vmaxget();

  for (int i = 0; i < size; ++i, ++p_x, ++p_out) {
    chr = *p_x;

    if (Rf_getCharCE(chr) == CE_UTF8) {
      *p_out = chr;
      continue;
    }

    *p_out = Rf_mkCharCE(Rf_translateCharUTF8(chr), CE_UTF8);
  }

  vmaxset(vmax);
  UNPROTECT(1);
  return out;
}

static SEXP list_translate_encoding(SEXP x, R_len_t size) {
  SEXP elt;
  x = PROTECT(r_maybe_duplicate(x));

  for (int i = 0; i < size; ++i) {
    elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, elt_translate_encoding(elt));
  }

  UNPROTECT(1);
  return x;
}

static SEXP df_translate_encoding(SEXP x, R_len_t size) {
  SEXP col;
  x = PROTECT(r_maybe_duplicate(x));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    col = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_translate_encoding(col, size));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------

static SEXP attr_translate_encoding(SEXP x);
static SEXP list_attr_translate_encoding(SEXP x, R_len_t size);
static SEXP df_attr_translate_encoding(SEXP x, R_len_t size);

static SEXP obj_attr_translate_encoding(SEXP x, R_len_t size) {
  if (TYPEOF(x) != VECSXP) {
    return attr_translate_encoding(x);
  }

  if (is_data_frame(x)) {
    return df_attr_translate_encoding(x, size);
  }

  return list_attr_translate_encoding(x, size);
}

static SEXP elt_attr_translate_encoding(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return attr_translate_encoding(x);
  }

  if (is_data_frame(x)) {
    return df_attr_translate_encoding(x, vec_size(x));
  }

  return list_attr_translate_encoding(x, Rf_length(x));
}

static SEXP attr_translate_encoding(SEXP x) {
  SEXP attrib = ATTRIB(x);

  if (attrib == R_NilValue) {
    return x;
  }

  x = PROTECT(r_maybe_duplicate(x));

  SEXP elt;
  attrib = PROTECT(Rf_shallow_duplicate(attrib));

  for (SEXP node = attrib; node != R_NilValue; node = CDR(node)) {
    elt = CAR(node);
    elt = PROTECT(attr_translate_encoding(elt));
    elt = PROTECT(elt_translate_encoding(elt));
    SETCAR(node, elt);
    UNPROTECT(2);
  }

  SET_ATTRIB(x, attrib);

  UNPROTECT(2);
  return x;
}

static SEXP list_attr_translate_encoding(SEXP x, R_len_t size) {
  x = PROTECT(r_maybe_duplicate(x));
  x = PROTECT(attr_translate_encoding(x));

  SEXP elt;

  for (int i = 0; i < size; ++i) {
    elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, elt_attr_translate_encoding(elt));
  }

  UNPROTECT(2);
  return x;
}

static SEXP df_attr_translate_encoding(SEXP x, R_len_t size) {
  x = PROTECT(r_maybe_duplicate(x));
  x = PROTECT(attr_translate_encoding(x));

  SEXP col;
  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    col = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_attr_translate_encoding(col, size));
  }

  UNPROTECT(2);
  return x;
}

// -----------------------------------------------------------------------------
// Utilities for translating encodings within one vector, if required.

// - If `x` is a character vector requiring translation, translate it.
// - If `x` is a list where any element has a "known" encoding, force a
//   translation of every element in the list.
// - If `x` is a data frame, translate the columns one by one, independently.

static SEXP obj_maybe_translate_encoding_impl(SEXP x, R_len_t size);
static SEXP obj_attr_maybe_translate_encoding_impl(SEXP x, R_len_t size);

static SEXP chr_maybe_translate_encoding(SEXP x, R_len_t size);
static SEXP list_maybe_translate_encoding(SEXP x, R_len_t size);
static SEXP df_maybe_translate_encoding(SEXP x, R_len_t size);

// [[ include("vctrs.h") ]]
SEXP obj_maybe_translate_encoding(SEXP x, R_len_t size) {
  x = PROTECT(obj_maybe_translate_encoding_impl(x, size));
  x = PROTECT(obj_attr_maybe_translate_encoding_impl(x, size));
  UNPROTECT(2);
  return x;
}

static SEXP obj_maybe_translate_encoding_impl(SEXP x, R_len_t size) {
  switch (TYPEOF(x)) {
  case STRSXP: return chr_maybe_translate_encoding(x, size);
  case VECSXP: return is_data_frame(x) ? df_maybe_translate_encoding(x, size) : list_maybe_translate_encoding(x, size);
  default: return x;
  }
}

static SEXP chr_maybe_translate_encoding(SEXP x, R_len_t size) {
  return chr_translation_required(x, size) ? chr_translate_encoding(x, size) : x;
}

static SEXP list_maybe_translate_encoding(SEXP x, R_len_t size) {
  return list_any_known_encoding(x, size) ? list_translate_encoding(x, size) : x;
}

static SEXP df_maybe_translate_encoding(SEXP x, R_len_t size) {
  int n_col = Rf_length(x);

  x = PROTECT(r_maybe_duplicate(x));

  SEXP elt;

  for (int i = 0; i < n_col; ++i) {
    elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_maybe_translate_encoding(elt, size));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------

static SEXP list_attr_maybe_translate_encoding(SEXP x, R_len_t size);
static SEXP df_attr_maybe_translate_encoding(SEXP x, R_len_t size);

static SEXP obj_attr_maybe_translate_encoding_impl(SEXP x, R_len_t size) {
  if (TYPEOF(x) != VECSXP) {
    return x;
  }

  if (is_data_frame(x)) {
    return df_attr_maybe_translate_encoding(x, size);
  }

  return list_attr_maybe_translate_encoding(x, size);
}

static SEXP list_attr_maybe_translate_encoding(SEXP x, R_len_t size) {
  return list_attr_any_known_encoding(x, size) ? list_attr_translate_encoding(x, size) : x;
}

static SEXP df_attr_maybe_translate_encoding(SEXP x, R_len_t size) {
  int n_col = Rf_length(x);

  x = PROTECT(r_maybe_duplicate(x));

  SEXP elt;

  for (int i = 0; i < n_col; ++i) {
    elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_attr_maybe_translate_encoding_impl(elt, size));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------
// Utilities for translating encodings of `x` and `y` relative to each other,
// if required.

static SEXP translate_none(SEXP x, SEXP y);
static SEXP chr_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP list_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP df_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);

static SEXP obj_maybe_translate_encoding2_impl(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP obj_attr_maybe_translate_encoding2_impl(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);

// Notes:
// - Assumes that `x` and `y` are the same type from calling `vec_cast()`.
// - Does not assume that `x` and `y` are the same size.
// - Returns a list holding `x` and `y` translated to their common encoding.

// [[ include("vctrs.h") ]]
SEXP obj_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(obj_maybe_translate_encoding2_impl(x, x_size, y, y_size));

  x = VECTOR_ELT(out, 0);
  y = VECTOR_ELT(out, 1);

  out = PROTECT(obj_attr_maybe_translate_encoding2_impl(x, x_size, y, y_size));

  UNPROTECT(2);
  return out;
}

static SEXP obj_maybe_translate_encoding2_impl(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  switch (TYPEOF(x)) {
  case STRSXP: return chr_maybe_translate_encoding2(x, x_size, y, y_size);
  case VECSXP: return is_data_frame(x) ? df_maybe_translate_encoding2(x, x_size, y, y_size) : list_maybe_translate_encoding2(x, x_size, y, y_size);
  default: return translate_none(x, y);
  }
}

static SEXP translate_none(SEXP x, SEXP y) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(1);
  return out;
}

static SEXP chr_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (chr_translation_required2(x, x_size, y, y_size)) {
    SET_VECTOR_ELT(out, 0, chr_translate_encoding(x, x_size));
    SET_VECTOR_ELT(out, 1, chr_translate_encoding(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP list_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (list_any_known_encoding(x, x_size) || list_any_known_encoding(y, y_size)) {
    SET_VECTOR_ELT(out, 0, list_translate_encoding(x, x_size));
    SET_VECTOR_ELT(out, 1, list_translate_encoding(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP df_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP x_elt;
  SEXP y_elt;
  SEXP translated;

  x = PROTECT(r_maybe_duplicate(x));
  y = PROTECT(r_maybe_duplicate(y));

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    x_elt = VECTOR_ELT(x, i);
    y_elt = VECTOR_ELT(y, i);

    translated = PROTECT(
      obj_maybe_translate_encoding2(x_elt, x_size, y_elt, y_size)
    );

    SET_VECTOR_ELT(x, i, VECTOR_ELT(translated, 0));
    SET_VECTOR_ELT(y, i, VECTOR_ELT(translated, 1));

    UNPROTECT(1);
  }

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

static SEXP attr_maybe_translate_encoding2(SEXP x, SEXP y);
static SEXP list_attr_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP df_attr_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);

static SEXP obj_attr_maybe_translate_encoding2_impl(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  if (TYPEOF(x) != VECSXP) {
    return attr_maybe_translate_encoding2(x, y);
  }

  if (is_data_frame(x)) {
    return df_attr_maybe_translate_encoding2(x, x_size, y, y_size);
  }

  return list_attr_maybe_translate_encoding2(x, x_size, y, y_size);
}

static SEXP attr_maybe_translate_encoding2(SEXP x, SEXP y) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (attr_any_known_encoding(x) || attr_any_known_encoding(y)) {
    SET_VECTOR_ELT(out, 0, attr_translate_encoding(x));
    SET_VECTOR_ELT(out, 1, attr_translate_encoding(y));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP list_attr_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (list_attr_any_known_encoding(x, x_size) || list_attr_any_known_encoding(y, y_size)) {
    SET_VECTOR_ELT(out, 0, list_attr_translate_encoding(x, x_size));
    SET_VECTOR_ELT(out, 1, list_attr_translate_encoding(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP df_attr_maybe_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP x_elt;
  SEXP y_elt;
  SEXP translated;

  x = PROTECT(r_maybe_duplicate(x));
  y = PROTECT(r_maybe_duplicate(y));

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    x_elt = VECTOR_ELT(x, i);
    y_elt = VECTOR_ELT(y, i);

    translated = PROTECT(
      obj_attr_maybe_translate_encoding2_impl(x_elt, x_size, y_elt, y_size)
    );

    SET_VECTOR_ELT(x, i, VECTOR_ELT(translated, 0));
    SET_VECTOR_ELT(y, i, VECTOR_ELT(translated, 1));

    UNPROTECT(1);
  }

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(3);
  return out;
}

// -----------------------------------------------------------------------------

// [[ register() ]]
SEXP vctrs_maybe_translate_encoding(SEXP x) {
  return obj_maybe_translate_encoding(x, vec_size(x));
}

// [[ register() ]]
SEXP vctrs_maybe_translate_encoding2(SEXP x, SEXP y) {
  struct vctrs_arg args_x = new_wrapper_arg(NULL, "x");
  struct vctrs_arg args_y = new_wrapper_arg(NULL, "y");

  int _;

  SEXP type = PROTECT(vec_type2(x, y, &args_x, &args_y, &_));

  x = PROTECT(vec_cast(x, type, args_empty, args_empty));
  y = PROTECT(vec_cast(y, type, args_empty, args_empty));

  x = PROTECT(vec_proxy_equal(x));
  y = PROTECT(vec_proxy_equal(y));

  UNPROTECT(5);
  return obj_maybe_translate_encoding2(x, vec_size(x), y, vec_size(y));
}

