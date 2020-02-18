#include "vctrs.h"
#include "utils.h"
#include "type-data-frame.h"
#include "subscript-loc.h"

static inline SEXP vec_lengthen_ptype(SEXP x);
static inline SEXP vec_lengthen_recycle(SEXP x, R_len_t size);
static SEXP new_lengthen_data_frame(SEXP loc, SEXP cols, R_len_t size, SEXP ptype, bool has_ptype);
static inline R_len_t update_size(R_len_t size, SEXP x);
static inline R_len_t finalise_size(R_len_t size);
static inline SEXP pull_ptype_column(SEXP ptype, R_len_t subscript);

SEXP vec_lengthen(SEXP x, SEXP ptype) {
  struct name_repair_opts name_repair_opts = {
    .type = name_repair_unique,
    .fn = R_NilValue,
    .quiet = false
  };

  x = PROTECT(as_df_row(x, &name_repair_opts));

  R_len_t n = Rf_length(x);
  R_len_t size = vec_size(x);

  bool has_ptype = (ptype != R_NilValue);
  if (has_ptype && !is_data_frame(ptype)) {
    Rf_errorcall(R_NilValue, "`ptype` must be a data frame");
  }

  if (size == 0) {
    SEXP cols = PROTECT(map(x, &vec_lengthen_ptype));
    SEXP out = PROTECT(new_lengthen_data_frame(vctrs_shared_empty_int, cols, 0, ptype, has_ptype));
    UNPROTECT(3);
    return out;
  }

  SEXP sizes = PROTECT(Rf_allocVector(INTSXP, size));
  r_int_fill(sizes, NA_INTEGER, size);
  int* p_sizes = INTEGER(sizes);

  SEXP index = PROTECT(Rf_allocVector(INTSXP, 1));
  int* p_index = INTEGER(index);

  // Evaluate in a child of the global environment to allow dispatch
  // to custom functions. We define `[` to point to its base
  // definition to ensure consistent look-up. This is the same logic
  // as in `vctrs_dispatch_n()`, reimplemented here to allow repeated
  // evaluations in a loop.
  SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 2));

  SEXP call = PROTECT(Rf_lang3(syms_bracket2, syms_x, syms_i));
  Rf_defineVar(syms_bracket2, fns_bracket2, env);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    Rf_defineVar(syms_x, col, env);

    for (R_len_t j = 0; j < size; ++j) {
      *p_index = j + 1;
      Rf_defineVar(syms_i, index, env);

      // TODO: col[[j]] -> vec_slice2(col, j)
      SEXP elt = PROTECT(Rf_eval(call, env));

      p_sizes[j] = update_size(p_sizes[j], elt);

      UNPROTECT(1);
    }
  }

  for (R_len_t i = 0; i < size; ++i) {
    p_sizes[i] = finalise_size(p_sizes[i]);
  }

  SEXP names = PROTECT(r_names(x));

  SEXP ptype_subscripts = vctrs_shared_empty_int;
  int* p_ptype_subscripts;

  if (has_ptype) {
    SEXP names_ptype = PROTECT(r_names(ptype));
    if (names_ptype == R_NilValue) {
      names_ptype = vctrs_shared_empty_chr;
    }

    ptype_subscripts = vec_match(names, names_ptype);

    UNPROTECT(1);
  }
  PROTECT(ptype_subscripts);
  p_ptype_subscripts = INTEGER(ptype_subscripts);

  SEXP cols = PROTECT(Rf_allocVector(VECSXP, n));
  Rf_setAttrib(cols, R_NamesSymbol, names);

  SEXP pieces = PROTECT(Rf_allocVector(VECSXP, size));

  const struct name_repair_opts vec_c_name_repair_opts = {
    .type = name_repair_none,
    .fn = R_NilValue
  };

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = VECTOR_ELT(x, i);
    Rf_defineVar(syms_x, col, env);

    for (R_len_t j = 0; j < size; ++j) {
      *p_index = j + 1;
      Rf_defineVar(syms_i, index, env);

      // TODO: col[[j]] -> vec_slice2(col, j)
      SEXP elt = PROTECT(Rf_eval(call, env));
      elt = vec_lengthen_recycle(elt, p_sizes[j]);

      SET_VECTOR_ELT(pieces, j, elt);
      UNPROTECT(1);
    }

    SEXP ptype_col = R_NilValue;
    if (has_ptype) {
      ptype_col = pull_ptype_column(ptype, p_ptype_subscripts[i]);
    }

    col = vec_c(pieces, ptype_col, R_NilValue, &vec_c_name_repair_opts);
    SET_VECTOR_ELT(cols, i, col);
  }

  R_len_t out_size = 0;
  for (R_len_t i = 0; i < size; ++i) {
    out_size += p_sizes[i];
  }

  SEXP loc = PROTECT(Rf_allocVector(INTSXP, out_size));
  int* p_loc = INTEGER(loc);

  R_len_t loc_idx = 0;

  for (R_len_t i = 0; i < size; ++i) {
    R_len_t loc_elt = i + 1;
    R_len_t sizes_elt = p_sizes[i];

    for (R_len_t j = 0; j < sizes_elt; ++j) {
      p_loc[loc_idx] = loc_elt;
      ++loc_idx;
    }
  }

  SEXP out = PROTECT(new_lengthen_data_frame(loc, cols, out_size, ptype, has_ptype));

  UNPROTECT(11);
  return out;
}

static inline SEXP pull_ptype_column(SEXP ptype, R_len_t subscript) {
  if (subscript == NA_INTEGER) {
    return R_NilValue;
  } else {
    return VECTOR_ELT(ptype, subscript - 1);
  }
}

static inline SEXP vec_lengthen_ptype(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return vec_type(x);
  }

  // Bare list
  if (!OBJECT(x)) {
    return vec_unspecified(0);
  }

  // TODO: Update if `list_of()` explicitly inherits from `"list"`
  if (Rf_inherits(x, "vctrs_list_of")) {
    return Rf_getAttrib(x, syms_ptype);
  }

  if (Rf_inherits(x, "list")) {
    return vec_unspecified(0);
  }

  return vec_type(x);
}

static inline SEXP vec_lengthen_recycle(SEXP x, R_len_t size) {
  if (x == R_NilValue) {
    return vec_unspecified(size);
  } else {
    return vec_recycle(x, size, args_empty);
  }
}

static SEXP new_lengthen_data_frame(SEXP loc, SEXP cols, R_len_t size, SEXP ptype, bool has_ptype) {
  init_data_frame(cols, size);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, loc);

  if (has_ptype) {
    cols = vec_cast(cols, ptype, args_empty, args_empty);
  }

  SET_VECTOR_ELT(out, 1, cols);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, strings_loc);
  SET_STRING_ELT(names, 1, strings_val);

  Rf_setAttrib(out, R_NamesSymbol, names);
  init_data_frame(out, size);

  UNPROTECT(2);
  return out;
}

static inline R_len_t vec_lengthen_size(SEXP x) {
  return (x == R_NilValue) ? NA_INTEGER : vec_size(x);
}

static inline R_len_t size2(R_len_t x_size, R_len_t y_size) {
  if (x_size == NA_INTEGER) {
    return y_size;
  } else if (y_size == NA_INTEGER) {
    return x_size;
  } else if (x_size == y_size) {
    return x_size;
  } else if (x_size == 1) {
    return y_size;
  } else if (y_size == 1) {
    return x_size;
  } else {
    Rf_errorcall(R_NilValue, "Incompatible lengths: %i, %i.", x_size, y_size);
  }
}

static inline R_len_t update_size(R_len_t size, SEXP x) {
  return size2(size, vec_lengthen_size(x));
}

static inline R_len_t finalise_size(R_len_t size) {
  return (size == NA_INTEGER) ? 0 : size;
}

