#ifndef VCTRS_EQUAL_H
#define VCTRS_EQUAL_H

int lgl_equal_scalar(const int* x, const int* y, bool na_equal);
int int_equal_scalar(const int* x, const int* y, bool na_equal);
int dbl_equal_scalar(const double* x, const double* y, bool na_equal);
int raw_equal_scalar(const Rbyte* x, const Rbyte* y, bool na_equal);
int cpl_equal_scalar(const Rcomplex* x, const Rcomplex* y, bool na_equal);
int chr_equal_scalar(const SEXP* x, const SEXP* y, bool na_equal);
int list_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);
int df_equal_scalar(SEXP x, R_len_t i, SEXP y, R_len_t j, bool na_equal);

#define EQUAL_ITERATE(FOR, SETUP, CODE, CTYPE, CONST_DEREF, SCALAR_EQUAL)  \
  do {                                                                     \
    const CTYPE* xp = CONST_DEREF(_x);                                     \
    const CTYPE* yp = CONST_DEREF(_y);                                     \
    FOR {                                                                  \
      SETUP                                                                \
      bool _equal = SCALAR_EQUAL(xp + _i, yp + _j, _na_equal);             \
      CODE                                                                 \
    }                                                                      \
  }                                                                        \
  while (0)

#define EQUAL_ITERATE_BARRIER(FOR, SETUP, CODE, SCALAR_EQUAL)     \
  do {                                                            \
    FOR {                                                         \
      SETUP                                                       \
      bool _equal = SCALAR_EQUAL(_x, _i, _y, _j, _na_equal);      \
      CODE                                                        \
    }                                                             \
  }                                                               \
  while (0)

#define EQUAL_APPLY(FOR, SETUP, CODE)                                                                           \
  do {                                                                                                          \
    enum vctrs_type type = vec_proxy_typeof(_x);                                                                \
    if (type != vec_proxy_typeof(_y) || vec_size(_x) != vec_size(_y)) {                                         \
      Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");                                 \
    }                                                                                                           \
                                                                                                                \
    switch (type) {                                                                                             \
    case vctrs_type_logical:   EQUAL_ITERATE(FOR, SETUP, CODE, int, LOGICAL_RO, lgl_equal_scalar); break;       \
    case vctrs_type_integer:   EQUAL_ITERATE(FOR, SETUP, CODE, int, INTEGER_RO, int_equal_scalar); break;       \
    case vctrs_type_double:    EQUAL_ITERATE(FOR, SETUP, CODE, double, REAL_RO, dbl_equal_scalar); break;       \
    case vctrs_type_raw:       EQUAL_ITERATE(FOR, SETUP, CODE, Rbyte, RAW_RO, raw_equal_scalar); break;         \
    case vctrs_type_complex:   EQUAL_ITERATE(FOR, SETUP, CODE, Rcomplex, COMPLEX_RO, cpl_equal_scalar); break;  \
    case vctrs_type_character: EQUAL_ITERATE(FOR, SETUP, CODE, SEXP, STRING_PTR_RO, chr_equal_scalar); break;   \
    case vctrs_type_list:      EQUAL_ITERATE_BARRIER(FOR, SETUP, CODE, list_equal_scalar); break;               \
    case vctrs_type_dataframe: EQUAL_ITERATE_BARRIER(FOR, SETUP, CODE, df_equal_scalar); break;                 \
    case vctrs_type_scalar:    Rf_errorcall(R_NilValue, "Can't compare scalars with `vctrs_equal()`");          \
    default:                   Rf_error("Unimplemented type in `vctrs_equal()`");                               \
    }                                                                                                           \
  }                                                                                                             \
  while (0)


#endif
