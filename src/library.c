#include "vctrs.h"

void vctrs_init_cast(SEXP ns);
void vctrs_init_data(SEXP ns);
void vctrs_init_dictionary(SEXP ns);
void vctrs_init_names(SEXP ns);
void vctrs_init_proxy_restore(SEXP ns);
void vctrs_init_slice(SEXP ns);
void vctrs_init_slice_assign(SEXP ns);
void vctrs_init_type2(SEXP ns);
void vctrs_init_type(SEXP ns);
void vctrs_init_type_info(SEXP ns);
void vctrs_init_unspecified(SEXP ns);
void vctrs_init_utils(SEXP ns);

// [[ export() ]]
SEXP vctrs_init_library(SEXP ns) {
  vctrs_init_cast(ns);
  vctrs_init_data(ns);
  vctrs_init_dictionary(ns);
  vctrs_init_names(ns);
  vctrs_init_proxy_restore(ns);
  vctrs_init_slice(ns);
  vctrs_init_slice_assign(ns);
  vctrs_init_type2(ns);
  vctrs_init_type(ns);
  vctrs_init_type_info(ns);
  vctrs_init_unspecified(ns);
  vctrs_init_utils(ns);
  return R_NilValue;
}
