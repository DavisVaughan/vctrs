//** File generated automatically by cbuild - please do not modify by hand

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h> // for bool
#include <R_ext/Rdynload.h>

// .Call declarations
extern SEXP altrep_rle_Make(SEXP);
extern SEXP vctrs_as_df_row(SEXP, SEXP);
extern SEXP vctrs_as_df_col(SEXP, SEXP);
extern SEXP vctrs_df_as_dataframe(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_coercible_cast(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_compare(SEXP, SEXP, SEXP);
extern SEXP vctrs_unique_loc(SEXP);
extern SEXP vctrs_duplicated_any(SEXP);
extern SEXP vctrs_n_distinct(SEXP);
extern SEXP vctrs_id(SEXP);
extern SEXP vctrs_match(SEXP, SEXP);
extern SEXP vctrs_in(SEXP, SEXP);
extern SEXP vctrs_count(SEXP);
extern SEXP vctrs_duplicated(SEXP);
extern SEXP vctrs_equal(SEXP, SEXP, SEXP);
extern SEXP vctrs_equal_object(SEXP, SEXP);
extern SEXP vctrs_duplicate_all(SEXP);
extern SEXP vctrs_equal_na(SEXP);
extern SEXP vctrs_list_get(SEXP, SEXP);
extern SEXP vctrs_list_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_fields(SEXP);
extern SEXP vctrs_n_fields(SEXP);
extern SEXP vctrs_field_get(SEXP, SEXP);
extern SEXP vctrs_field_set(SEXP, SEXP, SEXP);
extern SEXP vctrs_group_id(SEXP);
extern SEXP vctrs_group_rle(SEXP);
extern SEXP vec_group_pos(SEXP);
extern SEXP vctrs_hash_object(SEXP);
extern SEXP vctrs_hash(SEXP);
extern SEXP vctrs_init_library(SEXP);
extern SEXP vec_names(SEXP);
extern SEXP vctrs_as_minimal_names(SEXP);
extern SEXP vctrs_minimal_names(SEXP);
extern SEXP vctrs_as_unique_names(SEXP, SEXP);
extern SEXP vctrs_is_unique_names(SEXP);
extern SEXP vctrs_unique_names(SEXP, SEXP);
extern SEXP vctrs_outer_names(SEXP, SEXP, SEXP);
extern SEXP vctrs_apply_name_spec(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_set_names(SEXP, SEXP);
extern SEXP vec_restore_default(SEXP, SEXP);
extern SEXP vctrs_df_restore(SEXP, SEXP, SEXP);
extern SEXP vec_restore(SEXP, SEXP, SEXP);
extern SEXP vec_proxy(SEXP);
extern SEXP vec_proxy_equal(SEXP);
extern SEXP vctrs_proxy_recursive(SEXP, SEXP);
extern SEXP vec_dim(SEXP);
extern SEXP vctrs_dim_n(SEXP);
extern SEXP vctrs_size(SEXP);
extern SEXP vctrs_df_size(SEXP);
extern SEXP vctrs_recycle(SEXP, SEXP);
extern SEXP vec_assign(SEXP, SEXP, SEXP);
extern SEXP vctrs_slice(SEXP, SEXP);
extern SEXP vctrs_init(SEXP, SEXP);
extern SEXP vec_slice_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice_rep(SEXP, SEXP, SEXP);
extern SEXP vctrs_as_index(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_chop_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_chop(SEXP, SEXP);
extern SEXP vec_split(SEXP, SEXP);
extern SEXP vctrs_maybe_translate_encoding(SEXP);
extern SEXP vctrs_maybe_translate_encoding2(SEXP, SEXP);
extern SEXP vctrs_type_info(SEXP);
extern SEXP vctrs_proxy_info(SEXP);
extern SEXP vctrs_is_vector(SEXP);
extern SEXP vctrs_typeof(SEXP, SEXP);
extern SEXP vec_type(SEXP);
extern SEXP vec_type_finalise(SEXP);
extern SEXP vctrs_typeof2(SEXP, SEXP);
extern SEXP vctrs_type2(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type2_df_df(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_unspecified(SEXP);
extern SEXP vctrs_is_unspecified(SEXP);
extern SEXP vctrs_class_type(SEXP);
extern SEXP vctrs_set_attributes(SEXP, SEXP);

// .Call entries
static const R_CallMethodDef CallEntries[] = {
  {"vctrs_rle",                       (DL_FUNC) &altrep_rle_Make, 1},
  {"vctrs_as_df_row",                 (DL_FUNC) &vctrs_as_df_row, 2},
  {"vctrs_as_df_col",                 (DL_FUNC) &vctrs_as_df_col, 2},
  {"vctrs_df_as_dataframe",           (DL_FUNC) &vctrs_df_as_dataframe, 4},
  {"vctrs_cast",                      (DL_FUNC) &vctrs_cast, 4},
  {"vctrs_coercible_cast",            (DL_FUNC) &vctrs_coercible_cast, 4},
  {"vctrs_compare",                   (DL_FUNC) &vctrs_compare, 3},
  {"vctrs_unique_loc",                (DL_FUNC) &vctrs_unique_loc, 1},
  {"vctrs_duplicated_any",            (DL_FUNC) &vctrs_duplicated_any, 1},
  {"vctrs_n_distinct",                (DL_FUNC) &vctrs_n_distinct, 1},
  {"vctrs_id",                        (DL_FUNC) &vctrs_id, 1},
  {"vctrs_match",                     (DL_FUNC) &vctrs_match, 2},
  {"vctrs_in",                        (DL_FUNC) &vctrs_in, 2},
  {"vctrs_count",                     (DL_FUNC) &vctrs_count, 1},
  {"vctrs_duplicated",                (DL_FUNC) &vctrs_duplicated, 1},
  {"vctrs_equal",                     (DL_FUNC) &vctrs_equal, 3},
  {"vctrs_equal_object",              (DL_FUNC) &vctrs_equal_object, 2},
  {"vctrs_duplicate_all",             (DL_FUNC) &vctrs_duplicate_all, 1},
  {"vctrs_equal_na",                  (DL_FUNC) &vctrs_equal_na, 1},
  {"vctrs_list_get",                  (DL_FUNC) &vctrs_list_get, 2},
  {"vctrs_list_set",                  (DL_FUNC) &vctrs_list_set, 3},
  {"vctrs_fields",                    (DL_FUNC) &vctrs_fields, 1},
  {"vctrs_n_fields",                  (DL_FUNC) &vctrs_n_fields, 1},
  {"vctrs_field_get",                 (DL_FUNC) &vctrs_field_get, 2},
  {"vctrs_field_set",                 (DL_FUNC) &vctrs_field_set, 3},
  {"vctrs_group_id",                  (DL_FUNC) &vctrs_group_id, 1},
  {"vctrs_group_rle",                 (DL_FUNC) &vctrs_group_rle, 1},
  {"vctrs_group_pos",                 (DL_FUNC) &vec_group_pos, 1},
  {"vctrs_hash_object",               (DL_FUNC) &vctrs_hash_object, 1},
  {"vctrs_hash",                      (DL_FUNC) &vctrs_hash, 1},
  {"vctrs_init_library",              (DL_FUNC) &vctrs_init_library, 1},
  {"vctrs_names",                     (DL_FUNC) &vec_names, 1},
  {"vctrs_as_minimal_names",          (DL_FUNC) &vctrs_as_minimal_names, 1},
  {"vctrs_minimal_names",             (DL_FUNC) &vctrs_minimal_names, 1},
  {"vctrs_as_unique_names",           (DL_FUNC) &vctrs_as_unique_names, 2},
  {"vctrs_is_unique_names",           (DL_FUNC) &vctrs_is_unique_names, 1},
  {"vctrs_unique_names",              (DL_FUNC) &vctrs_unique_names, 2},
  {"vctrs_outer_names",               (DL_FUNC) &vctrs_outer_names, 3},
  {"vctrs_apply_name_spec",           (DL_FUNC) &vctrs_apply_name_spec, 4},
  {"vctrs_set_names",                 (DL_FUNC) &vec_set_names, 2},
  {"vctrs_restore_default",           (DL_FUNC) &vec_restore_default, 2},
  {"vctrs_df_restore",                (DL_FUNC) &vctrs_df_restore, 3},
  {"vctrs_restore",                   (DL_FUNC) &vec_restore, 3},
  {"vctrs_proxy",                     (DL_FUNC) &vec_proxy, 1},
  {"vctrs_proxy_equal",               (DL_FUNC) &vec_proxy_equal, 1},
  {"vctrs_proxy_recursive",           (DL_FUNC) &vctrs_proxy_recursive, 2},
  {"vctrs_dim",                       (DL_FUNC) &vec_dim, 1},
  {"vctrs_dim_n",                     (DL_FUNC) &vctrs_dim_n, 1},
  {"vctrs_size",                      (DL_FUNC) &vctrs_size, 1},
  {"vctrs_df_size",                   (DL_FUNC) &vctrs_df_size, 1},
  {"vctrs_recycle",                   (DL_FUNC) &vctrs_recycle, 2},
  {"vctrs_assign",                    (DL_FUNC) &vec_assign, 3},
  {"vctrs_slice",                     (DL_FUNC) &vctrs_slice, 2},
  {"vctrs_init",                      (DL_FUNC) &vctrs_init, 2},
  {"vctrs_slice_seq",                 (DL_FUNC) &vec_slice_seq, 4},
  {"vctrs_slice_rep",                 (DL_FUNC) &vec_slice_rep, 3},
  {"vctrs_as_index",                  (DL_FUNC) &vctrs_as_index, 4},
  {"vctrs_chop_seq",                  (DL_FUNC) &vctrs_chop_seq, 4},
  {"vctrs_chop",                      (DL_FUNC) &vctrs_chop, 2},
  {"vctrs_split",                     (DL_FUNC) &vec_split, 2},
  {"vctrs_maybe_translate_encoding",  (DL_FUNC) &vctrs_maybe_translate_encoding, 1},
  {"vctrs_maybe_translate_encoding2", (DL_FUNC) &vctrs_maybe_translate_encoding2, 2},
  {"vctrs_type_info",                 (DL_FUNC) &vctrs_type_info, 1},
  {"vctrs_proxy_info",                (DL_FUNC) &vctrs_proxy_info, 1},
  {"vctrs_is_vector",                 (DL_FUNC) &vctrs_is_vector, 1},
  {"vctrs_typeof",                    (DL_FUNC) &vctrs_typeof, 2},
  {"vctrs_type",                      (DL_FUNC) &vec_type, 1},
  {"vctrs_type_finalise",             (DL_FUNC) &vec_type_finalise, 1},
  {"vctrs_typeof2",                   (DL_FUNC) &vctrs_typeof2, 2},
  {"vctrs_type2",                     (DL_FUNC) &vctrs_type2, 4},
  {"vctrs_type2_df_df",               (DL_FUNC) &vctrs_type2_df_df, 4},
  {"vctrs_unspecified",               (DL_FUNC) &vctrs_unspecified, 1},
  {"vctrs_is_unspecified",            (DL_FUNC) &vctrs_is_unspecified, 1},
  {"vctrs_class_type",                (DL_FUNC) &vctrs_class_type, 1},
  {"vctrs_set_attributes",            (DL_FUNC) &vctrs_set_attributes, 2},
  {NULL, NULL, 0}
};

// .External2 declarations
extern SEXP vctrs_rbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cbind(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_c(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_cast_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_size_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_recycle_common(SEXP, SEXP, SEXP, SEXP);
extern SEXP vctrs_type_common(SEXP, SEXP, SEXP, SEXP);

// .External / .External2 entries
static const R_ExternalMethodDef ExtEntries[] = {
  {"vctrs_rbind",          (DL_FUNC) &vctrs_rbind, 3},
  {"vctrs_cbind",          (DL_FUNC) &vctrs_cbind, 3},
  {"vctrs_c",              (DL_FUNC) &vctrs_c, 3},
  {"vctrs_cast_common",    (DL_FUNC) &vctrs_cast_common, 1},
  {"vctrs_size_common",    (DL_FUNC) &vctrs_size_common, 2},
  {"vctrs_recycle_common", (DL_FUNC) &vctrs_recycle_common, 1},
  {"vctrs_type_common",    (DL_FUNC) &vctrs_type_common, 1},
  {NULL, NULL, 0}
};

// Callable API declarations
extern SEXP vec_size(SEXP);
extern SEXP vec_recycle(SEXP, SEXP);
extern SEXP vec_assign_impl(SEXP, SEXP, SEXP, SEXP);
extern SEXP vec_slice_impl(SEXP, SEXP);
extern SEXP vec_init(SEXP, SEXP);
extern SEXP vec_chop(SEXP, SEXP);

// Hidden callable API declarations
extern SEXP vec_is_vector(SEXP);
extern SEXP init_compact_seq(SEXP, SEXP, SEXP, SEXP);
extern SEXP compact_seq(SEXP, SEXP, SEXP);

// Init hook declarations
void vctrs_init_altrep_rle(DllInfo* dll);

void R_init_vctrs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // Hidden callable API registrations
  R_RegisterCCallable("vctrs", "vec_is_vector",    (DL_FUNC) &vec_is_vector);
  R_RegisterCCallable("vctrs", "init_compact_seq", (DL_FUNC) &init_compact_seq);
  R_RegisterCCallable("vctrs", "compact_seq",      (DL_FUNC) &compact_seq);

  // Callable API registrations
  R_RegisterCCallable("vctrs", "vec_names",       (DL_FUNC) &vec_names);
  R_RegisterCCallable("vctrs", "short_vec_size",  (DL_FUNC) &vec_size);
  R_RegisterCCallable("vctrs", "vec_recycle",     (DL_FUNC) &vec_recycle);
  R_RegisterCCallable("vctrs", "vec_assign_impl", (DL_FUNC) &vec_assign_impl);
  R_RegisterCCallable("vctrs", "vec_slice_impl",  (DL_FUNC) &vec_slice_impl);
  R_RegisterCCallable("vctrs", "short_vec_init",  (DL_FUNC) &vec_init);
  R_RegisterCCallable("vctrs", "vec_chop",        (DL_FUNC) &vec_chop);

  vctrs_init_altrep_rle(dll);
}

