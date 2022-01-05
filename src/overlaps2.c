#include <rlang.h>
#include "vctrs.h"
#include "order.h"
#include "translate.h"
#include "poly-op.h"

// -----------------------------------------------------------------------------

static inline
r_obj* interval_order(r_obj* start, r_obj* end) {
  // Put them in a data frame to compute joint ordering
  r_obj* df = KEEP(r_new_list(2));
  r_list_poke(df, 0, start);
  r_list_poke(df, 1, end);

  r_obj* df_names = r_new_character(2);
  r_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("start"));
  r_chr_poke(df_names, 1, r_str("end"));

  r_init_data_frame(df, r_length(start));

  // Could be a callback to R here instead if this lived in another package
  r_obj* direction = KEEP(r_chr("asc"));
  r_obj* na_value = KEEP(r_chr("largest"));
  bool nan_distinct = false;
  r_obj* chr_proxy_collate = r_null;

  r_obj* out = KEEP(vec_order(
    df,
    direction,
    na_value,
    nan_distinct,
    chr_proxy_collate
  ));

  FREE(4);
  return out;
}

// -----------------------------------------------------------------------------

/*
 * Assumptions:
 * - `start < end`
 * - If `start == NA`, then `end == NA` and vice versa
 * - `vec_size(start) == vec_size(end)`
 * - `vec_ptype(start) == vec_ptype(end)`
 */
static
r_obj* vec_locate_minimal_interval(r_obj* start, r_obj* end, bool groups) {
  int n_prot = 0;

  const r_ssize size = vec_size(start);

  if (size != vec_size(end)) {
    r_stop_internal(
      "vec_locate_minimal_interval",
      "`start` and `end` must have the same size."
    );
  }

  if (vec_typeof(start) != vec_typeof(end)) {
    r_stop_internal(
      "vec_locate_minimal_interval",
      "`start` and `end` must have the same type."
    );
  }

  r_obj* start_proxy_equal = KEEP_N(vec_proxy_equal(start), &n_prot);
  start_proxy_equal = KEEP_N(vec_normalize_encoding(start_proxy_equal), &n_prot);

  const enum vctrs_type type_proxy_equal = vec_proxy_typeof(start_proxy_equal);

  struct poly_vec* p_start_missing = new_poly_vec(start_proxy_equal, type_proxy_equal);
  PROTECT_POLY_VEC(p_start_missing, &n_prot);
  const void* p_start_missing_vec = p_start_missing->p_vec;

  const poly_unary_bool_fn_ptr fn_p_is_missing = new_poly_p_is_missing2(type_proxy_equal);

  r_obj* start_proxy_compare = KEEP_N(vec_proxy_compare(start), &n_prot);
  start_proxy_compare = KEEP_N(vec_normalize_encoding(start_proxy_compare), &n_prot);

  r_obj* end_proxy_compare = KEEP_N(vec_proxy_compare(end), &n_prot);
  end_proxy_compare = KEEP_N(vec_normalize_encoding(end_proxy_compare), &n_prot);

  const enum vctrs_type type_proxy_compare = vec_proxy_typeof(start_proxy_compare);

  struct poly_vec* p_start_compare = new_poly_vec(start_proxy_compare, type_proxy_compare);
  PROTECT_POLY_VEC(p_start_compare, &n_prot);
  const void* p_start_compare_vec = p_start_compare->p_vec;

  struct poly_vec* p_end_compare = new_poly_vec(end_proxy_compare, type_proxy_compare);
  PROTECT_POLY_VEC(p_end_compare, &n_prot);
  const void* p_end_compare_vec = p_end_compare->p_vec;

  const poly_binary_int_fn_ptr fn_p_compare = new_poly_p_compare_na_equal(type_proxy_compare);

  r_obj* order = KEEP_N(interval_order(start, end), &n_prot);
  const int* v_order = r_int_cbegin(order);

  // Assume the data can be collapsed in half to start with.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_starts->shelter, &n_prot);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_ends->shelter, &n_prot);

  r_ssize loc_order_start = 0;
  struct r_dyn_array* p_loc = NULL;
  r_obj* loc_shelter = r_null;
  if (groups) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    loc_shelter = p_loc->shelter;
  }
  KEEP_N(loc_shelter, &n_prot);

  r_ssize i = 0;
  r_ssize loc_set_start = r_globals.na_int;
  r_ssize loc_set_end = r_globals.na_int;

  // Find first non-NA interval
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    if (!fn_p_is_missing(p_start_missing_vec, loc)) {
      loc_set_start = loc;
      loc_set_end = loc;
      ++i;
      break;
    }
  }

  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    if (fn_p_is_missing(p_start_missing_vec, loc)) {
      // NA intervals are always at the end
      break;
    }

    if (fn_p_compare(p_end_compare_vec, loc_set_end, p_start_compare_vec, loc) == -1) {
      const r_ssize loc_order_end = i - 1;
      const r_ssize loc_size = loc_order_end - loc_order_start + 1;

      int loc_start = v_order[loc_order_start];
      int loc_end = v_order[loc_order_end];

      r_int_push_back(p_starts, loc_start);
      r_int_push_back(p_ends, loc_end);

      if (groups) {
        r_obj* loc = r_new_integer(loc_size);
        r_list_push_back(p_loc, loc);
        int* v_loc = r_int_begin(loc);

        const int* v_order_start = v_order + loc_order_start;
        memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
      }

      loc_order_start = loc_order_end + 1;

      loc_set_start = loc;
      loc_set_end = loc;
    } else if (fn_p_compare(p_end_compare_vec, loc_set_end, p_end_compare_vec, loc) == -1) {
      loc_set_end = loc;
    }
  }

  if (loc_set_start != r_globals.na_int) {
    const r_ssize loc_order_end = i - 1;
    const r_ssize loc_size = loc_order_end - loc_order_start + 1;

    int loc_start = v_order[loc_order_start];
    int loc_end = v_order[loc_order_end];

    r_int_push_back(p_starts, loc_start);
    r_int_push_back(p_ends, loc_end);

    if (groups) {
      r_obj* loc = r_new_integer(loc_size);
      r_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  r_obj* key = KEEP_N(r_new_list(2), &n_prot);
  r_list_poke(key, 0, r_arr_unwrap(p_starts));
  r_list_poke(key, 1, r_arr_unwrap(p_ends));

  r_obj* key_names = r_new_character(2);
  r_poke_names(key, key_names);
  r_chr_poke(key_names, 0, r_str("start"));
  r_chr_poke(key_names, 1, r_str("end"));

  r_init_data_frame(key, p_starts->count);

  r_obj* out = r_null;
  r_keep_t out_shelter;
  KEEP_HERE(out, &out_shelter);
  ++n_prot;

  if (groups) {
    out = r_new_list(2);
    KEEP_AT(out, out_shelter);
    r_list_poke(out, 0, key);
    r_list_poke(out, 1, r_arr_unwrap(p_loc));

    r_obj* out_names = r_new_character(2);
    r_poke_names(out, out_names);
    r_chr_poke(out_names, 0, r_str("key"));
    r_chr_poke(out_names, 1, r_str("loc"));

    r_init_data_frame(out, p_starts->count);
  } else {
    out = key;
  }

  FREE(n_prot);
  return out;
}

// [[ register() ]]
r_obj* vctrs_locate_minimal_interval(r_obj* start, r_obj* end) {
  const bool groups = false;
  return vec_locate_minimal_interval(start, end, groups);
}

// [[ register() ]]
r_obj* vctrs_locate_minimal_interval_groups(r_obj* start, r_obj* end) {
  const bool groups = true;
  return vec_locate_minimal_interval(start, end, groups);
}
