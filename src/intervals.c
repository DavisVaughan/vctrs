#include <rlang.h>
#include "vctrs.h"
#include "order.h"
#include "compare.h"
#include "complete.h"
#include "translate.h"
#include "poly-op.h"

#include "decl/intervals-decl.h"

// -----------------------------------------------------------------------------

// [[ register() ]]
r_obj* vctrs_interval_locate_minimal(r_obj* start,
                                     r_obj* end,
                                     r_obj* keep_empty,
                                     r_obj* keep_missing,
                                     r_obj* groups) {
  const bool c_keep_empty = r_as_bool(keep_empty);
  const bool c_keep_missing = r_as_bool(keep_missing);
  const bool c_groups = r_as_bool(groups);
  return vec_interval_locate_minimal(start, end, c_keep_empty, c_keep_missing, c_groups);
}

static
r_obj* vec_interval_locate_minimal(r_obj* start,
                                   r_obj* end,
                                   bool keep_empty,
                                   bool keep_missing,
                                   bool groups) {
  int n_prot = 0;

  const r_ssize size = vec_size(start);

  if (size != vec_size(end)) {
    r_abort("`start` and `end` must have the same size.");
  }

  int _;
  r_obj* ptype = vec_ptype2_params(
    start,
    end,
    args_start,
    args_end,
    DF_FALLBACK_quiet,
    &_
  );
  KEEP_N(ptype, &n_prot);

  start = vec_cast_params(
    start,
    ptype,
    args_start,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  );
  KEEP_N(start, &n_prot);

  end = vec_cast_params(
    end,
    ptype,
    args_end,
    args_empty,
    DF_FALLBACK_quiet,
    S3_FALLBACK_false
  );
  KEEP_N(end, &n_prot);


  r_obj* start_proxy = KEEP_N(vec_proxy_compare(start), &n_prot);
  start_proxy = KEEP_N(vec_normalize_encoding(start_proxy), &n_prot);

  r_obj* end_proxy = KEEP_N(vec_proxy_compare(end), &n_prot);
  end_proxy = KEEP_N(vec_normalize_encoding(end_proxy), &n_prot);

  const enum vctrs_type type_proxy = vec_proxy_typeof(start_proxy);

  struct poly_vec* p_start_shelter = new_poly_vec(start_proxy, type_proxy);
  PROTECT_POLY_VEC(p_start_shelter, &n_prot);
  const void* p_start = p_start_shelter->p_vec;

  struct poly_vec* p_end_shelter = new_poly_vec(end_proxy, type_proxy);
  PROTECT_POLY_VEC(p_end_shelter, &n_prot);
  const void* p_end = p_end_shelter->p_vec;

  const poly_binary_int_fn_ptr fn_compare = new_poly_p_compare_na_equal(type_proxy);


  /*
   * NA == (either incomplete), NA interval
   * -1 == (start >  end), not allowed
   *  0 == (start == end), empty interval
   *  1 == (start <  end), typical case
   *
   *  Note that we put `end` before `start` in the call here to get the
   *  comparison order above
   */
  r_obj* compare = KEEP_N(vec_compare(end, start, false), &n_prot);
  int* v_compare = r_int_begin(compare);

  // While `vec_compare(na_equal = false)` will propagate `NA`s, it will only
  // do so if it hits a missing value before it knows the comparison value.
  // We need to propagate any missing values, regardless of where they are.
  r_obj* complete = KEEP_N(interval_detect_complete(start, end), &n_prot);
  const int* v_complete = r_lgl_cbegin(complete);

  for (r_ssize i = 0; i < size; ++i) {
    if (!v_complete[i]) {
      v_compare[i] = r_globals.na_int;
    }
  }

  for (r_ssize i = 0; i < size; ++i) {
    if (v_compare[i] == -1) {
      r_abort("`start` must be less than or equal to `end`.");
    }
  }

  if (keep_empty) {
    // With `keep_empty`, we only care about using `compare` to order missing
    // values at the end. Empty intervals shouldn't be grouped separately.
    for (r_ssize i = 0; i < size; ++i) {
      if (v_compare[i] == 0) {
        v_compare[i] = 1;
      }
    }
  }


  r_obj* order = KEEP_N(interval_order(compare, start, end), &n_prot);
  const int* v_order = r_int_cbegin(order);


  // Assume the data can be collapsed in half to start with.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_loc_start = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_loc_start->shelter, &n_prot);

  struct r_dyn_array* p_loc_end = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP_N(p_loc_end->shelter, &n_prot);

  struct r_dyn_array* p_loc = NULL;
  r_obj* loc_shelter = r_null;
  if (groups) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    loc_shelter = p_loc->shelter;
  }
  KEEP_N(loc_shelter, &n_prot);


  r_ssize i = 0;

  r_ssize loc_order_missing_start = 0;
  r_ssize loc_order_missing_end = r_globals.na_int;

  // Move `i` past any missing intervals,
  // recording last missing interval location for later
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    if (v_compare[loc] != r_globals.na_int) {
      break;
    }

    loc_order_missing_end = i;
  }

  if (!keep_empty) {
    // Move `i` past any empty intervals
    for (; i < size; ++i) {
      const r_ssize loc = v_order[i] - 1;

      if (v_compare[loc] != 0) {
        break;
      }
    }
  }

  r_ssize loc_order_start = i;
  r_ssize loc_order_end = i;

  r_ssize loc_set_start = r_globals.na_int;
  r_ssize loc_set_end = r_globals.na_int;

  if (i < size) {
    // Set information about first usable interval
    const r_ssize loc = v_order[i] - 1;
    loc_set_start = loc;
    loc_set_end = loc;
    ++i;
  }

  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    if (fn_compare(p_end, loc_set_end, p_start, loc) == -1) {
      r_int_push_back(p_loc_start, loc_set_start + 1);
      r_int_push_back(p_loc_end, loc_set_end + 1);

      if (groups) {
        const r_ssize loc_size = loc_order_end - loc_order_start + 1;

        r_obj* loc = r_new_integer(loc_size);
        r_list_push_back(p_loc, loc);
        int* v_loc = r_int_begin(loc);

        const int* v_order_start = v_order + loc_order_start;
        memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
      }

      loc_order_start = loc_order_end + 1;
      loc_set_start = loc;
      loc_set_end = loc;
    } else if (fn_compare(p_end, loc_set_end, p_end, loc) == -1) {
      loc_set_end = loc;
    }

    loc_order_end = i;
  }

  if (loc_set_start != r_globals.na_int) {
    // Log last interval
    r_int_push_back(p_loc_start, loc_set_start + 1);
    r_int_push_back(p_loc_end, loc_set_end + 1);

    if (groups) {
      const r_ssize loc_size = loc_order_end - loc_order_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  if (keep_missing && loc_order_missing_end != r_globals.na_int) {
    // Log missing interval
    r_int_push_back(p_loc_start, r_globals.na_int);
    r_int_push_back(p_loc_end, r_globals.na_int);

    if (groups) {
      const r_ssize loc_size = loc_order_missing_end - loc_order_missing_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_missing_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  r_obj* key = KEEP_N(r_new_list(2), &n_prot);
  r_list_poke(key, 0, r_arr_unwrap(p_loc_start));
  r_list_poke(key, 1, r_arr_unwrap(p_loc_end));

  r_obj* key_names = r_new_character(2);
  r_poke_names(key, key_names);
  r_chr_poke(key_names, 0, r_str("start"));
  r_chr_poke(key_names, 1, r_str("end"));

  r_init_data_frame(key, p_loc_start->count);

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

    r_init_data_frame(out, p_loc_start->count);
  } else {
    out = key;
  }

  FREE(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

/*
 * `interval_order()` orders the `start` and `end` values of a vector of
 * intervals, but also groups them by their `compare` value. We sort in
 * ascending order and make NAs the smallest value, so we end up with:
 *
 * - Missing intervals first (`compare == NA`)
 * - Then empty intervals if `keep_empty = false` (since empty is `compare == 0`)
 * - Then typical intervals (with `compare == 1`)
 */
static inline
r_obj* interval_order(r_obj* compare, r_obj* start, r_obj* end) {
  // Put them in a data frame to compute joint ordering
  r_obj* df = KEEP(r_new_list(3));
  r_list_poke(df, 0, compare);
  r_list_poke(df, 1, start);
  r_list_poke(df, 2, end);

  r_obj* df_names = r_new_character(3);
  r_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("compare"));
  r_chr_poke(df_names, 1, r_str("start"));
  r_chr_poke(df_names, 2, r_str("end"));

  r_init_data_frame(df, vec_size(start));

  r_obj* direction = KEEP(r_chr("asc"));
  r_obj* na_value = KEEP(r_chr("smallest"));
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

static inline
r_obj* interval_detect_complete(r_obj* start, r_obj* end) {
  // Put them in a data frame to compute joint completeness
  r_obj* df = KEEP(r_new_list(2));
  r_list_poke(df, 0, start);
  r_list_poke(df, 1, end);

  r_obj* df_names = r_new_character(2);
  r_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("start"));
  r_chr_poke(df_names, 1, r_str("end"));

  r_init_data_frame(df, vec_size(start));

  r_obj* out = vec_detect_complete(df);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

void vctrs_init_intervals(r_obj* ns) {
  args_start_ = new_wrapper_arg(NULL, "start");
  args_end_ = new_wrapper_arg(NULL, "end");
}
