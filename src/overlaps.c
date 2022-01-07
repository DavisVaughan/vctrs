#include <rlang.h>
#include "vctrs.h"
#include "order.h"
#include "compare.h"

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

static inline
r_obj* interval_order2(r_obj* compare, r_obj* start, r_obj* end) {
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

  r_init_data_frame(df, r_length(start));

  // Could be a callback to R here instead if this lived in another package
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

// -----------------------------------------------------------------------------

static
r_obj* interval_minimize(r_obj* x, int gap) {
  r_obj* x_start = r_list_get(x, 0);
  r_obj* x_end = r_list_get(x, 1);

  const r_ssize size = r_length(x_start);

  if (gap < 0) {
    r_abort("`gap` must be >=0.");
  }

  const int* v_start = r_int_cbegin(x_start);
  const int* v_end = r_int_cbegin(x_end);

  r_obj* order = KEEP(interval_order(x_start, x_end));
  const int* v_order = r_int_cbegin(order);

  // Assume the data can be collapsed in half to start with.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_starts->shelter);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_ends->shelter);

  r_ssize i = 0;
  int set_start = r_globals.na_int;
  int set_end = r_globals.na_int;

  // Find first non-NA interval
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    if (elt_start != r_globals.na_int) {
      set_start = elt_start;
      set_end = elt_end;
      ++i;
      break;
    }
  }

  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    if (elt_start == r_globals.na_int) {
      // NA intervals are always at the end
      break;
    }

    if (set_end < elt_start - gap) {
      r_int_push_back(p_starts, set_start);
      r_int_push_back(p_ends, set_end);

      set_start = elt_start;
      set_end = elt_end;
    } else if (set_end < elt_end) {
      set_end = elt_end;
    }
  }

  if (set_start != r_globals.na_int) {
    r_int_push_back(p_starts, set_start);
    r_int_push_back(p_ends, set_end);
  }

  r_obj* out = KEEP(r_new_list(2));
  r_list_poke(out, 0, r_arr_unwrap(p_starts));
  r_list_poke(out, 1, r_arr_unwrap(p_ends));

  r_obj* out_names = r_new_character(2);
  r_poke_names(out, out_names);
  r_chr_poke(out_names, 0, r_str("start"));
  r_chr_poke(out_names, 1, r_str("end"));

  FREE(4);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_minimize(r_obj* x, r_obj* gap) {
  const int c_gap = r_as_int(gap);
  return interval_minimize(x, c_gap);
}

// -----------------------------------------------------------------------------

static
r_obj* interval_locate_minimal(r_obj* x, bool keep_empty, bool keep_missing, bool groups) {
  r_obj* start = r_list_get(x, 0);
  const int* v_start = r_int_cbegin(start);

  r_obj* end = r_list_get(x, 1);
  const int* v_end = r_int_cbegin(end);

  const r_ssize size = vec_size(start);

  /*
   * NA == (either incomplete), NA interval
   * -1 == (start >  end), not allowed
   *  0 == (start == end), empty interval
   *  1 == (start <  end), typical case
   */
  r_obj* compare = KEEP(vec_compare(end, start, false));
  int* v_compare = r_int_begin(compare);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_compare[i] == -1) {
      r_abort("`start` must be less than or equal to `end`.");
    }
  }

  if (keep_empty) {
    // With `keep_empty`, we only care about using `equal` to order missing
    // values at the end. Empty intervals shouldn't be grouped separately.
    for (r_ssize i = 0; i < size; ++i) {
      if (v_compare[i] == 0) {
        v_compare[i] = 1;
      }
    }
  }

  r_obj* order = KEEP(interval_order2(compare, start, end));
  const int* v_order = r_int_cbegin(order);

  // Assume the data can be collapsed in half to start with.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_starts->shelter);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_ends->shelter);

  struct r_dyn_array* p_loc = NULL;
  r_obj* loc_shelter = r_null;
  if (groups) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    loc_shelter = p_loc->shelter;
  }
  KEEP(loc_shelter);

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

  int set_start = r_globals.na_int;
  int set_end = r_globals.na_int;
  r_ssize loc_set_start = r_globals.na_int;
  r_ssize loc_set_end = r_globals.na_int;

  // Set information about first usable interval
  if (i < size) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    set_start = elt_start;
    set_end = elt_end;
    loc_set_start = loc;
    loc_set_end = loc;
    ++i;
  }

  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    if (set_end < elt_start) {
      r_int_push_back(p_starts, loc_set_start + 1);
      r_int_push_back(p_ends, loc_set_end + 1);

      if (groups) {
        const r_ssize loc_size = loc_order_end - loc_order_start + 1;

        r_obj* loc = r_new_integer(loc_size);
        r_list_push_back(p_loc, loc);
        int* v_loc = r_int_begin(loc);

        const int* v_order_start = v_order + loc_order_start;
        memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
      }

      loc_order_start = loc_order_end + 1;

      set_start = elt_start;
      set_end = elt_end;
      loc_set_start = loc;
      loc_set_end = loc;
    } else if (set_end < elt_end) {
      set_end = elt_end;
      loc_set_end = loc;
    }

    loc_order_end = i;
  }

  if (set_start != r_globals.na_int) {
    r_int_push_back(p_starts, loc_set_start + 1);
    r_int_push_back(p_ends, loc_set_end + 1);

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
    r_int_push_back(p_starts, r_globals.na_int);
    r_int_push_back(p_ends, r_globals.na_int);

    if (groups) {
      const r_ssize loc_size = loc_order_missing_end - loc_order_missing_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_missing_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  r_obj* key = KEEP(r_new_list(2));
  r_list_poke(key, 0, r_arr_unwrap(p_starts));
  r_list_poke(key, 1, r_arr_unwrap(p_ends));

  r_obj* key_names = r_new_character(2);
  r_poke_names(key, key_names);
  r_chr_poke(key_names, 0, r_str("start"));
  r_chr_poke(key_names, 1, r_str("end"));

  r_init_data_frame(key, p_starts->count);

  r_obj* out = r_null;

  if (groups) {
    out = KEEP(r_new_list(2));
    r_list_poke(out, 0, key);
    r_list_poke(out, 1, r_arr_unwrap(p_loc));

    r_obj* out_names = r_new_character(2);
    r_poke_names(out, out_names);
    r_chr_poke(out_names, 0, r_str("key"));
    r_chr_poke(out_names, 1, r_str("loc"));

    r_init_data_frame(out, p_starts->count);

    FREE(1);
  } else {
    out = key;
  }
  KEEP(out);

  FREE(7);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_locate_minimal(r_obj* x, r_obj* keep_empty, r_obj* keep_missing, r_obj* groups) {
  const bool c_keep_empty = r_as_bool(keep_empty);
  const bool c_keep_missing = r_as_bool(keep_missing);
  const bool c_groups = r_as_bool(groups);
  return interval_locate_minimal(x, c_keep_empty, c_keep_missing, c_groups);
}

// -----------------------------------------------------------------------------

static
r_obj* interval_complement(r_obj* x, int start, int end) {
  // Minimize to sort, remove all missings, remove all empty intervals,
  // and merge all abutting intervals
  r_obj* key = KEEP(interval_locate_minimal(x, false, false, false));
  const int* v_loc_start = r_int_cbegin(r_list_get(key, 0));
  const int* v_loc_end = r_int_cbegin(r_list_get(key, 1));

  r_ssize size = vec_size(key);

  r_obj* x_start = r_list_get(x, 0);
  r_obj* x_end = r_list_get(x, 1);
  const int* v_start = r_int_cbegin(x_start);
  const int* v_end = r_int_cbegin(x_end);

  bool use_forced_start = (start != r_globals.na_int);
  bool use_forced_end = (end != r_globals.na_int);

  if (use_forced_start && use_forced_end && start > end) {
    // Handle the one special case of `start > end` up front.
    // This is an invalid interval, but we try and be a little flexible here.
    r_obj* out = KEEP(r_new_list(2));
    r_list_poke(out, 0, vctrs_shared_empty_int);
    r_list_poke(out, 1, vctrs_shared_empty_int);

    r_obj* out_names = r_new_character(2);
    r_poke_names(out, out_names);
    r_chr_poke(out_names, 0, r_str("start"));
    r_chr_poke(out_names, 1, r_str("end"));

    FREE(2);
    return out;
  }

  // Assume the complement will take roughly half current size.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_starts->shelter);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_ends->shelter);

  r_ssize i = 0;

  r_ssize loc_lower_after_start_of = -1;
  r_ssize loc_lower_before_end_of = 0;

  if (use_forced_start) {
    // Shift `i` forward to the first interval completely past `start`.
    // Track information about where `start` is in relation to the intervals.
    for (; i < size; ++i) {
      const int elt_start = v_start[v_loc_start[i] - 1];
      const int elt_end = v_end[v_loc_end[i] - 1];

      if (start > elt_end) {
        ++loc_lower_before_end_of;
        ++loc_lower_after_start_of;
      } else if (start >= elt_start) {
        ++loc_lower_after_start_of;
      } else {
        break;
      }
    }
  }

  r_ssize loc_upper_after_start_of = size - 1;
  r_ssize loc_upper_before_end_of = size;

  if (use_forced_end) {
    // Shift `size` backwards to the first interval that is completely before `end`.
    // Track information about where `end` is in relation to the intervals.
    for (; size - 1 >= 0; --size) {
      const int elt_start = v_start[v_loc_start[size - 1] - 1];
      const int elt_end = v_end[v_loc_end[size - 1] - 1];

      if (end < elt_start) {
        --loc_upper_before_end_of;
        --loc_upper_after_start_of;
      } else if (end <= elt_end) {
        --loc_upper_before_end_of;
      } else {
        break;
      }
    }
  }

  const bool has_intervals_between = i < size;

  if (use_forced_start && has_intervals_between) {
    // If `start` lands in the middle of an interval, then we use the end
    // of that interval, otherwise we use the `start` value.
    const int gap_start =
      (loc_lower_before_end_of == loc_lower_after_start_of) ?
      v_end[v_loc_end[loc_lower_before_end_of] - 1] :
      start;

    // End of the gap is the next interval start. No need to worry about
    // `end` here since `has_intervals_between` told us there is an interval
    // between `start` and `end`.
    const int gap_end = v_start[v_loc_start[loc_lower_after_start_of + 1] - 1];

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }

  int set_start = r_globals.na_int;
  int set_end = r_globals.na_int;

  if (i < size) {
    // Set information about first usable interval
    const int elt_start = v_start[v_loc_start[i] - 1];
    const int elt_end = v_end[v_loc_end[i] - 1];

    set_start = elt_start;
    set_end = elt_end;
    ++i;
  }

  for (; i < size; ++i) {
    const int elt_start = v_start[v_loc_start[i] - 1];
    const int elt_end = v_end[v_loc_end[i] - 1];

    if (set_end < elt_start) {
      const int gap_start = set_end;
      const int gap_end = elt_start;

      r_int_push_back(p_starts, gap_start);
      r_int_push_back(p_ends, gap_end);

      set_start = elt_start;
      set_end = elt_end;
    } else if (set_end < elt_end) {
      set_end = elt_end;
    }
  }

  if (use_forced_end && has_intervals_between) {
    // Start of the gap is the previous interval end. No need to worry about
    // `start` here since `has_intervals_between` told us there is an interval
    // between `start` and `end`.
    const int gap_start = v_end[v_loc_end[loc_upper_before_end_of - 1] - 1];

    // If `end` lands in the middle of an interval, then we use the start
    // of that interval, otherwise we use the `end` value.
    const int gap_end =
      (loc_upper_before_end_of == loc_upper_after_start_of) ?
      v_start[v_loc_start[loc_upper_before_end_of] - 1] :
      end;

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }

  if (use_forced_start && use_forced_end && !has_intervals_between) {
    // Handle the case where `start` and `end` have no full intervals between
    // them. However, `start` and `end` may still fall inside an interval, so
    // we have to be careful about the bounds to use. If `start` and `end` are
    // in the same interval, we are careful to not log anything.
    const int gap_start =
      (loc_lower_before_end_of == loc_lower_after_start_of) ?
      v_end[v_loc_end[loc_lower_before_end_of] - 1] :
      start;

    const int gap_end =
      (loc_upper_before_end_of == loc_upper_after_start_of) ?
      v_start[v_loc_start[loc_upper_before_end_of] - 1] :
      end;

    if (gap_start < gap_end) {
      r_int_push_back(p_starts, gap_start);
      r_int_push_back(p_ends, gap_end);
    }
  }

  r_obj* out = KEEP(r_new_list(2));
  r_list_poke(out, 0, r_arr_unwrap(p_starts));
  r_list_poke(out, 1, r_arr_unwrap(p_ends));

  r_obj* out_names = r_new_character(2);
  r_poke_names(out, out_names);
  r_chr_poke(out_names, 0, r_str("start"));
  r_chr_poke(out_names, 1, r_str("end"));

  FREE(4);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_complement(r_obj* x, r_obj* start, r_obj* end) {
  const int c_start = (start == r_null) ? r_globals.na_int : r_as_int(start);
  const int c_end = (end == r_null) ? r_globals.na_int : r_as_int(end);
  return interval_complement(x, c_start, c_end);
}
