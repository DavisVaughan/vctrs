#include <rlang.h>
#include "vctrs.h"
#include "order.h"

// -----------------------------------------------------------------------------

static
bool interval_any_empty(r_obj* start, r_obj* end) {
  const r_ssize size = r_length(start);

  if (r_typeof(start) != R_TYPE_integer) {
    r_abort("`start` must be an integer.");
  }
  if (r_typeof(end) != R_TYPE_integer) {
    r_abort("`end` must be an integer.");
  }
  if (size != r_length(end)) {
    r_abort("`start` must be the same length as `end`.");
  }

  const int* v_start = r_int_cbegin(start);
  const int* v_end = r_int_cbegin(end);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_end[i] - v_start[i] <= 0) {
      return true;
    }
  }

  return false;
}

static
r_obj* interval_detect_empty(r_obj* start, r_obj* end) {
  const r_ssize size = r_length(start);

  if (r_typeof(start) != R_TYPE_integer) {
    r_abort("`start` must be an integer.");
  }
  if (r_typeof(end) != R_TYPE_integer) {
    r_abort("`end` must be an integer.");
  }
  if (size != r_length(end)) {
    r_abort("`start` must be the same length as `end`.");
  }

  const int* v_start = r_int_cbegin(start);
  const int* v_end = r_int_cbegin(end);

  r_obj* out = KEEP(r_new_logical(size));
  int* v_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    v_out[i] = v_end[i] - v_start[i] <= 0;
  }

  FREE(1);
  return out;
}

static
r_obj* interval_which_non_empty(r_obj* start, r_obj* end) {
  const r_ssize size = r_length(start);

  if (r_typeof(start) != R_TYPE_integer) {
    r_abort("`start` must be an integer.");
  }
  if (r_typeof(end) != R_TYPE_integer) {
    r_abort("`end` must be an integer.");
  }
  if (size != r_length(end)) {
    r_abort("`start` must be the same length as `end`.");
  }

  const int* v_start = r_int_cbegin(start);
  const int* v_end = r_int_cbegin(end);

  r_ssize j = 0;
  r_obj* out = KEEP(r_new_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_end[i] - v_start[i] > 0) {
      v_out[j] = i + 1;
      ++j;
    }
  }

  out = r_int_resize(out, j);

  FREE(1);
  return out;
}

static
r_obj* interval_drop_empty(r_obj* start, r_obj* end) {
  r_obj* out = KEEP(r_new_list(2));

  if (!interval_any_empty(start, end)) {
    r_list_poke(out, 0, start);
    r_list_poke(out, 1, end);
    FREE(1);
    return out;
  }

  const r_ssize size = r_length(start);

  const int* v_start = r_int_cbegin(start);
  const int* v_end = r_int_cbegin(end);

  r_obj* empty = KEEP(interval_detect_empty(start, end));
  const int* v_empty = r_lgl_cbegin(empty);
  r_ssize out_size = size - r_lgl_sum(empty, true);

  r_obj* out_start = r_new_integer(out_size);
  r_list_poke(out, 0, out_start);
  int* v_out_start = r_int_begin(out_start);

  r_obj* out_end = r_new_integer(out_size);
  r_list_poke(out, 1, out_end);
  int* v_out_end = r_int_begin(out_end);

  r_ssize j = 0;

  for (r_ssize i = 0; i < size; ++i) {
    if (!v_empty[i]) {
      v_out_start[j] = v_start[i];
      v_out_end[j] = v_end[i];
      ++j;
    }
  }

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* interval_minimize(r_obj* start, r_obj* end, bool locations, bool groups, int gap) {
  const r_ssize size = r_length(start);

  if (groups && !locations) {
    r_abort("If `groups` is set, then `locations` must also be set.");
  }
  if (gap < 0) {
    r_abort("`gap` must be >=0.");
  }

  // Put them in a data frame to compute joint ordering
  r_obj* df = KEEP(r_new_list(2));
  r_list_poke(df, 0, start);
  r_list_poke(df, 1, end);

  r_obj* df_names = r_new_character(2);
  r_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("start"));
  r_chr_poke(df_names, 1, r_str("end"));

  r_init_data_frame(df, size);

  // Could be a callback to R here instead if this lived in another package
  r_obj* direction = KEEP(r_chr("asc"));
  r_obj* na_value = KEEP(r_chr("largest"));
  bool nan_distinct = false;
  r_obj* chr_proxy_collate = r_null;

  r_obj* order = KEEP(vec_order(
    df,
    direction,
    na_value,
    nan_distinct,
    chr_proxy_collate
  ));
  const int* v_order = r_int_cbegin(order);

  const int* v_start = r_int_cbegin(start);
  const int* v_end = r_int_cbegin(end);

  // Assume the data can be collapsed in half to start with.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  // If `locations = false`, `p_starts` and `p_ends` contain the linked
  // interval values. If `locations = true`, they contain locations telling
  // you how to slice the original input to obtain the linked interval values.
  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_starts->shelter);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_ends->shelter);

  r_ssize loc_order_start = 0;
  struct r_dyn_array* p_loc = NULL;
  r_obj* loc_shelter = r_null;
  if (groups) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    loc_shelter = p_loc->shelter;
  }
  KEEP(loc_shelter);

  r_ssize i = 0;
  int set_start = INT_MAX;
  int set_end = -INT_MAX;

  // Find first non-empty interval
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    if (elt_end > elt_start) {
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

    if (elt_end <= elt_start) {
      // Found first empty interval, which are always at the end
      break;
    }

    if (set_end < elt_start - gap) {
      if (locations) {
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
      } else {
        r_int_push_back(p_starts, set_start);
        r_int_push_back(p_ends, set_end);
      }

      set_start = elt_start;
      set_end = elt_end;
    } else if (set_end < elt_end) {
      set_end = elt_end;
    }
  }

  if (set_end > set_start) {
    if (locations) {
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
    } else {
      r_int_push_back(p_starts, set_start);
      r_int_push_back(p_ends, set_end);
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

  FREE(9);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_minimize(r_obj* start, r_obj* end, r_obj* locations, r_obj* groups, r_obj* gap) {
  const bool c_locations = r_as_bool(locations);
  const bool c_groups = r_as_bool(groups);
  const int c_gap = r_as_int(gap);
  return interval_minimize(start, end, c_locations, c_groups, c_gap);
}

// -----------------------------------------------------------------------------

static
r_obj* interval_complement(r_obj* start, r_obj* end, int force_start, int force_end) {
  const r_ssize size = r_length(start);

  bool use_force_start = (force_start != r_globals.na_int);
  bool use_force_end = (force_end != r_globals.na_int);

  // Put them in a data frame to compute joint ordering
  r_obj* df = KEEP(r_new_list(2));
  r_list_poke(df, 0, start);
  r_list_poke(df, 1, end);

  r_obj* df_names = r_new_character(2);
  r_poke_names(df, df_names);
  r_chr_poke(df_names, 0, r_str("start"));
  r_chr_poke(df_names, 1, r_str("end"));

  r_init_data_frame(df, size);

  // Could be a callback to R here instead if this lived in another package
  r_obj* direction = KEEP(r_chr("asc"));
  r_obj* na_value = KEEP(r_chr("largest"));
  bool nan_distinct = false;
  r_obj* chr_proxy_collate = r_null;

  r_obj* order = KEEP(vec_order(
    df,
    direction,
    na_value,
    nan_distinct,
    chr_proxy_collate
  ));
  const int* v_order = r_int_cbegin(order);

  const int* v_start = r_int_cbegin(start);
  const int* v_end = r_int_cbegin(end);

  // Assume the data can be collapsed in half to start with.
  // Apply a minimum size to avoid a size of zero.
  const r_ssize initial_size = r_ssize_max(size / 2, 1);

  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_starts->shelter);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_ends->shelter);

  r_ssize i = 0;
  int set_start = INT_MAX;
  int set_end = -INT_MAX;

  // Find first non-empty interval
  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    if (elt_end > elt_start) {
      set_start = elt_start;
      set_end = elt_end;
      ++i;
      break;
    }
  }

  if (use_force_start && !use_force_end && force_start < set_start && set_start < set_end) {
    use_force_start = false;

    const int gap_start = force_start;
    const int gap_end = set_start;

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }
  if (use_force_start && use_force_end && force_start < set_start && force_start < force_end) {
    use_force_start = false;

    const int gap_start = force_start;
    const int gap_end = (set_start < force_end) ? set_start : force_end;

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }

  for (; i < size; ++i) {
    const r_ssize loc = v_order[i] - 1;

    const int elt_start = v_start[loc];
    const int elt_end = v_end[loc];

    if (elt_end <= elt_start) {
      // Found first empty interval, which are always at the end
      break;
    }

    const bool has_gap =
      !(use_force_end && set_end >= force_end) &&
      !(use_force_start && set_end < force_start) &&
      (set_end < elt_start);

    if (has_gap) {
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

  if (use_force_end && !use_force_start && force_end > set_end && set_end > set_start) {
    use_force_end = false;

    const int gap_start = set_end;
    const int gap_end = force_end;

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }
  if (use_force_end && use_force_start && force_end > set_end && force_end > force_start) {
    use_force_end = false;
    use_force_start = false;

    const int gap_start = (set_end > force_start) ? set_end : force_start;
    const int gap_end = force_end;

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }

  r_obj* out = KEEP(r_new_list(2));
  r_list_poke(out, 0, r_arr_unwrap(p_starts));
  r_list_poke(out, 1, r_arr_unwrap(p_ends));

  r_obj* out_names = r_new_character(2);
  r_poke_names(out, out_names);
  r_chr_poke(out_names, 0, r_str("start"));
  r_chr_poke(out_names, 1, r_str("end"));

  r_init_data_frame(out, p_starts->count);

  FREE(7);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_complement(r_obj* start, r_obj* end, r_obj* force_start, r_obj* force_end) {
  const int c_force_start = (force_start == r_null) ? r_globals.na_int : r_as_int(force_start);
  const int c_force_end = (force_end == r_null) ? r_globals.na_int : r_as_int(force_end);
  return interval_complement(start, end, c_force_start, c_force_end);
}
