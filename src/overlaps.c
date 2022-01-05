#include <rlang.h>
#include "vctrs.h"
#include "order.h"

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

  if (size > 0) {
    int set_start = v_start[v_order[0] - 1];
    int set_end = v_end[v_order[0] - 1];

    for (r_ssize i = 1; i < size; ++i) {
      const r_ssize loc = v_order[i] - 1;

      const int elt_start = v_start[loc];
      const int elt_end = v_end[loc];

      if (set_end < elt_start - gap) {
        r_int_push_back(p_starts, set_start);
        r_int_push_back(p_ends, set_end);

        set_start = elt_start;
        set_end = elt_end;
      } else if (set_end < elt_end) {
        set_end = elt_end;
      }
    }

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
r_obj* interval_locate_minimal(r_obj* x, int gap, bool groups) {
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

  r_ssize loc_order_start = 0;
  struct r_dyn_array* p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
  KEEP(p_loc->shelter);

  if (size > 0) {
    int set_start = v_start[v_order[0] - 1];
    int set_end = v_end[v_order[0] - 1];

    for (r_ssize i = 1; i < size; ++i) {
      const r_ssize loc = v_order[i] - 1;

      const int elt_start = v_start[loc];
      const int elt_end = v_end[loc];

      if (set_end < elt_start - gap) {
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

        set_start = elt_start;
        set_end = elt_end;
      } else if (set_end < elt_end) {
        set_end = elt_end;
      }
    }

    const r_ssize loc_order_end = size - 1;
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

  FREE(6);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_locate_minimal(r_obj* x, r_obj* gap, r_obj* groups) {
  const int c_gap = r_as_int(gap);
  const bool c_groups = r_as_bool(groups);
  return interval_locate_minimal(x, c_gap, c_groups);
}

// -----------------------------------------------------------------------------

static
r_obj* interval_complement(r_obj* x, int start, int end) {
  r_obj* x_start = r_list_get(x, 0);
  r_obj* x_end = r_list_get(x, 1);

  const r_ssize size = r_length(x_start);

  bool use_forced_start = (start != r_globals.na_int);
  bool use_forced_end = (end != r_globals.na_int);

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

  if (size == 0 && use_forced_start && use_forced_end && start < end) {
    use_forced_start = false;
    use_forced_end = false;

    const int gap_start = start;
    const int gap_end = end;

    r_int_push_back(p_starts, gap_start);
    r_int_push_back(p_ends, gap_end);
  }

  if (size > 0) {
    int set_start = v_start[v_order[0] - 1];
    int set_end = v_end[v_order[0] - 1];

    if (use_forced_start && !use_forced_end && start < set_start) {
      use_forced_start = false;

      const int gap_start = start;
      const int gap_end = set_start;

      r_int_push_back(p_starts, gap_start);
      r_int_push_back(p_ends, gap_end);
    }
    if (use_forced_start && use_forced_end && start < set_start && start < end) {
      use_forced_start = false;

      const int gap_start = start;
      const int gap_end = (set_start < end) ? set_start : end;

      r_int_push_back(p_starts, gap_start);
      r_int_push_back(p_ends, gap_end);
    }

    for (r_ssize i = 1; i < size; ++i) {
      const r_ssize loc = v_order[i] - 1;

      const int elt_start = v_start[loc];
      const int elt_end = v_end[loc];

      const bool has_gap =
        !(use_forced_end && set_end >= end) &&
        !(use_forced_start && set_end < start) &&
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

    if (use_forced_end && !use_forced_start && end > set_end) {
      use_forced_end = false;

      const int gap_start = set_end;
      const int gap_end = end;

      r_int_push_back(p_starts, gap_start);
      r_int_push_back(p_ends, gap_end);
    }
    if (use_forced_end && use_forced_start && end > set_end && end > start) {
      use_forced_end = false;
      use_forced_start = false;

      const int gap_start = (set_end > start) ? set_end : start;
      const int gap_end = end;

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
