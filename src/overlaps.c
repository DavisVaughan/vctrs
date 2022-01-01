#include <rlang.h>
#include "vctrs.h"
#include "order.h"

static
r_obj* interval_link(r_obj* start, r_obj* end, bool locations) {
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

  r_ssize loc_order_start = 0;
  struct r_dyn_array* p_loc = NULL;
  r_obj* loc_shelter = r_null;
  if (locations) {
    p_loc = r_new_dyn_vector(R_TYPE_list, initial_size);
    loc_shelter = p_loc->shelter;
  }
  KEEP(loc_shelter);

  if (size > 0) {
    int set_start = v_start[v_order[0] - 1];
    int set_end = v_end[v_order[0] - 1];

    for (r_ssize i = 1; i < size; ++i) {
      const r_ssize loc = v_order[i] - 1;

      const int elt_start = v_start[loc];
      const int elt_end = v_end[loc];

      if (set_end <= elt_start) {
        r_int_push_back(p_starts, set_start);
        r_int_push_back(p_ends, set_end);

        if (locations) {
          const r_ssize loc_order_stop = i - 1;
          const r_ssize loc_size = loc_order_stop - loc_order_start + 1;

          r_obj* loc = r_new_integer(loc_size);
          r_list_push_back(p_loc, loc);
          int* v_loc = r_int_begin(loc);

          const int* v_order_start = v_order + loc_order_start;
          memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));

          loc_order_start = loc_order_stop + 1;
        }

        set_start = elt_start;
        set_end = elt_end;
      } else if (set_end < elt_end) {
        set_end = elt_end;
      }
    }

    r_int_push_back(p_starts, set_start);
    r_int_push_back(p_ends, set_end);

    if (locations) {
      const r_ssize loc_order_stop = size - 1;
      const r_ssize loc_size = loc_order_stop - loc_order_start + 1;

      r_obj* loc = r_new_integer(loc_size);
      r_list_push_back(p_loc, loc);
      int* v_loc = r_int_begin(loc);

      const int* v_order_start = v_order + loc_order_start;
      memcpy(v_loc, v_order_start, loc_size * sizeof(*v_loc));
    }
  }

  const r_ssize out_n = locations ? 3 : 2;

  r_obj* out = KEEP(r_new_list(out_n));
  r_list_poke(out, 0, r_arr_unwrap(p_starts));
  r_list_poke(out, 1, r_arr_unwrap(p_ends));

  r_obj* out_names = r_new_character(out_n);
  r_poke_names(out, out_names);
  r_chr_poke(out_names, 0, r_str("start"));
  r_chr_poke(out_names, 1, r_str("end"));

  if (locations) {
    r_list_poke(out, 2, r_arr_unwrap(p_loc));
    r_chr_poke(out_names, 2, r_str("loc"));
  }

  r_init_data_frame(out, p_starts->count);

  FREE(8);
  return out;
}

// [[ register() ]]
r_obj* vctrs_interval_link(r_obj* start, r_obj* end, r_obj* locations) {
  const bool c_locations = r_as_bool(locations);
  return interval_link(start, end, c_locations);
}

// -----------------------------------------------------------------------------

static
r_obj* interval_complement(r_obj* start, r_obj* end, int force_start, int force_end) {
  const r_ssize size = r_length(start);

  bool use_force_start = (force_start != r_globals.na_int);
  bool use_force_end = (force_end != r_globals.na_int);

  if (r_typeof(start) != R_TYPE_integer) {
    r_abort("`start` must be an integer.");
  }
  if (r_typeof(end) != R_TYPE_integer) {
    r_abort("`end` must be an integer.");
  }
  if (size != r_length(end)) {
    r_abort("`start` must be the same length as `end`.");
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

  struct r_dyn_array* p_starts = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_starts->shelter);

  struct r_dyn_array* p_ends = r_new_dyn_vector(R_TYPE_integer, initial_size);
  KEEP(p_ends->shelter);

  if (size == 0 && use_force_start && use_force_end) {
    const int gap_start = force_start;
    const int gap_end = force_end;

    if (gap_start < gap_end) {
      r_int_push_back(p_starts, gap_start);
      r_int_push_back(p_ends, gap_end);
    }
  }

  if (size > 0) {
    int set_start = v_start[v_order[0] - 1];
    int set_end = v_end[v_order[0] - 1];

    if (use_force_start && force_start < set_start) {
      use_force_start = false;

      const int gap_start = force_start;

      int gap_end = set_start;
      if (use_force_end && force_end < gap_end) {
        gap_end = force_end;
      }

      if (gap_start < gap_end) {
        r_int_push_back(p_starts, gap_start);
        r_int_push_back(p_ends, gap_end);
      }
    }

    for (r_ssize i = 1; i < size; ++i) {
      const r_ssize loc = v_order[i] - 1;

      const int elt_start = v_start[loc];
      const int elt_end = v_end[loc];

      const bool has_gap =
        !(use_force_end && set_end >= force_end) &&
        !(use_force_start && set_end < force_start) &&
        (set_end < elt_start);

      if (has_gap) {
        const int gap_start = set_end;
        const int gap_end = elt_start;

        r_int_push_back(p_starts, gap_start);
        r_int_push_back(p_ends, gap_end);

        set_end = elt_end;
      } else if (set_end < elt_end) {
        set_end = elt_end;
      }
    }

    if (use_force_end && force_end > set_end) {
      use_force_end = false;

      int gap_start = set_end;
      if (use_force_start && force_start > gap_start) {
        gap_start = force_start;
      }

      const int gap_end = force_end;

      if (gap_start < gap_end) {
        r_int_push_back(p_starts, gap_start);
        r_int_push_back(p_ends, gap_end);
      }
    }
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
