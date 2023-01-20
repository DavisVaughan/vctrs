#include "vctrs.h"

r_obj* ffi_vec_partition(r_obj* x, r_obj* sizes) {
  return vec_partition(x, sizes);
}

r_obj* vec_partition(r_obj* x, r_obj* sizes) {
  sizes = KEEP(vec_cast(
    sizes,
    r_globals.empty_int,
    vec_args.sizes,
    vec_args.empty,
    r_lazy_null
  ));

  const int* v_sizes = r_int_cbegin(sizes);

  const r_ssize n = r_length(sizes);
  const r_ssize x_size = vec_size(x);

  r_obj* indices = KEEP(r_alloc_list(n));

  r_ssize start = 0;
  const bool increasing = true;

  for (r_ssize i = 0; i < n; ++i) {
    const int size = v_sizes[i];

    if (size < 0) {
      if (size == r_globals.na_int) {
        r_abort("`sizes` can't contain `NA`.");
      } else {
        r_abort("`sizes` can't contain negative sizes.");
      }
    }

    r_list_poke(indices, i, compact_seq(start, size, increasing));

    start += size;
  }

  if (x_size != start) {
    r_abort("`sizes` must sum up to the size of `x`.");
  }

  r_obj* out = vec_chop(x, indices);

  FREE(2);
  return out;
}
