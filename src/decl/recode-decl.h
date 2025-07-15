static
r_obj* vec_recode_values_with_multiple_to(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_obj* default_,
  r_obj* ptype,
  r_ssize x_size,
  r_ssize from_size,
  r_ssize to_size,
  r_ssize default_size,
  bool has_default,
  bool multiple_from
);

static
r_obj* vec_recode_values_with_single_to(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  r_obj* default_,
  r_obj* ptype,
  r_ssize x_size,
  r_ssize to_size
);

static
r_obj* build_indices(r_obj* x, r_obj* from, r_ssize x_size, r_ssize from_size, bool multiple_from);

static
r_obj* ptype_finalize(
  r_obj* ptype,
  r_obj* to,
  r_obj* default_,
  bool multiple_to,
  struct vctrs_arg* p_to_arg,
  struct vctrs_arg* p_default_arg,
  struct r_lazy call
);
