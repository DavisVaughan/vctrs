#ifndef VCTRS_SHAPE_H
#define VCTRS_SHAPE_H

#include "vctrs-core.h"
#include "cast.h"
#include "dim.h"


SEXP vec_shaped_ptype_impl(
  SEXP ptype,
  SEXP x,
  SEXP y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
);

static inline
SEXP vec_shaped_ptype(
  SEXP ptype,
  SEXP x,
  SEXP y,
  struct vctrs_arg* p_x_arg,
  struct vctrs_arg* p_y_arg
) {
  if (has_dim(x) || has_dim(y)) {
    return vec_shaped_ptype_impl(ptype, x, y, p_x_arg, p_y_arg);
  } else {
    return ptype;
  }
}

r_obj* vec_shape_broadcast(r_obj* out, const struct cast_opts* p_opts);


#endif
