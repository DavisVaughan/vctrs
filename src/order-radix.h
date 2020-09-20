/*
 * The implementation of vec_order() is based on data.table’s forder() and their
 * earlier contribution to R’s order(). See LICENSE.note for more information.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2020, RStudio
 * Copyright (c) 2020, Data table team
 */

#ifndef VCTRS_ORDER_RADIX_H
#define VCTRS_ORDER_RADIX_H

#include "vctrs.h"
#include "lazy.h"
#include "order-groups.h"
#include "order-truelength.h"

// -----------------------------------------------------------------------------

/*
 * `order` is an integer vector intended to hold the ordering vector
 * in `vec_order()`. It is allocated eagerly, but the initialization of its
 * values is done lazily. Typically, it is initialized to a 1-based sequential
 * ordering which is rearranged by the internal algorithm. However, for the
 * counting order, the initialization is not required for the first integer
 * column, which can result in a nice performance improvement.
 */
struct order {
  SEXP self;
  SEXP data;
  int* p_data;
  r_ssize size;
  bool initialized;
};

#define PROTECT_ORDER(p_order, p_n) do { \
  PROTECT((p_order)->self);              \
  PROTECT((p_order)->data);              \
  *(p_n) += 2;                           \
} while (0)

static inline
struct order* new_order(r_ssize size) {
  SEXP self = PROTECT(r_new_raw(sizeof(struct order)));
  struct order* p_order = (struct order*) RAW(self);

  SEXP data = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_data = INTEGER(data);

  p_order->self = self;
  p_order->data = data;
  p_order->p_data = p_data;
  p_order->size = size;
  p_order->initialized = false;

  UNPROTECT(2);
  return p_order;
}

static inline
int* init_order(struct order* p_order) {
  if (p_order->initialized) {
    return p_order->p_data;
  }

  // Initialize `x` with sequential 1-based ordering
  for (r_ssize i = 0; i < p_order->size; ++i) {
    p_order->p_data[i] = i + 1;
  }

  p_order->initialized = true;

  return p_order->p_data;
}

// -----------------------------------------------------------------------------

SEXP parse_na_value(SEXP na_value);
SEXP parse_direction(SEXP direction);
SEXP vec_order_expand_args(SEXP x, SEXP decreasing, SEXP na_last);

// -----------------------------------------------------------------------------

/*
 * `order_info` is a meta struct that holds all of the information used to
 * call `vec_order()`. It is returned from `vec_order_info()` and can be
 * used:
 * - For just the ordering in `p_order`
 * - For the additional group sizes in `p_group_infos`
 * - To compute the ordering by appearance by calling `vec_order_opts()`
 *   a second time with `p_order->data` as the input requiring ordering
 */
struct order_info {
  SEXP self;
  struct order* p_order;
  struct lazy_raw* p_lazy_x_chunk;
  struct lazy_raw* p_lazy_x_aux;
  struct lazy_raw* p_lazy_o_aux;
  struct lazy_raw* p_lazy_bytes;
  struct lazy_raw* p_lazy_counts;
  struct group_infos* p_group_infos;
  struct lazy_chr* p_lazy_x_reencoded;
  struct truelength_info* p_truelength_info;
};

#define PROTECT_ORDER_INFO(p_order_info, p_n) do {                    \
  PROTECT((p_order_info)->self);                                      \
  *(p_n) += 1;                                                        \
  PROTECT_ORDER((p_order_info)->p_order, (p_n));                      \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_x_chunk, (p_n));            \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_x_aux, (p_n));              \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_o_aux, (p_n));              \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_bytes, (p_n));              \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_counts, (p_n));             \
  PROTECT_GROUP_INFOS((p_order_info)->p_group_infos, (p_n));          \
  PROTECT_LAZY_VEC((p_order_info)->p_lazy_x_reencoded, (p_n));        \
  PROTECT_TRUELENGTH_INFO((p_order_info)->p_truelength_info, (p_n));  \
} while (0)

struct order_info* new_order_info(SEXP proxy,
                                  r_ssize size,
                                  const enum vctrs_type type,
                                  bool force_tracking);

void vec_order_info(SEXP proxy,
                    SEXP decreasing,
                    SEXP na_last,
                    r_ssize size,
                    const enum vctrs_type type,
                    bool sort_chr,
                    struct order_info* p_info);

// -----------------------------------------------------------------------------
#endif
