flatten_vec_impl <- function(x, ptype = list()) {
  ns <- vec_init(integer(), vec_size(x))

  for (i in vec_seq_along(x)) {
    elt <- vec_get(x, i)
    ns[[i]] <- vec_size(elt)
  }

  n <- sum(ns)

  out <- vec_init(ptype, n)
  loc <- 1L

  for (i in vec_seq_along(x)) {
    elt <- vec_get(x, i)

    # because vec_assign(list(), integer(), NULL) errors?
    if (is.null(elt)) {
      next
    }

    # don't want coercible cast, want real cast
    elt <- vec_cast(elt, ptype)
    elt_n <- ns[[i]]
    step <- elt_n - 1L
    out <- vec_assign(out, seq2(loc, loc + step), elt)
    loc <- loc + elt_n
  }

  out
}


flatten_impl <- function(x) {
  ns <- vec_init(integer(), vec_size(x))

  for (i in vec_seq_along(x)) {
    elt <- vec_get(x, i)

    if (vec_is_recursive(elt)) {
      ns[[i]] <- vec_size(elt)
    } else {
      ns[[i]] <- 1L
    }
  }

  n <- sum(ns)

  out <- vec_init(list(), n)
  loc <- 1L

  for (i in vec_seq_along(x)) {
    elt <- vec_get(x, i)

    if (vec_is_recursive(elt)) {
      for (j in vec_seq_along(elt)) {
        out <- vec_set(out, loc, vec_get(elt, j))
        loc <- loc + 1L
      }
      next
    }

    out <- vec_set(out, loc, elt)
    loc <- loc + 1L
  }

  out
}

vctrs_flatten_vec <- function(x, ptype = list()) {
  flatten_vec_impl(x, ptype = ptype)
}


vctrs_flatten <- function(x) {
  flatten_impl(x)
}
