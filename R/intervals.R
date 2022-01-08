vec_interval_locate_minimal <- function(start,
                                        end,
                                        ...,
                                        keep_empty = FALSE,
                                        keep_missing = FALSE) {
  check_dots_empty0(...)
  groups <- FALSE
  .Call(vctrs_interval_locate_minimal, start, end, keep_empty, keep_missing, groups)
}

vec_interval_locate_minimal_groups <- function(start,
                                               end,
                                               ...,
                                               keep_empty = FALSE,
                                               keep_missing = FALSE) {
  check_dots_empty0(...)
  groups <- TRUE
  .Call(vctrs_interval_locate_minimal, start, end, keep_empty, keep_missing, groups)
}

vec_interval_complement <- function(start,
                                    end,
                                    ...,
                                    lower = NULL,
                                    upper = NULL) {
  check_dots_empty0(...)
  .Call(vctrs_interval_complement, start, end, lower, upper)
}

# ------------------------------------------------------------------------------

interval <- function(start, end) {
  args <- list(start = start, end = end)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  start <- args$start
  end <- args$end

  missing_start <- vec_equal_na(start)
  missing_end <- vec_equal_na(end)

  if (any(missing_start)) {
    end <- vec_assign(end, missing_start, NA)
  }
  if (any(missing_end)) {
    start <- vec_assign(start, missing_end, NA)
  }

  if (any(vec_compare(start, end) == 1L, na.rm = TRUE)) {
    abort("`start` must be less than or equal to `end`.")
  }

  new_interval(start, end)
}

new_interval <- function(start, end, ..., class = character()) {
  fields <- list(start = start, end = end)
  new_rcrd(fields, ..., class = c(class, "vctrs_interval"))
}

interval_start <- function(x) {
  x <- interval_proxy(x)
  field_start(x)
}
field_start <- function(x) {
  field(x, "start")
}

interval_end <- function(x) {
  x <- interval_proxy(x)
  field_end(x)
}
field_end <- function(x) {
  field(x, "end")
}

#' @export
format.vctrs_interval <- function(x, ...) {
  start <- interval_start(x)
  end <- interval_end(x)

  start <- as.character(start)
  end <- as.character(end)

  out <- as.character(glue::glue("[{start}, {end})"))

  out
}

#' @export
vec_ptype_full.vctrs_interval <- function(x, ...) {
  inner <- vec_ptype_full(interval_start(x))
  paste0("interval<", inner, ">")
}

#' @export
vec_ptype2.vctrs_interval.vctrs_interval <- function(x, y, ...) {
  ptype <- vec_ptype2(field_start(x), field_start(y), ...)
  new_interval(ptype, ptype)
}

#' @export
vec_cast.vctrs_interval.vctrs_interval <- function(x, to, ...) {
  to <- field_start(to)
  start <- vec_cast(field_start(x), to, ...)
  end <- vec_cast(field_end(x), to, ...)
  new_interval(start, end)
}

interval_proxy <- function(x) {
  UseMethod("interval_proxy")
}

#' @export
interval_proxy.vctrs_interval <- function(x) {
  x
}

interval_restore <- function(x, to) {
  UseMethod("interval_restore", to)
}

#' @export
interval_restore.vctrs_interval <- function(x, to) {
  x
}

interval_locate_minimal <- function(x, ..., keep_empty = FALSE, keep_missing = FALSE) {
  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  vec_interval_locate_minimal(
    start = start,
    end = end,
    ...,
    keep_empty = keep_empty,
    keep_missing = keep_missing
  )
}

interval_locate_minimal_groups <- function(x, ..., keep_empty = FALSE, keep_missing = FALSE) {
  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  vec_interval_locate_minimal_groups(
    start = start,
    end = end,
    ...,
    keep_empty = keep_empty,
    keep_missing = keep_missing
  )
}

interval_complement <- function(x, ..., lower = NULL, upper = NULL) {
  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  out <- vec_interval_complement(
    start = start,
    end = end,
    ...,
    lower = lower,
    upper = upper
  )

  out <- new_interval(out$start, out$end)
  out <- interval_restore(out, x)

  out
}

interval_minimize <- function(x, ..., keep_empty = FALSE, keep_missing = FALSE) {
  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  loc <- vec_interval_locate_minimal(
    start = start,
    end = end,
    ...,
    keep_empty = keep_empty,
    keep_missing = keep_missing
  )

  start <- vec_slice(start, loc$start)
  end <- vec_slice(end, loc$end)

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

interval_update_minimal <- function(x) {
  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  groups <- vec_interval_locate_minimal_groups(
    start = start,
    end = end,
    keep_empty = TRUE,
    keep_missing = TRUE
  )

  start <- vec_slice(start, groups$key$start)
  end <- vec_slice(end, groups$key$end)

  out <- new_interval(start, end)

  out <- vec_rep_each(out, times = list_sizes(groups$loc))
  out <- vec_slice(out, vec_unchop(groups$loc, ptype = integer(), name_spec = zap()))

  out <- interval_restore(out, x)

  out
}

interval_set_union <- function(x, y) {
  out <- vec_c(x, y)
  interval_minimize(out)
}

interval_set_difference <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  if (vec_size(x) == 0L || all(vec_equal_na(x))) {
    return(interval_minimize(x))
  }
  if (vec_size(y) == 0L || all(vec_equal_na(y))) {
    return(interval_minimize(x))
  }

  x_proxy <- interval_proxy(x)
  y_proxy <- interval_proxy(y)

  lower <- min(
    min(field_start(x_proxy), na.rm = TRUE),
    min(field_start(y_proxy), na.rm = TRUE)
  )
  upper <- max(
    max(field_end(x_proxy), na.rm = TRUE),
    max(field_end(y_proxy), na.rm = TRUE)
  )

  x_c <- interval_complement(x_proxy, lower = lower, upper = upper)

  u <- interval_set_union(x_c, y_proxy)

  out <- interval_complement(u, lower = lower, upper = upper)

  out <- interval_restore(out, x)

  out
}

interval_set_intersect <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  if (vec_size(x) == 0L || all(vec_equal_na(x))) {
    return(vec_ptype(x))
  }
  if (vec_size(y) == 0L || all(vec_equal_na(y))) {
    return(vec_ptype(x))
  }

  x_proxy <- interval_proxy(x)
  y_proxy <- interval_proxy(y)

  lower <- min(
    min(field_start(x_proxy), na.rm = TRUE),
    min(field_start(y_proxy), na.rm = TRUE)
  )
  upper <- max(
    max(field_end(x_proxy), na.rm = TRUE),
    max(field_end(y_proxy), na.rm = TRUE)
  )

  x_c <- interval_complement(x, lower = lower, upper = upper)
  y_c <- interval_complement(y, lower = lower, upper = upper)

  u <- interval_set_union(x_c, y_c)

  out <- interval_complement(u, lower = lower, upper = upper)

  out <- interval_restore(out, x)

  out
}

interval_parallel_union <- function(x, y, ..., fill = FALSE) {
  if (!is_bool(fill)) {
    abort("`fill` must be a single `TRUE` or `FALSE`.")
  }

  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- interval_proxy(x)
  y_proxy <- interval_proxy(y)

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  if (!fill) {
    max_start <- vec_parallel_max(x_start, y_start)
    min_end <- vec_parallel_min(x_end, y_end)
    has_gap <- vec_compare(max_start, min_end) == 1L

    if (any(has_gap, na.rm = TRUE)) {
      loc <- which(has_gap)[[1]]

      abort(c(
        "Can't take the union of intervals containing a gap.",
        i = glue::glue("Location {loc} contains a gap."),
        i = "Set `fill = TRUE` to force a union anyways."
      ))
    }
  }

  start <- vec_parallel_min(x_start, y_start)
  end <- vec_parallel_max(x_end, y_end)

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

vec_parallel_min <- function(x, y) {
  vec_parallel_summary(x, y, type = "min")
}
vec_parallel_max <- function(x, y) {
  vec_parallel_summary(x, y, type = "max")
}
vec_parallel_summary <- function(x, y, type) {
  args <- list(x = x, y = y)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  cmp <- vec_compare(x, y)

  if (type == "min") {
    x_wins <- cmp <= 0L
    y_wins <- !x_wins
  } else if (type == "max") {
    x_wins <- cmp >= 0L
    y_wins <- !x_wins
  } else {
    abort("Unknown `type`.")
  }

  # Assign mins and maxes, propagate missings through `vec_init()`
  out <- vec_init(x, vec_size(x))
  out <- vec_assign(out, x_wins, vec_slice(x, x_wins))
  out <- vec_assign(out, y_wins, vec_slice(y, y_wins))

  out
}

# ------------------------------------------------------------------------------

integer_interval <- function(start = integer(), end = integer()) {
  args <- interval(start, end)

  start <- interval_start(args)
  end <- interval_end(args)

  args <- vec_cast_common(start = start, end = end, .to = integer())

  start <- args$start
  end <- args$end

  new_integer_interval(start, end)
}

new_integer_interval <- function(start, end, ..., class = character()) {
  if (!is_bare_integer(start)) {
    abort("`start` must be an integer.")
  }
  if (!is_bare_integer(end)) {
    abort("`end` must be an integer.")
  }

  new_interval(start, end, ..., class = c(class, "integer_interval"))
}

#' @export
vec_ptype_full.integer_interval <- function(x, ...) {
  "integer_interval"
}

#' @export
vec_ptype2.integer_interval.integer_interval <- function(x, y, ...) {
  new_integer_interval(start = integer(), end = integer())
}

#' @export
vec_cast.integer_interval.integer_interval <- function(x, to, ...) {
  x
}

#' @export
interval_proxy.integer_interval <- function(x) {
  x
}

#' @export
interval_restore.integer_interval <- function(x, to) {
  new_integer_interval(field_start(x), field_end(x))
}
