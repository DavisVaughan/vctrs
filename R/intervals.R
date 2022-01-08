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

#' Interval definitions and properties
#'
#' @description
#' Intervals are always half-open. In particular, they are left-closed,
#' right-open, i.e. `[a, b)`. It is required that `a >= b`. When `a == b`,
#' the interval is _empty_.
#'
#' ## Comparison states
#'
#' When comparing two intervals, there are three distinct states that can be
#' used to describe the relationship between them.
#'
#' - _Abutting_ intervals occur when the end of one interval exactly matches
#'   the start of the other interval. For example, `[1, 5)` and `[5, 7)` are
#'   abutting.
#'
#' - _Overlapping_ intervals occur when two intervals overlap by at least one
#'   value. For example, `[1, 5)` overlaps with `[2, 7)`. It also overlaps with
#'   (and completely contains) `[2, 3)`.
#'
#' - _Disjoint_ intervals are those with a _gap_ between them. For example,
#'   `[1, 5)` and `[6, 7)` are disjoint, with a gap of `[5, 6)`. This is also
#'   known as the _complement_ of those two intervals.
#'
#' Note that when there is any ambiguity, the abutting state overrides the
#' overlapping and disjoint states. For example, the empty interval of
#' `[1, 1)` abuts, but does not overlap, `[1, 5)`. However, `[1, 1)` does
#' overlap `[0, 5)`.
#'
#' ## Incomparable intervals
#'
#' Incomparable intervals are those that can't be compared with any other
#' interval. In most cases, this just means the interval contains a missing
#' value, like `[NA, NA)`. If one side of the interval is incomparable, then
#' the other side is required to be incomparable as well. The [interval()]
#' helper enforces this automatically.
#'
#' Incomparable intervals are infectious. For example, when intersecting two
#' sets of intervals in parallel with `interval_parallel_intersect()`, if `x`
#' or `y` contain an incomparable interval, then the result will also be
#' incomparable.
#'
#' Intervals containing data frames are a special case where missing values
#' may be present without automatically forcing the interval to be incomparable.
#' For example, an interval containing
#' `[ data.frame(x = 1, y = NA), data.frame(x = 2, y = 3) )`
#' is not automatically incomparable. Because `1 < 2`, the `x` column determined
#' that the `start` of the interval was less than the `end` of it. However, the
#' following would result in an incomparable interval:
#' `[ data.frame(x = 1, y = NA), data.frame(x = 1, y = 3) )`.
#' Because the `x` results in a tie, we look to `y` to break the tie, but this
#' contains an incomparable value. If we supplied these values to the
#' `interval()` helper, it would detect this and standardize both sides of the
#' interval into their missing value, `data.frame(x = NA, y = NA)`.
#'
#' ## Minimal interval vectors
#'
#' A _minimal_ interval vector is one that contains no redundant information. In
#' particular, a minimal interval vector:
#'
#' - Has no incomparable intervals
#' - Has no empty intervals
#' - Has no abutting intervals
#' - Has no overlapping intervals
#' - Is ordered by both the `start` and `end` fields
#'
#' [interval_minimize()] will turn an interval vector into its minimal form.
#'
#' @noRd
NULL

interval <- function(start, end) {
  args <- list(start = start, end = end)
  args <- vec_cast_common(!!!args)
  args <- vec_recycle_common(!!!args)
  start <- args$start
  end <- args$end

  # With `na_equal = FALSE`, comparisons between `start` and `end` that can't
  # be made because of missing values will result in a missing value. This
  # occurs when either `start` or `end` contains a missing value or an
  # incomplete column of a data frame where the incomplete value occurs before
  # all ties are broken. We call these cases incomparable, and they result in
  # a missing interval.
  compare <- vec_compare(start, end)

  if (any(compare == 1L, na.rm = TRUE)) {
    abort("`start` must be less than or equal to `end`.")
  }

  if (anyNA(compare)) {
    incomparable <- vec_equal_na(compare)
    start <- vec_assign(start, incomparable, NA)
    end <- vec_assign(end, incomparable, NA)
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

interval_parallel_intersect <- function(x, y) {
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

  start <- vec_parallel_max(x_start, y_start)
  end <- vec_parallel_min(x_end, y_end)

  has_gap <- vec_compare(start, end) == 1L

  if (any(has_gap, na.rm = TRUE)) {
    loc <- which(has_gap)[[1]]

    abort(c(
      "Can't take the intersection of intervals containing a gap.",
      i = "A gap would generate an ambiguous empty interval.",
      i = glue::glue("Location {loc} contains a gap.")
    ))
  }

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

interval_parallel_complement <- function(x, y) {
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

  end <- vec_parallel_max(x_start, y_start)
  start <- vec_parallel_min(x_end, y_end)

  overlap <- start > end
  if (any(overlap, na.rm = TRUE)) {
    loc <- which(overlap)[[1]]

    abort(c(
      "Can't take the complement of overlapping intervals.",
      i = glue::glue("Location {loc} contains an overlap.")
    ))
  }

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

interval_parallel_difference <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_proxy <- interval_proxy(x)
  y_proxy <- interval_proxy(y)

  x_start <- field_start(x_proxy)
  y_start <- field_start(y_proxy)

  x_end <- field_end(x_proxy)
  y_end <- field_end(y_proxy)

  y_contained <-
    (vec_compare(y_start, x_start) == 1L) &
    (vec_compare(y_end, x_end) == -1L)

  if (any(y_contained, na.rm = TRUE)) {
    loc <- which(y_contained)[[1]]

    abort(c(
      "Can't compute a difference when `y` is completely contained within `x`.",
      i = "This would result in two distinct intervals for a single observation.",
      i = glue::glue("Location {loc} contains this issue.")
    ))
  }

  start <- x_start
  end <- x_end

  max_start <- vec_parallel_max(x_start, y_start)
  min_end <- vec_parallel_min(x_end, y_end)

  update <- vec_compare(max_start, min_end) < 0L
  direction <- vec_equal(min_end, x_end)

  clamp_end <- update & direction
  if (any(clamp_end, na.rm = TRUE)) {
    end <- vec_assign(end, clamp_end, vec_slice(max_start, clamp_end))
  }

  clamp_start <- update & !direction
  if (any(clamp_start, na.rm = TRUE)) {
    start <- vec_assign(start, clamp_start, vec_slice(min_end, clamp_start))
  }

  if (anyNA(update)) {
    # Ensure missings / incomparables in `y` get propagated
    incomparable <- vec_equal_na(update)
    start <- vec_assign(start, incomparable, NA)
    end <- vec_assign(end, incomparable, NA)
  }

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

# ------------------------------------------------------------------------------

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
