vec_interval_locate_minimal <- function(start,
                                        end,
                                        ...,
                                        merge_abutting = TRUE,
                                        keep_empty = FALSE,
                                        keep_missing = FALSE) {
  check_dots_empty0(...)
  groups <- FALSE
  .Call(vctrs_interval_locate_minimal, start, end, merge_abutting, keep_empty, keep_missing, groups)
}

vec_interval_locate_minimal_groups <- function(start,
                                               end,
                                               ...,
                                               merge_abutting = TRUE,
                                               keep_empty = FALSE,
                                               keep_missing = FALSE) {
  check_dots_empty0(...)
  groups <- TRUE
  .Call(vctrs_interval_locate_minimal, start, end, merge_abutting, keep_empty, keep_missing, groups)
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
#' right-open, i.e. `[a, b)`. It is required that `a > b`. Empty intervals
#' are not allowed.
#'
#' ## Missing intervals
#'
#' When creating intervals with `interval()`, if either input is
#' [incomplete][vec_detect_complete], then both intervals are set to their
#' missing value.
#'
#' Missing intervals are infectious. For example, when intersecting two
#' sets of intervals in parallel with `interval_parallel_intersect()`, if `x`
#' or `y` contain a missing interval, then the result will also be
#' missing.
#'
#' ## Minimal interval vectors
#'
#' A _minimal_ interval vector is one that contains no redundant information. In
#' particular, a minimal interval vector:
#'
#' - Has no missing intervals
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

  compare <- vec_compare(start, end)

  if (any(compare != -1L, na.rm = TRUE)) {
    abort("`start` must be less than `end`.")
  }

  joint <- data_frame(start = start, end = end)
  complete <- vec_detect_complete(joint)

  if (!all(complete)) {
    incomplete <- !complete
    start <- vec_assign(start, incomplete, NA)
    end <- vec_assign(end, incomplete, NA)
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

interval_locate_minimal <- function(x,
                                    ...,
                                    merge_abutting = TRUE,
                                    keep_missing = FALSE) {
  check_dots_empty0(...)

  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  vec_interval_locate_minimal(
    start = start,
    end = end,
    merge_abutting = merge_abutting,
    keep_missing = keep_missing
  )
}

interval_locate_minimal_groups <- function(x,
                                           ...,
                                           merge_abutting = TRUE,
                                           keep_missing = FALSE) {
  check_dots_empty0(...)

  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  vec_interval_locate_minimal_groups(
    start = start,
    end = end,
    merge_abutting = merge_abutting,
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

interval_minimize <- function(x,
                              ...,
                              merge_abutting = TRUE,
                              keep_missing = FALSE) {
  check_dots_empty0(...)

  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  loc <- vec_interval_locate_minimal(
    start = start,
    end = end,
    merge_abutting = merge_abutting,
    keep_missing = keep_missing
  )

  start <- vec_slice(start, loc$start)
  end <- vec_slice(end, loc$end)

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

interval_update_minimal <- function(x, ..., merge_abutting = TRUE) {
  check_dots_empty0(...)

  proxy <- interval_proxy(x)

  start <- field_start(proxy)
  end <- field_end(proxy)

  groups <- vec_interval_locate_minimal_groups(
    start = start,
    end = end,
    merge_abutting = merge_abutting,
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

  non_overlapping <- vec_compare(start, end) >= 0L

  if (any(non_overlapping, na.rm = TRUE)) {
    loc <- which(non_overlapping)[[1]]

    abort(c(
      "Can't take the intersection of non-overlapping intervals.",
      i = "This would result in an empty interval.",
      i = glue::glue("Location {loc} contains non-overlapping intervals.")
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

  overlaps_or_abuts <- start >= end
  if (any(overlaps_or_abuts, na.rm = TRUE)) {
    loc <- which(overlaps_or_abuts)[[1]]

    abort(c(
      "Can't take the complement of overlapping or abutting intervals.",
      i = glue::glue("Location {loc} contains overlapping or abutting intervals.")
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

  compare_start <- vec_compare(y_start, x_start)
  compare_end <- vec_compare(y_end, x_end)

  y_contained <- (compare_start == 1L) & (compare_end == -1L)

  if (any(y_contained, na.rm = TRUE)) {
    loc <- which(y_contained)[[1]]

    abort(c(
      "Can't compute a difference when `y` is completely contained within `x`.",
      i = "This would result in two distinct intervals for a single observation.",
      i = glue::glue("Location {loc} contains this issue.")
    ))
  }

  y_contains <- (compare_start <= 0L) & (compare_end >= 0L)

  if (any(y_contains, na.rm = TRUE)) {
    loc <- which(y_contains)[[1]]

    abort(c(
      "Can't compute a difference when `y` completely contains `x`.",
      i = "This would result in an empty interval.",
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
    # Ensure missings in `y` get propagated
    missing <- vec_equal_na(update)
    start <- vec_assign(start, missing, NA)
    end <- vec_assign(end, missing, NA)
  }

  out <- new_interval(start, end)
  out <- interval_restore(out, x)

  out
}

vec_within <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  y_proxy <- interval_proxy(y)
  y_start <- field_start(y_proxy)
  y_end <- field_end(y_proxy)

  ptype <- vec_ptype2(x, y_start, x_arg = "x", y_arg = "interval_start(y)")

  x <- vec_cast(x, ptype)
  y_start <- vec_cast(y_start, ptype)
  y_end <- vec_cast(y_end, ptype)

  # a vs [b, c)
  (vec_compare(x, y_start) >= 0L) & (vec_compare(x, y_end) < 0L)
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



# Pass at interval splitting function
# - Could be simplified with a fix to vec_locate_matches() to ensure that
#   missings are always matched exactly even when using `>` or `<`
#
# x <- data_frame(start = c(1, NA, 2, 5, NA, 7, 8), end = c(3, NA, 4, 6, NA, 12, 10))
# interval_split(x)
#
#
# interval_proxy <- function(x) {
#   x
# }
# field_start <- function(x) {
#   x$start
# }
# field_end <- function(x) {
#   x$end
# }
#
# interval_split_points <- function(start, end, keep_missing) {
#   points <- vec_sort(vec_unique(vec_c(start, end)))
#   size_points <- vec_size(points)
#
#   any_missing <- any(vec_equal_na(vec_slice(points, size_points)))
#   if (any_missing) {
#     points <- vec_slice(points, -size_points)
#     size_points <- size_points - 1L
#   }
#
#   slice_start <- 1L
#   slice_end <- size_points
#
#   loc_start <- seq2(slice_start, slice_end - 1L)
#   loc_end <- seq2(slice_start + 1L, slice_end)
#
#   point_start <- vec_slice(points, loc_start)
#   point_end <- vec_slice(points, loc_end)
#
#   if (keep_missing && any_missing) {
#     point_start <- vec_c(point_start, vec_init(point_start))
#     point_end <- vec_c(point_end, vec_init(point_end))
#   }
#
#   list(start = point_start, end = point_end)
# }
#
# interval_split <- function(x, ..., keep_missing = FALSE) {
#   if (!is_bool(keep_missing)) {
#     abort("`keep_missing` must be a single `TRUE` or `FALSE`.")
#   }
#
#   proxy <- interval_proxy(x)
#
#   start <- field_start(proxy)
#   end <- field_end(proxy)
#
#   args <- interval_split_points(start, end, keep_missing)
#   point_start <- args$start
#   point_end <- args$end
#
#   # This doesn't work right now with < or > conditions
#   # if (keep_missing) {
#   #   incomplete <- "match"
#   # } else {
#   #   incomplete <- "drop"
#   # }
#
#   needles <- data_frame(start = point_start, end = point_end)
#   haystack <- data_frame(start = end, end = start)
#
#   # Find actual overlaps from all possible intervals
#   loc <- vec_locate_matches(
#     needles,
#     haystack,
#     condition = c("<", ">"),
#     no_match = "drop",
#     incomplete = "drop",
#     multiple = "first"
#   )
#
#   out <- vec_slice(needles, loc$needles)
#
#   # TODO: Remove this if `incomplete = "match"` works
#   if (keep_missing && any(vec_equal_na(x))) {
#     out <- vec_c(out, vec_init(out))
#   }
#
#   # TODO: Put back into an interval object
#   out
# }
#
# interval_locate_split_groups <- function(x, ..., keep_missing = FALSE) {
#   if (!is_bool(keep_missing)) {
#     abort("`keep_missing` must be a single `TRUE` or `FALSE`.")
#   }
#
#   proxy <- interval_proxy(x)
#
#   start <- field_start(proxy)
#   end <- field_end(proxy)
#
#   args <- interval_split_points(start, end, keep_missing)
#   point_start <- args$start
#   point_end <- args$end
#
#   # This doesn't work right now with < or > conditions
#   # if (keep_missing) {
#   #   incomplete <- "match"
#   # } else {
#   #   incomplete <- "drop"
#   # }
#
#   needles <- data_frame(start = point_start, end = point_end)
#   haystack <- data_frame(start = end, end = start)
#
#   # Find actual overlaps from all possible intervals
#   loc <- vec_locate_matches(
#     needles,
#     haystack,
#     condition = c("<", ">"),
#     no_match = "drop",
#     incomplete = "drop"
#   )
#   loc <- vec_split(loc$haystack, loc$needles)
#
#   # TODO: Put into an interval object
#   key <- vec_slice(needles, loc$key)
#   loc <- loc$val
#
#   out <- data_frame(key = key, loc = loc)
#
#   # TODO: Remove this if `incomplete = "match"` works
#   if (keep_missing) {
#     missing <- vec_equal_na(x)
#
#     if (any(missing)) {
#       missing <- which(missing)
#       key_missing <- vec_init(x)
#       loc_missing <- list(missing)
#       out_missing <- data_frame(key = key_missing, loc = loc_missing)
#       out <- vec_c(out, out_missing)
#     }
#   }
#
#   out
# }
#
# interval_update_splits <- function(x) {
#   proxy <- interval_proxy(x)
#
#   start <- field_start(proxy)
#   end <- field_end(proxy)
#
#   args <- interval_split_points(start, end, keep_missing = TRUE)
#   point_start <- args$start
#   point_end <- args$end
#
#   needles <- data_frame(start = start, end = end)
#   haystack <- data_frame(start = point_end, end = point_start)
#
#   loc <- vec_locate_matches(
#     needles,
#     haystack,
#     condition = c("<", ">"),
#     no_match = "error",
#     incomplete = NA_integer_
#   )
#
#   loc <- vec_split(loc$haystack, loc$needles)
#
#   candidates <- data_frame(start = point_start, end = point_end)
#
#   vec_chop(candidates, loc$val)
# }
