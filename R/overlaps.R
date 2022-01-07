new_interval <- function(start, end, ..., class = character()) {
  fields <- list(start = start, end = end)
  new_rcrd(fields, ..., class = c("vctrs_interval", class))
}

interval <- function(start = integer(), end = integer()) {
  args <- list(start = start, end = end)
  args <- vec_cast_common(!!!args, .to = integer())
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

  if (any(start > end, na.rm = TRUE)) {
    abort("`start` must be less than or equal to `end`.")
  }

  new_interval(start, end)
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

interval_start <- function(x) {
  field(x, "start")
}
interval_end <- function(x) {
  field(x, "end")
}

#' Minimize an interval
#'
#' @description
#' `interval_minimize()` collapses redundant information in `start` and
#' `end`, resulting in new `start` and `end` values that represent the interval
#' in the most minimal form.
#'
#' A minimal set of intervals:
#' - Has no missing intervals
#' - Has no overlapping intervals
#' - Has no adjacent intervals
#' - Is ordered from left to right
#'
#' Two intervals are adjacent if the open endpoint of one is equivalent to the
#' closed startpoint of the other. For example, `[a, b)` and `[b, c)` are
#' adjacent.
#'
#' @inheritParams ellipsis::dots_empty
#'
#' @param start,end `[integer]`
#'
#'   A pair of integer vectors.
#'
#' @param gap `[non-negative integer(1)]`
#'
#'   The maximum gap allowed when deciding whether or not two intervals can
#'   be combined.
#'
#'   The default, `0L`, links intervals that either overlap or are adjacent. For
#'   example, `[1, 3)` and `[3, 4)` would become `[1, 4)`.
#'
#'   Setting this to a positive number will combine intervals with discrete
#'   gaps. For example, with `gap = 1L` the intervals `[1, 3)` and `[4, 5)`
#'   would become `[1, 5)`.
#'
#' @return
#' A data frame with `start` and `end` integer columns containing the minimized
#' interval.
#'
#' @noRd
#'
#' @examples
#' # Look at the overlaps
#' x <- interval(start = c(1L, 10L, 2L, 2L, 9L), end = c(5L, 12L, 6L, 8L, 10L))
#' x
#'
#' # Remove all redundant overlaps
#' interval_minimize(x)
#'
#' # You can set also set `gap` to be `>0` to link intervals that have actual
#' # gaps between them
#' interval_minimize(x, gap = 1L)
#'
#' # Compute locations telling you where to slice the start/end data to
#' # construct the minimal result and how to map each start/end combination
#' # of the input to its corresponding minimal result in the output.
#' info <- interval_locate_minimal_groups(x)
#' info
#'
#' old <- vec_slice(x, vec_unchop(info$loc))
#'
#' new <- interval(
#'   start = vec_slice(interval_start(x), info$key$start),
#'   end = vec_slice(interval_end(x), info$key$end)
#' )
#' new <- vec_slice(new, vec_rep_each(vec_seq_along(info), list_sizes(info$loc)))
#'
#' data_frame(old = old, new = new)
interval_minimize <- function(x, ..., keep_empty = FALSE, keep_missing = FALSE) {
  loc <- interval_locate_minimal(x, ..., keep_empty = keep_empty, keep_missing = keep_missing)
  start <- vec_slice(interval_start(x), loc$start)
  end <- vec_slice(interval_end(x), loc$end)
  new_interval(start, end)
}

interval_locate_minimal <- function(x, ..., keep_empty = FALSE, keep_missing = FALSE) {
  check_dots_empty0(...)
  groups <- FALSE
  .Call(vctrs_interval_locate_minimal, interval_start(x), interval_end(x), keep_empty, keep_missing, groups)
}

interval_locate_minimal_groups <- function(x, ..., keep_empty = FALSE, keep_missing = FALSE) {
  check_dots_empty0(...)
  groups <- TRUE
  .Call(vctrs_interval_locate_minimal, interval_start(x), interval_end(x), keep_empty, keep_missing, groups)
}

interval_complement <- function(x, ..., lower = NULL, upper = NULL) {
  check_dots_empty0(...)
  out <- .Call(vctrs_interval_complement, interval_start(x), interval_end(x), lower, upper)
  new_interval(out$start, out$end)
}

interval_union <- function(x, y) {
  out <- vec_c(x, y)
  interval_minimize(out)
}

interval_difference <- function(x, y) {
  lower <- min(int_min(interval_start(x)), int_min(interval_start(y)))
  upper <- max(int_max(interval_end(x)), int_max(interval_end(y)))

  x_c <- interval_complement(x, lower = lower, upper = upper)

  u <- interval_union(x_c, y)

  interval_complement(u, lower = lower, upper = upper)
}

interval_intersect <- function(x, y) {
  lower <- min(int_min(interval_start(x)), int_min(interval_start(y)))
  upper <- max(int_max(interval_end(x)), int_max(interval_end(y)))

  x_c <- interval_complement(x, lower = lower, upper = upper)
  y_c <- interval_complement(y, lower = lower, upper = upper)

  u <- interval_union(x_c, y_c)

  interval_complement(u, lower = lower, upper = upper)
}

interval_parallel_union <- function(x, y, ..., fill_gap = FALSE) {
  if (!is_bool(fill_gap)) {
    abort("`fill_gap` must be a single `TRUE` or `FALSE`.")
  }

  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_start <- interval_start(x)
  x_end <- interval_end(x)

  y_start <- interval_start(y)
  y_end <- interval_end(y)

  if (!fill_gap) {
    gap <- vec_parallel_max(x_start, y_start) - vec_parallel_min(x_end, y_end)
    has_gap <- gap > 0L

    if (any(has_gap, na.rm = TRUE)) {
      loc <- which(has_gap)[[1]]
      gap <- gap[[loc]]

      abort(c(
        "Can't take the union of intervals containing a gap.",
        i = glue::glue("Location {loc} contains a gap of size {gap}."),
        i = "Set `fill_gap = TRUE` to force a union anyways."
      ))
    }
  }

  start <- vec_parallel_min(x_start, y_start)
  end <- vec_parallel_max(x_end, y_end)

  new_interval(start, end)
}

interval_parallel_intersect <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  start <- vec_parallel_max(interval_start(x), interval_start(y))
  end <- vec_parallel_min(interval_end(x), interval_end(y))

  empty <- start >= end
  if (any(empty, na.rm = TRUE)) {
    loc <- which(empty)[[1]]

    abort(c(
      "Intersection between `x` and `y` can't result in an empty interval.",
      i = glue::glue("Intersection is empty at location {loc}.")
    ))
  }

  new_interval(start, end)
}

interval_parallel_difference <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_start <- interval_start(x)
  x_end <- interval_end(x)

  y_start <- interval_start(y)
  y_end <- interval_end(y)

  y_contained <- (y_start > x_start) & (y_end < x_end)
  if (any(y_contained, na.rm = TRUE)) {
    loc <- which(y_contained)[[1]]

    abort(c(
      "Can't subtract ranges when `y` is completely contained within `x`.",
      i = glue::glue("This occurs at location {loc}.")
    ))
  }

  start <- x_start
  end <- x_end

  max_start <- vec_parallel_max(x_start, y_start)
  min_end <- vec_parallel_min(x_end, y_end)

  update <- max_start <= min_end
  direction <- min_end == x_end

  clamp_end <- update & direction
  if (any(clamp_end, na.rm = TRUE)) {
    end <- vec_assign(end, clamp_end, vec_slice(max_start, clamp_end))
  }

  clamp_start <- update & !direction
  if (any(clamp_start, na.rm = TRUE)) {
    start <- vec_assign(start, clamp_start, vec_slice(min_end, clamp_start))
  }

  missing <- vec_equal_na(x) | vec_equal_na(y)
  if (any(missing)) {
    start <- vec_assign(start, missing, NA)
    end <- vec_assign(end, missing, NA)
  }

  empty <- start >= end
  if (any(empty, na.rm = TRUE)) {
    loc <- which(empty)[[1]]

    abort(c(
      "Difference between `x` and `y` can't result in an empty interval.",
      i = glue::glue("Difference is empty at location {loc}.")
    ))
  }

  new_interval(start, end)
}

interval_parallel_complement <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  end <- vec_parallel_max(interval_start(x), interval_start(y))
  start <- vec_parallel_min(interval_end(x), interval_end(y))

  empty <- start >= end
  if (any(empty, na.rm = TRUE)) {
    loc <- which(empty)[[1]]

    abort(c(
      "Complement between `x` and `y` can't result in an empty interval.",
      i = glue::glue("Complement is empty at location {loc}.")
    ))
  }

  new_interval(start, end)
}


vec_parallel_min <- function(x, y) {
  vec_parallel_summary(x, y, type = "min")
}
vec_parallel_max <- function(x, y) {
  vec_parallel_summary(x, y, type = "max")
}
vec_parallel_summary <- function(x, y, type) {
  args <- vec_cast_common(x = x, y = y)
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

  out <- vec_init(x, vec_size(x))
  out <- vec_assign(out, x_wins, vec_slice(x, x_wins))
  out <- vec_assign(out, y_wins, vec_slice(y, y_wins))

  out
}

int_min <- function(x) {
  empty <- vec_equal_na(x)
  if (any(empty)) {
    x <- vec_slice(x, !empty)
  }

  if (is_empty(x)) {
    .Machine$integer.max
  } else {
    min(x)
  }
}

int_max <- function(x) {
  empty <- vec_equal_na(x)
  if (any(empty)) {
    x <- vec_slice(x, !empty)
  }

  if (is_empty(x)) {
    -.Machine$integer.max
  } else {
    max(x)
  }
}

# ------------------------------------------------------------------------------

vec_interval <- function(start, end) {
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

  if (any(vec_compare(start, end) >= 0L, na.rm = TRUE)) {
    abort("`start` must be less than `end`.")
  }

  new_interval(start, end)
}

vec_locate_minimal_interval <- function(x) {
  .Call(vctrs_locate_minimal_interval, interval_start(x), interval_end(x))
}

vec_locate_minimal_interval_groups <- function(x) {
  .Call(vctrs_locate_minimal_interval_groups, interval_start(x), interval_end(x))
}

vec_interval_complement <- function(x, ..., start = NULL, end = NULL) {
  check_dots_empty0(...)
  out <- .Call(vctrs_interval_complement2, interval_start(x), interval_end(x), start, end)
  new_interval(out$start, out$end)
}

vec_interval_minimize <- function(x) {
  loc <- vec_locate_minimal_interval(x)
  new_interval(
    start = vec_slice(interval_start(x), loc$start),
    end = vec_slice(interval_end(x), loc$end)
  )
}

vec_interval_union <- function(x, y) {
  out <- vec_c(x, y)
  vec_interval_minimize(out)
}

vec_interval_difference <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  if (vec_size(x) == 0L || all(vec_equal_na(x))) {
    return(vec_interval_minimize(x))
  }
  if (vec_size(y) == 0L || all(vec_equal_na(y))) {
    return(vec_interval_minimize(x))
  }

  start <- min(
    min(interval_start(x), na.rm = TRUE),
    min(interval_start(y), na.rm = TRUE)
  )
  end <- max(
    max(interval_end(x), na.rm = TRUE),
    max(interval_end(y), na.rm = TRUE)
  )

  x_c <- vec_interval_complement(x, start = start, end = end)

  u <- vec_interval_union(x_c, y)

  vec_interval_complement(u, start = start, end = end)
}

vec_interval_intersect <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  if (vec_size(x) == 0L || all(vec_equal_na(x))) {
    return(vec_interval_minimize(x))
  }
  if (vec_size(y) == 0L || all(vec_equal_na(y))) {
    return(vec_interval_minimize(x))
  }

  start <- min(
    min(interval_start(x), na.rm = TRUE),
    min(interval_start(y), na.rm = TRUE)
  )
  end <- max(
    max(interval_end(x), na.rm = TRUE),
    max(interval_end(y), na.rm = TRUE)
  )

  x_c <- vec_interval_complement(x, start = start, end = end)
  y_c <- vec_interval_complement(y, start = start, end = end)

  u <- vec_interval_union(x_c, y_c)

  vec_interval_complement(u, start = start, end = end)
}

vec_interval_parallel_union <- function(x, y, ..., fill_gap = FALSE) {
  if (!is_bool(fill_gap)) {
    abort("`fill_gap` must be a single `TRUE` or `FALSE`.")
  }

  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_start <- interval_start(x)
  x_end <- interval_end(x)

  y_start <- interval_start(y)
  y_end <- interval_end(y)

  if (!fill_gap) {
    max_start <- vec_parallel_max(x_start, y_start)
    min_end <- vec_parallel_min(x_end, y_end)
    has_gap <- vec_compare(max_start, min_end) == 1L

    if (any(has_gap, na.rm = TRUE)) {
      loc <- which(has_gap)[[1]]

      abort(c(
        "Can't take the union of intervals containing a gap.",
        i = glue::glue("Location {loc} contains a gap."),
        i = "Set `fill_gap = TRUE` to force a union anyways."
      ))
    }
  }

  start <- vec_parallel_min(x_start, y_start)
  end <- vec_parallel_max(x_end, y_end)

  new_interval(start, end)
}

vec_interval_parallel_intersect <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  start <- vec_parallel_max(interval_start(x), interval_start(y))
  end <- vec_parallel_min(interval_end(x), interval_end(y))

  empty <- vec_compare(start, end) >= 0L
  if (any(empty, na.rm = TRUE)) {
    loc <- which(empty)[[1]]

    abort(c(
      "Intersection between `x` and `y` can't result in an empty interval.",
      i = glue::glue("Intersection is empty at location {loc}.")
    ))
  }

  new_interval(start, end)
}

vec_interval_parallel_difference <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  x_start <- interval_start(x)
  x_end <- interval_end(x)

  y_start <- interval_start(y)
  y_end <- interval_end(y)

  y_contained <-
    (vec_compare(y_start, x_start) == 1L) &&
    (vec_compare(y_end, x_end) == -1L)

  if (any(y_contained, na.rm = TRUE)) {
    loc <- which(y_contained)[[1]]

    abort(c(
      "Can't subtract ranges when `y` is completely contained within `x`.",
      i = glue::glue("This occurs at location {loc}.")
    ))
  }

  start <- x_start
  end <- x_end

  max_start <- vec_parallel_max(x_start, y_start)
  min_end <- vec_parallel_min(x_end, y_end)

  update <- vec_compare(max_start, min_end) <= 0L
  direction <- vec_equal(min_end, x_end)

  clamp_end <- update & direction
  if (any(clamp_end, na.rm = TRUE)) {
    end <- vec_assign(end, clamp_end, vec_slice(max_start, clamp_end))
  }

  clamp_start <- update & !direction
  if (any(clamp_start, na.rm = TRUE)) {
    start <- vec_assign(start, clamp_start, vec_slice(min_end, clamp_start))
  }

  missing <- vec_equal_na(x) | vec_equal_na(y)
  if (any(missing)) {
    start <- vec_assign(start, missing, NA)
    end <- vec_assign(end, missing, NA)
  }

  empty <- vec_compare(start, end) >= 0L
  if (any(empty, na.rm = TRUE)) {
    loc <- which(empty)[[1]]

    abort(c(
      "Difference between `x` and `y` can't result in an empty interval.",
      i = glue::glue("Difference is empty at location {loc}.")
    ))
  }

  new_interval(start, end)
}

vec_interval_parallel_complement <- function(x, y) {
  args <- list(x = x, y = y)
  args <- vec_recycle_common(!!!args)
  args <- vec_cast_common(!!!args)
  x <- args[[1]]
  y <- args[[2]]

  end <- vec_parallel_max(interval_start(x), interval_start(y))
  start <- vec_parallel_min(interval_end(x), interval_end(y))

  empty <- vec_compare(start, end) >= 0L
  if (any(empty, na.rm = TRUE)) {
    loc <- which(empty)[[1]]

    abort(c(
      "Complement between `x` and `y` can't result in an empty interval.",
      i = glue::glue("Complement is empty at location {loc}.")
    ))
  }

  new_interval(start, end)
}
