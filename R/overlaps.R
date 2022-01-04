new_interval <- function(start, end, ..., class = character()) {
  fields <- list(start = start, end = end)
  new_rcrd(fields, ..., class = c("vctrs_interval", class))
}

interval <- function(start = integer(), end = integer()) {
  args <- vec_cast_common(start = start, end = end, .to = integer())
  args <- vec_recycle_common(!!!args)
  start <- args$start
  end <- args$end

  if (any(vec_equal_na(start)) || any(vec_equal_na(end))) {
    abort("`start` and `end` can't contain missing values.")
  }

  out <- new_interval(start, end)
  out <- interval_standardize(out)

  out
}

#' @export
format.vctrs_interval <- function(x, ...) {
  start <- interval_start(x)
  end <- interval_end(x)
  empty <- interval_empty(x)

  start <- as.character(start)
  end <- as.character(end)

  out <- as.character(glue::glue("{start}, {end}"))
  out[empty] <- ""
  out <- as.character(glue::glue("[{out})"))

  out
}

interval_maximum <- function() {
  .Machine$integer.max
}
interval_minimum <- function() {
  -.Machine$integer.max
}

interval_standardize <- function(x) {
  start <- interval_start(x)
  end <- interval_end(x)

  empty <- start >= end

  if (any(empty)) {
    start[empty] <- interval_maximum()
    end[empty] <- interval_minimum()
  }

  new_interval(start, end)
}

interval_start <- function(x) {
  field(x, "start")
}
interval_end <- function(x) {
  field(x, "end")
}

interval_empty <- function(x) {
  (interval_start(x) == interval_maximum()) & (interval_end(x) == interval_minimum())
}

interval_range <- function(x) {
  if (length(x) == 0L) {
    interval(start = interval_maximum(), end = interval_minimum())
  } else {
    interval(start = min(interval_start(x)), end = max(interval_end(x)))
  }
}

#' Minimize an interval
#'
#' @description
#' `interval_minimize()` collapses redundant information in `start` and
#' `end`, resulting in new `start` and `end` values that represent the interval
#' in the most minimal form.
#'
#' A minimal set of intervals:
#' - Has no empty intervals
#' - Has no overlapping intervals
#' - Has no adjacent intervals
#' - Is ordered from left to right
#'
#' An empty interval is one where `start >= end`. Two intervals are adjacent
#' if the open endpoint of one is equivalent to the closed startpoint of
#' the other. For example, `[a, b)` and `[b, c)` are adjacent.
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
#' start <- c(1L, 10L, 2L, 2L, 9L)
#' end <- c(5L, 12L, 6L, 8L, 10L)
#'
#' # Look at the overlaps
#' df <- data_frame(start = start, end = end)
#' df
#'
#' # Remove all redundant overlaps
#' interval_minimize(start, end)
#'
#' # You can set also set `gap` to be `>0` to link intervals that have actual
#' # gaps between them
#' interval_minimize(start, end, gap = 1L)
#'
#' # Compute locations telling you where to slice the start/end data to
#' # construct the minimal result and how to map each start/end combination
#' # of the input to its corresponding minimal result in the output.
#' info <- interval_locate_minimal_groups(start, end)
#' info
#'
#' old <- vec_slice(df, vec_unchop(info$loc))
#'
#' new <- data_frame(
#'   start_minimal = vec_slice(df$start, info$key$start),
#'   end_minimal = vec_slice(df$end, info$key$end)
#' )
#' new <- vec_slice(new, vec_rep_each(vec_seq_along(info), list_sizes(info$loc)))
#'
#' vec_cbind(old, new)
interval_minimize <- function(x, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- FALSE
  groups <- FALSE
  start <- interval_start(x)
  end <- interval_end(x)
  fields <- .Call(vctrs_interval_minimize, start, end, locations, groups, gap)
  new_interval(fields$start, fields$end)
}

interval_locate_minimal <- function(x, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- TRUE
  groups <- FALSE
  start <- interval_start(x)
  end <- interval_end(x)
  .Call(vctrs_interval_minimize, start, end, locations, groups, gap)
}

interval_locate_minimal_groups <- function(x, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- TRUE
  groups <- TRUE
  start <- interval_start(x)
  end <- interval_end(x)
  .Call(vctrs_interval_minimize, start, end, locations, groups, gap)
}

interval_complement <- function(x, ..., force_start = NULL, force_end = NULL) {
  check_dots_empty0(...)
  start <- interval_start(x)
  end <- interval_end(x)
  out <- .Call(vctrs_interval_complement, start, end, force_start, force_end)
  new_interval(out$start, out$end)
}

interval_union <- function(x, y) {
  out <- vec_c(x, y)
  interval_minimize(out)
}

interval_difference <- function(x, y) {
  x_range <- interval_range(x)
  y_range <- interval_range(y)

  force_start <- min(interval_start(x_range), interval_start(y_range))
  force_end <- max(interval_end(x_range), interval_end(y_range))

  x_c <- interval_complement(x, force_start = force_start, force_end = force_end)

  u <- interval_union(x_c, y)

  interval_complement(u, force_start = force_start, force_end = force_end)
}

interval_intersect <- function(x, y) {
  x_range <- interval_range(x)
  y_range <- interval_range(y)

  force_start <- min(interval_start(x_range), interval_start(y_range))
  force_end <- max(interval_end(x_range), interval_end(y_range))

  x_c <- interval_complement(x, force_start = force_start, force_end = force_end)
  y_c <- interval_complement(y, force_start = force_start, force_end = force_end)

  u <- interval_union(x_c, y_c)

  interval_complement(u, force_start = force_start, force_end = force_end)
}

interval_parallel_union <- function(x, y, ..., fill_gap = FALSE) {
  if (!is_bool(fill_gap)) {
    abort("`fill_gap` must be a single `TRUE` or `FALSE`.")
  }

  if (!fill_gap) {
    complement <- interval_parallel_complement(x, y)
    has_gap <- !interval_empty(complement)

    if (any(has_gap)) {
      loc <- which(has_gap)[[1]]

      gap <- vec_slice(complement, loc)
      gap <- interval_end(gap) - interval_start(gap)

      abort(c(
        "Can't take the union of intervals containing a gap.",
        i = glue::glue("Location {loc} contains a gap of size {gap}."),
        i = "Set `fill_gap = TRUE` to force a union anyways."
      ))
    }
  }

  start <- vec_parallel_min(interval_start(x), interval_start(y))
  end <- vec_parallel_max(interval_end(x), interval_end(y))

  new_interval(start, end)
}

interval_parallel_intersect <- function(x, y) {
  start <- vec_parallel_max(interval_start(x), interval_start(y))
  end <- vec_parallel_min(interval_end(x), interval_end(y))

  out <- new_interval(start, end)
  out <- interval_standardize(out)

  out
}

interval_parallel_difference <- function(x, y) {
  x_start <- interval_start(x)
  x_end <- interval_end(x)

  y_start <- interval_start(y)
  y_end <- interval_end(y)

  y_contained <- (y_start > x_start) & (y_end < x_end) & !interval_empty(y)

  if (any(y_contained)) {
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
  if (any(clamp_end)) {
    end[clamp_end] <- max_start[clamp_end]
  }

  clamp_start <- update & !direction
  if (any(clamp_start)) {
    start[clamp_start] <- min_end[clamp_start]
  }

  out <- new_interval(start, end)
  out <- interval_standardize(out)

  out
}

interval_parallel_complement <- function(x, y) {
  end <- vec_parallel_max(interval_start(x), interval_start(y))
  start <- vec_parallel_min(interval_end(x), interval_end(y))

  empty <- interval_empty(x) | interval_empty(y)
  if (any(empty)) {
    end[empty] <- start[empty]
  }

  out <- new_interval(start = start, end = end)
  out <- interval_standardize(out)

  out
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

  out <- vec_init(x, vec_size(x))

  if (type == "min") {
    x_wins <- cmp <= 0L
    y_wins <- !x_wins
  } else if (type == "max") {
    x_wins <- cmp >= 0L
    y_wins <- !x_wins
  } else {
    abort("Unknown `type`.")
  }

  out <- vec_assign(out, x_wins, vec_slice(x, x_wins))
  out <- vec_assign(out, y_wins, vec_slice(y, y_wins))

  out
}
