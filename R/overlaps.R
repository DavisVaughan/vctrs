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
interval_minimize <- function(start, end, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- FALSE
  groups <- FALSE
  .Call(vctrs_interval_minimize, start, end, locations, groups, gap)
}

interval_locate_minimal <- function(start, end, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- TRUE
  groups <- FALSE
  .Call(vctrs_interval_minimize, start, end, locations, groups, gap)
}

interval_locate_minimal_groups <- function(start, end, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- TRUE
  groups <- TRUE
  .Call(vctrs_interval_minimize, start, end, locations, groups, gap)
}

interval_complement <- function(start, end, ..., force_start = NULL, force_end = NULL) {
  check_dots_empty0(...)
  .Call(vctrs_interval_complement, start, end, force_start, force_end)
}

interval_union <- function(x_start, x_end, y_start, y_end) {
  start <- vec_c(x_start, y_start)
  end <- vec_c(x_end, y_end)
  interval_minimize(start, end)
}

interval_difference <- function(x_start, x_end, y_start, y_end) {
  force_start <- min(int_min(x_start), int_min(y_start))
  force_end <- max(int_max(x_end), int_max(y_end))

  x_c <- interval_complement(x_start, x_end, force_start = force_start, force_end = force_end)

  u <- interval_union(x_c$start, x_c$end, y_start, y_end)

  interval_complement(u$start, u$end, force_start = force_start, force_end = force_end)
}

interval_intersect <- function(x_start, x_end, y_start, y_end) {
  force_start <- min(int_min(x_start), int_min(y_start))
  force_end <- max(int_max(x_end), int_max(y_end))

  x_c <- interval_complement(x_start, x_end, force_start = force_start, force_end = force_end)
  y_c <- interval_complement(y_start, y_end, force_start = force_start, force_end = force_end)

  u <- interval_union(x_c$start, x_c$end, y_c$start, y_c$end)

  interval_complement(u$start, u$end, force_start = force_start, force_end = force_end)
}

interval_parallel_union <- function(x_start, x_end, y_start, y_end, ..., fill_gap = FALSE) {
  if (!is_bool(fill_gap)) {
    abort("`fill_gap` must be a single `TRUE` or `FALSE`.")
  }

  args <- vec_recycle_common(x_start = x_start, x_end = x_end, y_start = y_start, y_end = y_end)
  args <- vec_cast_common(!!!args)
  x_start <- args$x_start
  x_end <- args$x_end
  y_start <- args$y_start
  y_end <- args$y_end

  x_empty <- interval_empty(x_start, x_end)
  if (any(x_empty)) {
    replace <- y_start[x_empty]
    x_start[x_empty] <- replace
    x_end[x_empty] <- replace
  }

  y_empty <- interval_empty(y_start, y_end)
  if (any(y_empty)) {
    replace <- x_start[y_empty]
    y_start[y_empty] <- replace
    y_end[y_empty] <- replace
  }

  if (!fill_gap) {
    gap <- vec_parallel_max(x_start, y_start) - vec_parallel_min(x_end, y_end)
    has_gap <- gap > 0L

    if (any(has_gap)) {
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

  data_frame(start = start, end = end)
}

interval_parallel_intersect <- function(x_start, x_end, y_start, y_end) {
  start <- vec_parallel_max(x_start, y_start)
  end <- vec_parallel_min(x_end, y_end)

  data_frame(start = start, end = end)
}

interval_parallel_difference <- function(x_start, x_end, y_start, y_end) {
  args <- vec_recycle_common(x_start = x_start, x_end = x_end, y_start = y_start, y_end = y_end)
  args <- vec_cast_common(!!!args)
  x_start <- args$x_start
  x_end <- args$x_end
  y_start <- args$y_start
  y_end <- args$y_end

  x_empty <- interval_empty(x_start, x_end)
  if (any(x_empty)) {
    replace <- y_start[x_empty]
    x_start[x_empty] <- replace
    x_end[x_empty] <- replace
  }

  y_empty <- interval_empty(y_start, y_end)
  if (any(y_empty)) {
    replace <- x_start[y_empty]
    y_start[y_empty] <- replace
    y_end[y_empty] <- replace
  }

  x_contained <- (x_start < y_start) & (x_end > y_end)

  if (any(x_contained)) {
    loc <- which(x_contained)[[1]]

    abort(c(
      "Can't subtract ranges when `x` is completely contained in `y`.",
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

  data_frame(start = start, end = end)
}

interval_parallel_complement <- function(x_start, x_end, y_start, y_end) {
  end <- vec_parallel_max(x_start, y_start)
  start <- vec_parallel_min(x_end, y_end)

  empty <- interval_empty(x_start, x_end) | interval_empty(y_start, y_end)
  if (any(empty)) {
    start[empty] <- end[empty]
  }

  data_frame(start = start, end = end)
}

interval_width <- function(x_start, x_end) {
  x_end - x_start
}

interval_empty <- function(x_start, x_end) {
  interval_width(x_start, x_end) <= 0L
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

int_min <- function(x) {
  if (length(x) == 0L) {
    .Machine$integer.max
  } else {
    min(x)
  }
}

int_max <- function(x) {
  if (length(x) == 0L) {
    -.Machine$integer.max
  } else {
    max(x)
  }
}

# define parallel intersect, union, and setdiff helpers as well
