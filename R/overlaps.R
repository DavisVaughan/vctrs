#' Link up overlapping ranges
#'
#' `interval_link()` collapses overlapping information in `start` and
#' `end`, resulting in new `start` and `end` values that are non-overlapping
#' and contain no redundant information.
#'
#' @inheritParams ellipsis::dots_empty
#'
#' @param start,end `[integer]`
#'
#'   A pair of integer vectors. It is assumed that `start <= end`, but this is
#'   not checked.
#'
#' @param gap `[integer(1) / NULL]`
#'
#'   The maximum gap allowed when deciding whether or not two intervals can
#'   be linked. The default, `NULL`, requires that two intervals must overlap
#'   to be linked (this is equivalent to a `gap` of `-1L`).
#'
#'   Setting this to `0L` will link adjacent intervals. For example, `[1, 3)`
#'   and `[3, 4)` would be linked together as `[1, 4)`.
#'
#'   Setting this to a positive number will link intervals with discrete gaps.
#'   For example, with `gap = 1L` the intervals `[1, 3)` and `[4, 5)` would be
#'   linked together as `[1, 5)`.
#'
#' @return
#' A data frame with `start` and `end` integer columns containing the collapsed
#' ranges.
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
#' interval_link(start, end)
#'
#' # Note that because these are half-open intervals,
#' # an endpoint of 10) doesn't overlap a startpoint of [10.
#' # To force this to overlap, set `gap = 0` to allow a maximum gap
#' # size of 0 to still be linkable.
#' interval_link(start, end, gap = 0L)
#'
#' # You can set also set `gap` to be `>0` to link intervals that have actual
#' # gaps between them
#' interval_link(start, end, gap = 1L)
#'
#' # Retain locations to map input to output
#' info <- interval_locate_links(start, end)
#' info
#'
#' old <- vec_slice(df, vec_unchop(info$loc))
#' new <- vec_slice(
#'   data_frame(start_new = info$start, end_new = info$end),
#'   vec_rep_each(vec_seq_along(info), lengths(info$loc))
#' )
#'
#' vec_cbind(old, new)
interval_link <- function(start, end, ..., gap = NULL) {
  check_dots_empty0(...)
  locations <- FALSE
  .Call(vctrs_interval_link, start, end, locations, gap)
}

interval_locate_links <- function(start, end, ..., gap = NULL) {
  check_dots_empty0(...)
  locations <- TRUE
  .Call(vctrs_interval_link, start, end, locations, gap)
}

interval_complement <- function(start, end, ..., force_start = NULL, force_end = NULL) {
  check_dots_empty0(...)
  .Call(vctrs_interval_complement, start, end, force_start, force_end)
}

interval_union <- function(x_start, x_end, y_start, y_end) {
  start <- vec_c(x_start, y_start)
  end <- vec_c(x_end, y_end)
  interval_link(start, end)
}

interval_setdiff <- function(x_start, x_end, y_start, y_end) {
  if (length(x_start) == 0L) {
    # Just for the data frame structure (return empty interval object `x` here)
    return(interval_complement(x_start, x_end))
  }

  force_start <- min(vec_c(x_start, y_start))
  force_end <- max(vec_c(x_end, y_end))

  out <- interval_complement(x_start, x_end, force_start = force_start, force_end = force_end)
  out <- interval_union(out$start, out$end, y_start, y_end)
  out <- interval_complement(out$start, out$end, force_start = force_start, force_end = force_end)

  out
}

interval_intersect <- function(x_start, x_end, y_start, y_end) {
  if (length(x_start) == 0L) {
    # Just for the data frame structure (return empty interval object `x` here)
    return(interval_complement(x_start, x_end))
  }

  force_start <- min(vec_c(x_start, y_start))
  force_end <- max(vec_c(x_end, y_end))

  out <- interval_complement(y_start, y_end, force_start = force_start, force_end = force_end)
  out <- interval_setdiff(x_start, x_end, out$start, out$end)

  out
}

# define parallel intersect, union, and setdiff helpers as well
