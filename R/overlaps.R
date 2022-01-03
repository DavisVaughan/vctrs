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
#' @param gap `[non-negative integer(1)]`
#'
#'   The maximum gap allowed when deciding whether or not two intervals can
#'   be linked.
#'
#'   The default, `0L`, links intervals that either overlap or touch. For
#'   example, `[1, 3)` and `[3, 4)` would be linked together as `[1, 4)`.
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
#' # You can set also set `gap` to be `>0` to link intervals that have actual
#' # gaps between them
#' interval_link(start, end, gap = 1L)
#'
#' # Compute locations telling you where to slice the start/end data to
#' # construct the linked result and how to map each start/end combination
#' # of the input to its corresponding linked result in the output.
#' info <- interval_locate_link_groups(start, end)
#' info
#'
#' old <- vec_slice(df, vec_unchop(info$loc))
#'
#' new <- data_frame(
#'   start_link = vec_slice(df$start, info$key$start),
#'   end_link = vec_slice(df$end, info$key$end)
#' )
#' new <- vec_slice(new, vec_rep_each(vec_seq_along(info), list_sizes(info$loc)))
#'
#' vec_cbind(old, new)
interval_link <- function(start, end, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- FALSE
  groups <- FALSE
  .Call(vctrs_interval_link, start, end, locations, groups, gap)
}

interval_locate_links <- function(start, end, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- TRUE
  groups <- FALSE
  .Call(vctrs_interval_link, start, end, locations, groups, gap)
}

interval_locate_link_groups <- function(start, end, ..., gap = 0L) {
  check_dots_empty0(...)
  locations <- TRUE
  groups <- TRUE
  .Call(vctrs_interval_link, start, end, locations, groups, gap)
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
