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
NULL
