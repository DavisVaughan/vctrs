#' Merge overlapping ranges
#'
#' `vec_merge_overlaps()` collapses overlapping information in `start` and
#' `end`, resulting in new `start` and `end` values that are non-overlapping
#' and contain no redundant information.
#'
#' @details
#' If `locations` are returned, they are returned in such a way that they
#' order the original input.
#'
#' @inheritParams ellipsis::dots_empty
#'
#' @param start,end A pair of integer vectors. It is assumed that
#'   `start <= end`, but this is not checked.
#'
#' @param locations Should locations that map the output back to the input
#'   also be returned? If so, they are returned as a list of integer vectors in
#'   an additional `loc` column.
#'
#' @return
#' A data frame with `start` and `end` integer columns containing the collapsed
#' ranges. If `locations` is `TRUE`, an additional `loc` list column will be
#' returned.
#'
#' @noRd
#'
#' @examples
#' start <- c(1L, 10L, 2L, 2L, 9L)
#' end <- c(5L, 12L, 6L, 8L, 10L)
#'
#' # Look at the overlaps
#' df <- data_frame(start = start, end = end)
#'
#' # Remove all redundant overlaps
#' vec_merge_overlaps(start, end)
#'
#' # Retain locations to map input to output
#' info <- vec_merge_overlaps(start, end, locations = TRUE)
#' info
#'
#' old <- vec_slice(df, vec_unchop(info$loc))
#' new <- vec_slice(
#'   data_frame(start_new = info$start, end_new = info$end),
#'   vec_rep_each(vec_seq_along(info), lengths(info$loc))
#' )
#'
#' vec_cbind(old, new)
vec_merge_overlaps <- function(start, end, ..., locations = FALSE) {
  check_dots_empty0(...)
  .Call(vctrs_merge_overlaps, start, end, locations)
}
