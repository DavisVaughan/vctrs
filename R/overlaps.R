vec_merge_overlaps <- function(start, end, ..., locations = FALSE) {
  check_dots_empty0(...)
  .Call(vctrs_merge_overlaps, start, end, locations)
}
