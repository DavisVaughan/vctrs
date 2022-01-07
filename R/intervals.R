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
