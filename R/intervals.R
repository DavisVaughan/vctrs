vec_locate_interval_merge_bounds <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             empty = "overlap",
                                             missing = "overlap") {
  check_dots_empty0(...)
  groups <- FALSE
  .Call(vctrs_locate_interval_merge_info, start, end, abutting, empty, missing, groups)
}

vec_locate_interval_merge_groups <- function(start,
                                             end,
                                             ...,
                                             abutting = TRUE,
                                             empty = "overlap",
                                             missing = "overlap") {
  check_dots_empty0(...)
  groups <- TRUE
  .Call(vctrs_locate_interval_merge_info, start, end, abutting, empty, missing, groups)
}

vec_interval_complement <- function(start,
                                    end,
                                    ...,
                                    lower = NULL,
                                    upper = NULL) {
  check_dots_empty0(...)
  .Call(vctrs_interval_complement, start, end, lower, upper)
}
