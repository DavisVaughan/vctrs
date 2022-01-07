# can't have `start > end`

    Code
      (expect_error(vec_interval_locate_minimal_groups(x$start, x$end)))
    Output
      <error/rlang_error>
      Error:
      ! `start` must be less than or equal to `end`.

# common type is taken

    Code
      (expect_error(vec_interval_locate_minimal_groups(1, "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `start` <double> and `end` <character>.

