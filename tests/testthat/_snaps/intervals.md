# missing intervals can cause an error

    Code
      (expect_error(vec_locate_interval_merge_groups(NA, NA, missing = "error")))
    Output
      <error/rlang_error>
      Error:
      ! `start` and `end` can't contain missing values.
    Code
      (expect_error(vec_locate_interval_merge_groups(1, NA, missing = "error")))
    Output
      <error/rlang_error>
      Error:
      ! `start` and `end` can't contain missing values.
    Code
      (expect_error(vec_locate_interval_merge_groups(NA, 1, missing = "error")))
    Output
      <error/rlang_error>
      Error:
      ! `start` and `end` can't contain missing values.

# empty intervals can cause an error

    Code
      (expect_error(vec_locate_interval_merge_groups(1, 1, empty = "error")))
    Output
      <error/rlang_error>
      Error:
      ! `start` must be less than `end`.

# can't have `start > end`

    Code
      (expect_error(vec_locate_interval_merge_groups(x$start, x$end)))
    Output
      <error/rlang_error>
      Error:
      ! `start` must be less than or equal to `end`.

# common type is taken

    Code
      (expect_error(vec_locate_interval_merge_groups(1, "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `start` <double> and `end` <character>.

