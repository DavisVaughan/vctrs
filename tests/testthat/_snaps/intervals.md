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

# `start` must be less than `end`

    Code
      (expect_error(interval(2, 1)))
    Output
      <error/rlang_error>
      Error in `interval()`:
      ! `start` must be less than `end`.

---

    Code
      (expect_error(interval(2, 2)))
    Output
      <error/rlang_error>
      Error in `interval()`:
      ! `start` must be less than `end`.

# errors on gaps

    Code
      (expect_error(interval_parallel_union(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap.
      i Set `fill = TRUE` to force a union anyways.

---

    Code
      (expect_error(interval_parallel_union(y, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap.
      i Set `fill = TRUE` to force a union anyways.

# parallel intersection between non-overlapping intervals errors

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

---

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

---

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

---

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Can't take the intersection of non-overlapping intervals.
      i This would result in an empty interval.
      i Location 1 contains non-overlapping intervals.

# parallel complement of interval with itself is not allowed

    Code
      (expect_error(interval_parallel_complement(x, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

# parallel complement of abutting intervals is not allowed

    Code
      (expect_error(interval_parallel_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

---

    Code
      (expect_error(interval_parallel_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

# parallel complement of overlapping intervals is not allowed

    Code
      (expect_error(interval_parallel_complement(x, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

---

    Code
      (expect_error(interval_parallel_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.
    Code
      (expect_error(interval_parallel_complement(y, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

---

    Code
      (expect_error(interval_parallel_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.
    Code
      (expect_error(interval_parallel_complement(y, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping or abutting intervals.
      i Location 1 contains overlapping or abutting intervals.

# parallel difference between interval and itself is not allowed

    Code
      (expect_error(interval_parallel_difference(x, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Can't compute a difference when `y` completely contains `x`.
      i This would result in an empty interval.
      i Location 1 contains this issue.

# throws error when `y` is contained within `x`

    Code
      (expect_error(interval_parallel_difference(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Can't compute a difference when `y` is completely contained within `x`.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.

# throws error when `y` contains `x`

    Code
      (expect_error(interval_parallel_difference(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Can't compute a difference when `y` completely contains `x`.
      i This would result in an empty interval.
      i Location 1 contains this issue.

# takes the common type with interval fields

    Code
      (expect_error(vec_within(x, y)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`:
      ! Can't combine `x` <character> and `interval_start(y)` <double>.

