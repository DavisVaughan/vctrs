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
      (expect_error(interval_parallel_union(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap.
      i Set `fill = TRUE` to force a union anyways.

---

    Code
      (expect_error(interval_parallel_union(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap.
      i Set `fill = TRUE` to force a union anyways.

# parallel intersection between intervals with a gap errors

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Can't take the intersection of intervals containing a gap.
      i A gap would generate an ambiguous empty interval.
      i Location 1 contains a gap.

---

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Can't take the intersection of intervals containing a gap.
      i A gap would generate an ambiguous empty interval.
      i Location 1 contains a gap.

# parallel complement can't be taken of overlapping intervals

    Code
      (expect_error(interval_parallel_complement(x, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping intervals.
      i Location 1 contains an overlap.

---

    Code
      (expect_error(interval_parallel_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping intervals.
      i Location 1 contains an overlap.
    Code
      (expect_error(interval_parallel_complement(y, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping intervals.
      i Location 1 contains an overlap.

---

    Code
      (expect_error(interval_parallel_complement(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping intervals.
      i Location 1 contains an overlap.
    Code
      (expect_error(interval_parallel_complement(y, x)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_complement()`:
      ! Can't take the complement of overlapping intervals.
      i Location 1 contains an overlap.

# throws error when `y` is contained within `x`

    Code
      (expect_error(interval_parallel_difference(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Can't compute a difference when `y` is completely contained within `x`.
      i This would result in two distinct intervals for a single observation.
      i Location 1 contains this issue.

