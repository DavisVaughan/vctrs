# `gap` must be 0 or positive

    Code
      (expect_error(interval_minimize(interval(1L, 2L), gap = -1L)))
    Output
      <error/rlang_error>
      Error:
      ! `gap` must be >=0.

# errors on gaps

    Code
      (expect_error(interval_parallel_union(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_union()`:
      ! Can't take the union of intervals containing a gap.
      i Location 1 contains a gap of size 1.
      i Set `fill_gap = TRUE` to force a union anyways.

# parallel intersection resulting in empty ranges errors

    Code
      (expect_error(interval_parallel_intersect(x, y)))
    Output
      <error/rlang_error>
      Error in `interval_parallel_intersect()`:
      ! Intersection between `x` and `y` can't result in an empty interval.
      i Intersection is empty at location 1.

# parallel difference can't result in an empty interval

    Code
      (expect_error(interval_parallel_difference(interval(1, 3), interval(1, 3))))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Difference between `x` and `y` can't result in an empty interval.
      i Difference is empty at location 1.

# throws error when `y` is contained within `x`

    Code
      (expect_error(interval_parallel_difference(interval(1, 4), interval(2, 3))))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Can't subtract ranges when `y` is completely contained within `x`.
      i This occurs at location 1.

# parallel complement can't result in an empty set

    Code
      expect_error(interval_parallel_complement(interval(1, 2), interval(1, 2)))

