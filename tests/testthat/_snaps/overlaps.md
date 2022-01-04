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

# throws error when `y` is contained within `x`

    Code
      (expect_error(interval_parallel_difference(interval(1, 4), interval(2, 3))))
    Output
      <error/rlang_error>
      Error in `interval_parallel_difference()`:
      ! Can't subtract ranges when `y` is completely contained within `x`.
      i This occurs at location 1.

