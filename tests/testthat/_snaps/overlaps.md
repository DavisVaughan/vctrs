# `gap` must be 0 or positive

    Code
      (expect_error(interval_minimize(1L, 2L, gap = -1L)))
    Output
      <error/rlang_error>
      Error:
      ! `gap` must be >=0.

