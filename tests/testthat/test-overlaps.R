# ------------------------------------------------------------------------------
# interval_minimize()

test_that("can minimize overlaps", {
  x <- interval(
    c(1L, 10L,  2L, 2L, 9L),
    c(5L, 12L, 6L, 8L, 11L)
  )

  expect_identical(
    interval_minimize(x),
    interval(c(1L, 9L), c(8L, 12L))
  )
})

test_that("`[a, b)` combines with `[b, c)`", {
  x <- interval(
    c(10L, 9L),
    c(12L, 10L)
  )

  expect_identical(
    interval_minimize(x),
    interval(9L, 12L)
  )
})

test_that("keys are returned ordered", {
  x <- interval(start = c(4L, 3L, 1L), end = c(6L, 5L, 2L))

  expect_identical(
    interval_minimize(x),
    interval(start = c(1L, 3L), end = c(2L, 6L))
  )
})

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- interval(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    interval_minimize(x),
    interval(start = c(1L, 12L), end = c(10L, 15L))
  )
})

test_that("can minimize with size zero input", {
  expect_identical(
    interval_minimize(interval(integer(), integer())),
    interval(integer(), integer())
  )
})

test_that("can minimize with size one input", {
  expect_identical(
    interval_minimize(interval(1L, 2L)),
    interval(1L, 2L)
  )
})

test_that("missing intervals are removed by default", {
  x <- interval(NA, NA)
  expect_identical(interval_minimize(x), interval())
})

test_that("missing intervals don't affect the result", {
  x <- interval(c(3, NA, 2, NA), c(5, NA, 3, NA))
  expect_identical(interval_minimize(x), interval(2, 5))
})

# ------------------------------------------------------------------------------
# interval_locate_minimal()

test_that("can compute minimal locations", {
  x <- interval(
    c(1L, 9L,  2L, 2L, 10L),
    c(5L, 11L, 6L, 8L, 12L)
  )

  expect_identical(
    interval_locate_minimal(x),
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )
})

test_that("can minimize with size one input", {
  x <- interval(1L, 2L)

  expect_identical(
    interval_locate_minimal(x),
    data_frame(start = 1L, end = 1L)
  )
})

test_that("can minimize with size zero input", {
  x <- interval(integer(), integer())

  expect_identical(
    interval_locate_minimal(x),
    data_frame(start = integer(), end = integer())
  )
})

test_that("missing intervals are removed", {
  x <- interval(NA, NA)
  expect_identical(
    interval_locate_minimal(x),
    data_frame(start = integer(), end = integer())
  )
})

test_that("missing intervals don't affect the result", {
  x <- interval(c(3, NA, 2, NA), c(5, NA, 3, NA))
  expect_identical(
    interval_locate_minimal(x),
    data_frame(start = 3L, end = 1L)
  )
})

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- interval(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    interval_locate_minimal(x),
    data_frame(start = c(1L, 3L), end = c(1L, 3L))
  )
})

# ------------------------------------------------------------------------------
# interval_locate_minimal_groups()

test_that("can compute minimal locations and groups", {
  x <- interval(
    c(1L, 9L,  2L, 2L, 10L),
    c(5L, 11L, 6L, 8L, 12L)
  )

  out <- interval_locate_minimal_groups(x)

  expect_identical(
    out$key,
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )

  expect_identical(
    out$loc,
    list(c(1L, 3L, 4L), c(2L, 5L))
  )
})

test_that("can minimize with size one input", {
  expect_identical(
    interval_locate_minimal_groups(interval(1L, 2L)),
    data_frame(
      key = data_frame(start = 1L, end = 1L),
      loc = list(1L)
    )
  )
})

test_that("can minimize with size zero input", {
  expect_identical(
    interval_locate_minimal_groups(interval(integer(), integer())),
    data_frame(
      key = data_frame(start = integer(), end = integer()),
      loc = list()
    )
  )
})

test_that("locations are ordered by both `start` and `end`", {
  x <- interval(start = c(4L, 4L, 1L), end = c(6L, 5L, 2L))

  out <- interval_locate_minimal_groups(x)

  # Ties of `start = 4` are broken by `end` values and reordered
  expect_identical(
    out$loc,
    list(3L, c(2L, 1L))
  )

  # So this orders `x`
  expect_identical(
    vec_slice(x, unlist(out$loc)),
    vec_sort(x)
  )
})

test_that("missing intervals are removed by default", {
  x <- interval(NA, NA)

  out <- interval_locate_minimal_groups(x)

  expect_identical(
    out$key,
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    out$loc,
    list()
  )
})

test_that("missing intervals can be retained", {
  x <- interval(NA, NA)

  out <- interval_locate_minimal_groups(x, keep_missing = TRUE)

  expect_identical(
    out$key,
    data_frame(start = NA_integer_, end = NA_integer_)
  )
  expect_identical(
    out$loc,
    list(1L)
  )
})

test_that("empty intervals can be retained", {
  x <- interval(1, 1)

  out <- interval_locate_minimal_groups(x, keep_empty = TRUE)

  expect_identical(
    out$key,
    data_frame(start = 1L, end = 1L)
  )
  expect_identical(
    out$loc,
    list(1L)
  )
})

test_that("all combinations of `keep_empty` and `keep_missing` work", {
  x <- interval(
    c(1, NA, 2, 1, 7, NA, 9),
    c(1, NA, 3, 5, 8, NA, 9)
  )

  expect_identical(
    interval_locate_minimal_groups(x, keep_empty = FALSE, keep_missing = FALSE),
    data_frame(
      key = data_frame(start = c(4L, 5L), end = c(4L, 5L)),
      loc = list(c(4L, 3L), 5L)
    )
  )

  expect_identical(
    interval_locate_minimal_groups(x, keep_empty = TRUE, keep_missing = FALSE),
    data_frame(
      key = data_frame(start = c(1L, 5L, 7L), end = c(4L, 5L, 7L)),
      loc = list(c(1L, 4L, 3L), 5L, 7L)
    )
  )

  expect_identical(
    interval_locate_minimal_groups(x, keep_empty = FALSE, keep_missing = TRUE),
    data_frame(
      key = data_frame(start = c(4L, 5L, NA), end = c(4L, 5L, NA)),
      loc = list(c(4L, 3L), 5L, c(2L, 6L))
    )
  )

  expect_identical(
    interval_locate_minimal_groups(x, keep_empty = TRUE, keep_missing = TRUE),
    data_frame(
      key = data_frame(start = c(1L, 5L, 7L, NA), end = c(4L, 5L, 7L, NA)),
      loc = list(c(1L, 4L, 3L), 5L, 7L, c(2L, 6L))
    )
  )
})

test_that("missing intervals don't affect the result by default", {
  x <- interval(c(3, NA, 2, NA), c(5, NA, 3, NA))

  out <- interval_locate_minimal_groups(x)

  expect_identical(
    out$key,
    data_frame(start = 3L, end = 1L)
  )
  expect_identical(
    out$loc,
    list(c(3L, 1L))
  )
})

test_that("can have missings in either input", {
  x <- new_interval(c(1L, NA, 1L), c(NA, 1L, 2L))

  out <- interval_locate_minimal_groups(x)

  expect_identical(out$key, data_frame(start = 3L, end = 3L))
  expect_identical(out$loc, list(3L))

  out <- interval_locate_minimal_groups(x, keep_missing = TRUE)

  expect_identical(out$key, data_frame(start = c(3L, NA), end = c(3L, NA)))
  expect_identical(out$loc, list(3L, c(2L, 1L)))
})

test_that("can set `keep_missing = TRUE` without any missings", {
  x <- interval(c(1, 3), c(3, 5))

  out <- interval_locate_minimal_groups(x, keep_missing = TRUE)

  expect_identical(out$key, data_frame(start = 1L, end = 2L))
  expect_identical(out$loc, list(c(1L, 2L)))
})

test_that("can't have `start > end`", {
  x <- new_interval(1L, 0L)
  expect_snapshot((expect_error(interval_locate_minimal_groups(x))))
})

# ------------------------------------------------------------------------------
# interval_complement()

test_that("computes the complement", {
  x <- interval(
    c(6L, 1L, 2L, 12L),
    c(9L, 3L, 4L, 14L)
  )

  expect_identical(
    interval_complement(x),
    interval(start = c(4L, 9L), end = c(6L, 12L))
  )
})

test_that("treats intervals as half-open like [a, b)", {
  x <- interval(
    c(1L, 5L),
    c(4L, 6L)
  )

  expect_identical(
    interval_complement(x),
    interval(start = 4L, end = 5L)
  )
})

test_that("`[a, b)` and `[b, c)` result in no complement values", {
  x <- interval(
    c(1L, 5L),
    c(5L, 6L)
  )

  expect_identical(
    interval_complement(x),
    interval(start = integer(), end = integer())
  )
})

test_that("complement is invertible", {
  x <- interval(
    c(1L, 5L),
    c(5L, 6L)
  )

  start <- min(interval_start(x))
  end <- max(interval_end(x))

  # Should always be seen as invertible as long as linking is done first
  x <- interval_minimize(x)

  x_c <- interval_complement(x, start = start, end = end)
  x2 <- interval_complement(x_c, start = start, end = end)

  expect_identical(x, x2)
})

test_that("works with `start >= end`", {
  x <- interval(
    c(1L, 2L, 12L, NA),
    c(10L, 5L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, start = 10L, end = 9L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(x, start = 10L, end = 10L),
    interval(start = integer(), end = integer())
  )
})

test_that("works with `start >= end` before any values", {
  x <- interval(
    c(1L, 2L, 12L, NA),
    c(10L, 5L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, start = -1L, end = -3L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(x, start = -1L, end = -1L),
    interval(start = integer(), end = integer())
  )
})

test_that("works with `start >= end` after any values", {
  x <- interval(
    c(1L, 2L, 12L, NA),
    c(10L, 5L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, start = 20L, end = 18L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(x, start = 20L, end = 20L),
    interval(start = integer(), end = integer())
  )
})

test_that("works with `start` before any values", {
  x <- interval(
    c(1L, 2L, 12L, NA),
    c(10L, 5L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, start = -1L),
    interval(start = c(-1L, 10L), end = c(1L, 12L))
  )
})

test_that("works if both `start` and `end` are before any values", {
  x <- interval(
    c(2L, 1L, 12L, NA),
    c(5L, 10L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, start = -5L, end = -2L),
    interval(start = -5L, end = -2L)
  )
})

test_that("works with `end` after any values", {
  x <- interval(
    c(2L, 1L, 13L, 12L, NA),
    c(5L, 10L, 17L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, end = 20L),
    interval(start = c(10L, 17L), end = c(12L, 20L))
  )
})

test_that("works if both `start` and `end` are after any values", {
  x <- interval(
    c(2L, 1L, 12L, NA),
    c(5L, 10L, 15L, NA)
  )

  expect_identical(
    interval_complement(x, start = 17L, end = 19L),
    interval(start = 17L, end = 19L)
  )
})

test_that("works with only NA and `start`", {
  x <- interval(NA, NA)
  expect_identical(interval_complement(x, start = 5L), interval())
})

test_that("works with only NA and `end`", {
  x <- interval(NA, NA)
  expect_identical(interval_complement(x, end = 5L), interval())
})

test_that("works with only NA and both `start` and `end`", {
  x <- interval(NA, NA)
  expect_identical(interval_complement(x, start = 2L, end = 5L), interval(2, 5))
  expect_identical(interval_complement(x, start = 2L, end = -5L), interval())
})

test_that("works with `start` that is on the max set value", {
  x <- interval(
    c(1L, 12L),
    c(9L, 13L)
  )

  expect_identical(
    interval_complement(x, start = 9L),
    interval(start = 9L, end = 12L)
  )
})

test_that("works with `end` that is on the max set value", {
  x <- interval(
    c(1L, 2L, 12L),
    c(10L, 5L, 15L)
  )

  expect_identical(
    interval_complement(x, end = 10L),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(x, start = 10L, end = 10L),
    interval(start = integer(), end = integer())
  )
})

test_that("size zero case generally returns nothing", {
  expect_identical(
    interval_complement(interval(integer(), integer())),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(interval(integer(), integer()), start = 5L),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(interval(integer(), integer()), end = 5L),
    interval(start = integer(), end = integer())
  )
})

test_that("size zero case with both `start` and `end` returns an interval", {
  expect_identical(
    interval_complement(interval(integer(), integer()), start = 5L, end = 10L),
    interval(start = 5L, end = 10L)
  )
})

test_that("size zero case with `start >= end` doesn't return anything", {
  expect_identical(
    interval_complement(interval(integer(), integer()), start = 5L, end = 5L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(interval(integer(), integer()), start = 10L, end = 5L),
    interval(start = integer(), end = integer())
  )
})

# ------------------------------------------------------------------------------
# interval_union()

test_that("union links", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))
  y <- interval(start = 5L, end = 6L)

  expect_identical(
    interval_union(x, y),
    interval(start = c(1L, 5L), end = c(3L, 6L))
  )
})

test_that("union treats intervals as half open `[a, b)`", {
  x <- interval(start = 1L, end = 2L)
  y <- interval(start = 3L, end = 5L)

  expect_identical(
    interval_union(x, y),
    interval(start = c(1L, 3L), end = c(2L, 5L))
  )
})

test_that("union drops NAs", {
  x <- interval(c(1, NA), c(2, NA))
  y <- interval(2, 3)

  expect_identical(
    interval_union(x, y),
    interval(1, 3)
  )
})

# ------------------------------------------------------------------------------
# interval_intersect()

test_that("intersect links", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))

  expect_identical(
    interval_intersect(x, x),
    interval(start = 1L, end = 3L)
  )
})

test_that("intersect works", {
  x <- interval(start = c(1L, 6L), end = c(4L, 8L))
  y <- interval(start = 2L, end = 3L)
  z <- interval(start = 3L, end = 7L)

  expect_identical(
    interval_intersect(x, y),
    interval(start = 2L, end = 3L)
  )
  expect_identical(
    interval_intersect(x, z),
    interval(start = c(3L, 6L), end = c(4L, 7L))
  )
})

test_that("intersect works with size zero inputs", {
  x <- interval()
  expect_identical(interval_intersect(x, x), x)
})

test_that("intersect drops NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(c(1, NA), c(4, NA))

  expect_identical(
    interval_intersect(x, y),
    interval(1, 2)
  )
})

# ------------------------------------------------------------------------------
# interval_difference()

test_that("difference links", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))
  y <- interval(start = integer(), end = integer())

  expect_identical(
    interval_difference(x, y),
    interval(start = 1L, end = 3L)
  )
})

test_that("difference works", {
  x <- interval(start = c(1L, 6L), end = c(4L, 8L))
  y <- interval(start = 2L, end = 3L)
  z <- interval(start = 3L, end = 7L)

  expect_identical(
    interval_difference(x, y),
    interval(start = c(1, 3, 6), end = c(2, 4, 8))
  )
  expect_identical(
    interval_difference(x, z),
    interval(start = c(1, 7), end = c(3, 8))
  )
})

test_that("difference works with size zero inputs", {
  x <- interval()
  expect_identical(interval_difference(x, x), x)
})

test_that("difference drops NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(1, 4)

  expect_identical(
    interval_difference(x, y),
    interval(0, 1)
  )
  expect_identical(
    interval_difference(y, x),
    interval(2, 4)
  )
})

# ------------------------------------------------------------------------------
# interval_parallel_union()

test_that("can take the parallel union", {
  x <- interval(1, 3)
  y <- interval(2, 4)

  expect_identical(
    interval_parallel_union(x, y),
    interval(1, 4)
  )

  y <- interval(3, 4)

  expect_identical(
    interval_parallel_union(x, y),
    interval(1, 4)
  )
})

test_that("errors on gaps", {
  x <- interval(1, 3)
  y <- interval(4, 5)

  expect_snapshot((expect_error(interval_parallel_union(x, y))))
})

test_that("can force gaps to be filled", {
  x <- interval(1, 3)
  y <- interval(4, 5)

  expect_identical(
    interval_parallel_union(x, y, fill_gap = TRUE),
    interval(1, 5)
  )
})

test_that("parallel union propagates NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(1, 4)

  expect_identical(
    interval_parallel_union(x, y),
    interval(c(0, NA), c(4, NA))
  )
  expect_identical(
    interval_parallel_union(y, x),
    interval(c(0, NA), c(4, NA))
  )
})

# ------------------------------------------------------------------------------
# interval_parallel_intersect()

test_that("can take parallel intersection", {
  x <- interval(start = 1L, end = 4L)
  y <- interval(start = 0L, end = 3L)

  expect_identical(
    interval_parallel_intersect(x, y),
    interval(start = 1L, end = 3L)
  )
})

test_that("can recycle inputs", {
  x <- interval(start = c(1L, 2L), end = c(4L, 5L))
  y <- interval(start = 0L, end = 3L)

  expect_identical(
    interval_parallel_intersect(x, y),
    interval(start = c(1L, 2L), end = c(3L, 3L))
  )
})

test_that("parallel intersection resulting in empty ranges errors", {
  x <- interval(start = 1L, end = 4L)
  y <- interval(start = 4L, end = 5L)

  expect_snapshot(
    (expect_error(interval_parallel_intersect(x, y)))
  )
})

test_that("parallel intersection propagates NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(1, 4)

  expect_identical(
    interval_parallel_intersect(x, y),
    interval(c(1, NA), c(2, NA))
  )
  expect_identical(
    interval_parallel_intersect(y, x),
    interval(c(1, NA), c(2, NA))
  )
})

# ------------------------------------------------------------------------------
# interval_parallel_difference()

test_that("can parallel difference", {
  expect_identical(
    interval_parallel_difference(interval(1, 5), interval(1, 3)),
    interval(3, 5)
  )

  expect_identical(
    interval_parallel_difference(interval(1, 5), interval(-1, 0)),
    interval(1, 5)
  )
})

test_that("parallel difference can't result in an empty interval", {
  expect_snapshot(
    (expect_error(interval_parallel_difference(interval(1, 3), interval(1, 3))))
  )
})

test_that("throws error when `y` is contained within `x`", {
  expect_snapshot(
    (expect_error(interval_parallel_difference(interval(1, 4), interval(2, 3))))
  )
})

test_that("parallel difference propagates NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(1, 4)

  expect_identical(
    interval_parallel_difference(x, y),
    interval(c(0, NA), c(1, NA))
  )
  expect_identical(
    interval_parallel_difference(y, x),
    interval(c(2, NA), c(4, NA))
  )
})

# ------------------------------------------------------------------------------
# interval_parallel_complement()

test_that("can parallel complement", {
  expect_identical(
    interval_parallel_complement(interval(1, 2), interval(5, 6)),
    interval(2, 5)
  )

  expect_identical(
    interval_parallel_complement(interval(1, 2), interval(-1, 0)),
    interval(0, 1)
  )
})

test_that("parallel complement can't result in an empty set", {
  expect_snapshot(
    (expect_error(interval_parallel_complement(interval(1, 2), interval(1, 2))))
  )
})

test_that("parallel complement propagates NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(4, 5)

  expect_identical(
    interval_parallel_complement(x, y),
    interval(c(2, NA), c(4, NA))
  )
  expect_identical(
    interval_parallel_complement(y, x),
    interval(c(2, NA), c(4, NA))
  )
})
