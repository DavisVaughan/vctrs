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

test_that("empty intervals are merged if they fall fully within another interval", {
  x <- interval(start = c(2L, 1L), end = c(2L, 3L))

  expect_identical(
    interval_minimize(x),
    interval(start = 1L, end = 3L)
  )
})

test_that("empty intervals touching another interval on either side are combined", {
  x <- interval(start = c(2L, 2L), end = c(2L, 3L))

  expect_identical(
    interval_minimize(x),
    interval(start = 2L, end = 3L)
  )

  x <- interval(start = c(2L, 1L), end = c(2L, 2L))

  expect_identical(
    interval_minimize(x),
    interval(start = 1L, end = 2L)
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

test_that("empty intervals get dropped", {
  expect_identical(
    interval_minimize(interval(2L, 2L)),
    interval(integer(), integer())
  )
})

test_that("can minimize with size one input", {
  expect_identical(
    interval_minimize(interval(1L, 2L)),
    interval(1L, 2L)
  )
})

test_that("can set a maximum gap of `>0` to combine intervals with gaps", {
  x <- interval(start = c(1L, 3L, 4L), end = c(2L, 4L, 5L))

  expect_identical(
    interval_minimize(x, gap = 1L),
    interval(start = 1L, end = 5L)
  )

  x <- interval(start = c(1L, 3L, 6L), end = c(2L, 4L, 7L))

  expect_identical(
    interval_minimize(x, gap = 1L),
    interval(start = c(1L, 6L), end = c(4L, 7L))
  )
})

test_that("`gap` must be 0 or positive", {
  expect_snapshot((expect_error(interval_minimize(interval(1L, 2L), gap = -1L))))
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

test_that("works with ignored empty intervals", {
  x <- interval(start = c(2L, 2L, 6L, 1L), end = c(3L, 2L, 7L, 2L))

  out <- interval_locate_minimal_groups(x)

  expect_identical(
    out$key,
    data_frame(start = c(4L, 3L), end = c(1L, 3L))
  )
  expect_identical(
    out$loc,
    list(c(4L, 1L), 3L)
  )
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

  force_start <- min(interval_start(x))
  force_end <- max(interval_end(x))

  # Should always be seen as invertible as long as linking is done first
  x <- interval_minimize(x)

  x_c <- interval_complement(x, force_start = force_start, force_end = force_end)
  x2 <- interval_complement(x_c, force_start = force_start, force_end = force_end)

  expect_identical(x, x2)
})

test_that("works with `force_start >= force_end`", {
  x <- interval(
    c(1L, 2L, 12L),
    c(10L, 5L, 15L)
  )

  expect_identical(
    interval_complement(x, force_start = 10L, force_end = 9L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(x, force_start = 10L, force_end = 10L),
    interval(start = integer(), end = integer())
  )
})

test_that("works with `force_start >= force_end` before any values", {
  x <- interval(
    c(1L, 2L, 12L),
    c(10L, 5L, 15L)
  )

  expect_identical(
    interval_complement(x, force_start = -1L, force_end = -3L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(x, force_start = -1L, force_end = -1L),
    interval(start = integer(), end = integer())
  )
})

test_that("works with `force_start >= force_end` after any values", {
  x <- interval(
    c(1L, 2L, 12L),
    c(10L, 5L, 15L)
  )

  expect_identical(
    interval_complement(x, force_start = 20L, force_end = 18L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(x, force_start = 20L, force_end = 20L),
    interval(start = integer(), end = integer())
  )
})

test_that("works with `force_start` before any values", {
  x <- interval(
    c(1L, 2L, 12L),
    c(10L, 5L, 15L)
  )

  expect_identical(
    interval_complement(x, force_start = -1L),
    interval(start = c(-1L, 10L), end = c(1L, 12L))
  )
})

test_that("works if both `force_start` and `force_end` are before any values", {
  x <- interval(
    c(2L, 1L, 12L),
    c(5L, 10L, 15L)
  )

  expect_identical(
    interval_complement(x, force_start = -5L, force_end = -2L),
    interval(start = -5L, end = -2L)
  )
})

test_that("works with `force_end` after any values", {
  x <- interval(
    c(2L, 1L, 13L, 12L),
    c(5L, 10L, 17L, 15L)
  )

  expect_identical(
    interval_complement(x, force_end = 20L),
    interval(start = c(10L, 17L), end = c(12L, 20L))
  )
})

test_that("works if both `force_start` and `force_end` are after any values", {
  x <- interval(
    c(2L, 1L, 12L),
    c(5L, 10L, 15L)
  )

  expect_identical(
    interval_complement(x, force_start = 17L, force_end = 19L),
    interval(start = 17L, end = 19L)
  )
})

test_that("works with `force_start` that is on the max set value", {
  x <- interval(
    c(1L, 12L),
    c(9L, 13L)
  )

  expect_identical(
    interval_complement(x, force_start = 9L),
    interval(start = 9L, end = 12L)
  )
})

test_that("works with `force_end` that is on the max set value", {
  x <- interval(
    c(1L, 2L, 12L),
    c(10L, 5L, 15L)
  )

  expect_identical(
    interval_complement(x, force_end = 10L),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(x, force_start = 10L, force_end = 10L),
    interval(start = integer(), end = integer())
  )
})

test_that("size zero case generally returns nothing", {
  expect_identical(
    interval_complement(interval(integer(), integer())),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(interval(integer(), integer()), force_start = 5L),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(interval(integer(), integer()), force_end = 5L),
    interval(start = integer(), end = integer())
  )
})

test_that("size zero case with both `force_start` and `force_end` returns an interval", {
  expect_identical(
    interval_complement(interval(integer(), integer()), force_start = 5L, force_end = 10L),
    interval(start = 5L, end = 10L)
  )
})

test_that("size zero case with `force_start >= force_end` doesn't return anything", {
  expect_identical(
    interval_complement(interval(integer(), integer()), force_start = 5L, force_end = 5L),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(interval(integer(), integer()), force_start = 10L, force_end = 5L),
    interval(start = integer(), end = integer())
  )
})

test_that("complement of empty interval is correct", {
  x <- interval(start = 5L, end = 5L)

  expect_identical(
    interval_complement(x),
    interval(start = integer(), end = integer())
  )

  x <- interval(start = c(5L, 5L), end = c(5L, 5L))

  expect_identical(
    interval_complement(x),
    interval(start = integer(), end = integer())
  )
})

test_that("complement isn't affected by contained empty interval", {
  x <- interval(start = c(1L, 3L), end = c(5L, 3L))

  expect_identical(
    interval_complement(x),
    interval(start = integer(), end = integer())
  )
})

test_that("complement isn't affected by empty interval", {
  x <- interval(start = c(1L, 7L), end = c(5L, 7L))

  expect_identical(
    interval_complement(x),
    interval(start = integer(), end = integer())
  )
})

test_that("complement isn't affected by empty interval when `force_start` is set", {
  x <- interval(start = 3L, end = 3L)

  expect_identical(
    interval_complement(x, force_start = 1L),
    interval(start = integer(), end = integer())
  )

  x <- interval(start = c(3L, 5L), end = c(3L, 6L))

  expect_identical(
    interval_complement(x, force_start = 1L),
    interval(start = 1L, end = 5L)
  )
})

test_that("complement isn't affected by empty interval when `force_end` is set", {
  x <- interval(start = 3L, end = 3L)

  expect_identical(
    interval_complement(x, force_end = 8L),
    interval(start = integer(), end = integer())
  )

  x <- interval(start = c(3L, 5L), end = c(3L, 6L))

  expect_identical(
    interval_complement(x, force_end = 8L),
    interval(start = 6L, end = 8L)
  )
})

test_that("complement isn't affected by empty interval when `force_start` and `force_end` are set", {
  x <- interval(start = 3L, end = 3L)

  expect_identical(
    interval_complement(x, force_start = 1L, force_end = 8L),
    interval(start = 1L, end = 8L)
  )
  expect_identical(
    interval_complement(x, force_start = 3L, force_end = 4L),
    interval(start = 3L, end = 4L)
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

test_that("union with empty interval ignores the empty interval", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))
  y <- interval(start = 5L, end = 5L)

  expect_identical(
    interval_union(x, y),
    interval(start = 1L, end = 3L)
  )
  expect_identical(
    interval_union(y, x),
    interval(start = 1L, end = 3L)
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

test_that("union with empty interval works on either side", {
  x <- interval(start = 1L, end = 2L)
  y <- interval(start = 1L, end = 1L)
  z <- interval(start = 2L, end = 2L)

  expect_identical(
    interval_union(x, y),
    x
  )

  expect_identical(
    interval_union(x, z),
    x
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

test_that("intersect with empty intervals on either side doesn't result in values", {
  x <- interval(start = 1L, end = 2L)
  y <- interval(start = 1L, end = 1L)
  z <- interval(start = 2L, end = 2L)

  expect_identical(
    interval_intersect(x, y),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_intersect(y, x),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_intersect(x, z),
    interval(start = integer(), end = integer())
  )
  expect_identical(
    interval_intersect(z, x),
    interval(start = integer(), end = integer())
  )
})

test_that("intersect with contained empty interval doesn't result in values", {
  x <- interval(start = 1L, end = 3L)
  y <- interval(start = 2L, end = 2L)

  expect_identical(
    interval_intersect(x, y),
    interval(start = integer(), end = integer())
  )

  expect_identical(
    interval_intersect(y, x),
    interval(start = integer(), end = integer())
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

test_that("difference with empty intervals on either side doesn't drop values", {
  x <- interval(start = 1L, end = 2L)
  y <- interval(start = 1L, end = 1L)
  z <- interval(start = 2L, end = 2L)

  expect_identical(
    interval_difference(x, y),
    x
  )

  expect_identical(
    interval_difference(x, z),
    x
  )
})

test_that("difference with contained empty interval doesn't drop values", {
  x <- interval(start = 1L, end = 3L)
  y <- interval(start = 2L, end = 2L)

  expect_identical(
    interval_difference(x, y),
    x
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

test_that("empty intervals in any location aren't in the union", {
  x <- interval(1, 1)
  y <- interval(4, 5)

  expect_identical(
    interval_parallel_union(x, y),
    y
  )
  expect_identical(
    interval_parallel_union(y, x),
    y
  )

  x <- interval(7, 7)

  expect_identical(
    interval_parallel_union(x, y),
    y
  )
  expect_identical(
    interval_parallel_union(y, x),
    y
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

test_that("can take parallel intersection resulting in empty ranges", {
  x <- interval(start = 1L, end = 4L)
  y <- interval(start = 4L, end = 5L)

  expect_identical(
    interval_parallel_intersect(x, y),
    interval(start = 4L, end = 4L)
  )

  x <- interval(start = 1L, end = 4L)
  y <- interval(start = 5L, end = 6L)

  expect_identical(
    interval_parallel_intersect(x, y),
    interval(start = 5L, end = 4L)
  )
})

test_that("can take parallel intersection of empty ranges", {
  x <- interval(start = 1L, end = 1L)
  y <- interval(start = 2L, end = 2L)

  # Results in another empty range
  expect_identical(
    interval_parallel_intersect(x, y),
    interval(start = 1L, end = 1L)
  )
})

test_that("can take parallel intersection when empty range is contained in another range", {
  x <- interval(start = 1L, end = 1L)
  y <- interval(start = 0L, end = 3L)

  # There is no intersection between these, so we get an empty range
  expect_identical(
    interval_parallel_intersect(x, y),
    interval(start = 1L, end = 1L)
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

test_that("can parallel difference with self to get empty set", {
  expect_identical(
    interval_parallel_difference(interval(1, 3), interval(1, 3)),
    interval(1, 1)
  )
})

test_that("throws error when `y` is contained within `x`", {
  expect_snapshot(
    (expect_error(interval_parallel_difference(interval(1, 4), interval(2, 3))))
  )
})

test_that("doesn't throw error when `x` is contained within `y`", {
  expect_identical(
    interval_parallel_difference(interval(2, 3), interval(1, 4)),
    interval(2, 2)
  )
})

test_that("works with empty set on either side", {
  expect_identical(
    interval_parallel_difference(interval(1, 4), interval(2, 2)),
    interval(1, 4)
  )

  expect_identical(
    interval_parallel_difference(interval(2, 2), interval(1, 4)),
    interval(2, 2)
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

test_that("can parallel complement with self to get the empty set", {
  expect_identical(
    interval_parallel_complement(interval(1, 2), interval(1, 2)),
    interval(1, 1)
  )
})

test_that("can parallel complement with empty intervals", {
  expect_identical(
    interval_parallel_complement(interval(1, 2), interval(5, 5)),
    interval(1, 1)
  )
})

