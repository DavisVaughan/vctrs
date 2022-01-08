# ------------------------------------------------------------------------------
# vec_vec_interval_locate_minimal()

test_that("can compute minimal locations", {
  x <- data_frame(
    start = c(1L, 9L,  2L, 2L, 10L),
    end = c(5L, 11L, 6L, 8L, 12L)
  )

  expect_identical(
    vec_interval_locate_minimal(x$start, x$end),
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )
})

test_that("can minimize with size one input", {
  x <- data_frame(start = 1L, end = 2L)

  expect_identical(
    vec_interval_locate_minimal(x$start, x$end),
    data_frame(start = 1L, end = 1L)
  )
})

test_that("can minimize with size zero input", {
  x <- data_frame(start = integer(), end = integer())

  expect_identical(
    vec_interval_locate_minimal(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("missing intervals are removed", {
  x <- data_frame(start = NA, end = NA)
  expect_identical(
    vec_interval_locate_minimal(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("missing intervals don't affect the result", {
  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))
  expect_identical(
    vec_interval_locate_minimal(x$start, x$end),
    data_frame(start = 3L, end = 1L)
  )
})

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- data_frame(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    vec_interval_locate_minimal(x$start, x$end),
    data_frame(start = c(1L, 3L), end = c(1L, 3L))
  )
})

# ------------------------------------------------------------------------------
# vec_interval_locate_minimal_groups()

test_that("can compute minimal locations and groups", {
  x <- data_frame(
    start = c(1L, 9L,  2L, 2L, 10L),
    end = c(5L, 11L, 6L, 8L, 12L)
  )

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

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
    vec_interval_locate_minimal_groups(1L, 2L),
    data_frame(
      key = data_frame(start = 1L, end = 1L),
      loc = list(1L)
    )
  )
})

test_that("can minimize with size zero input", {
  expect_identical(
    vec_interval_locate_minimal_groups(integer(), integer()),
    data_frame(
      key = data_frame(start = integer(), end = integer()),
      loc = list()
    )
  )
})

test_that("locations are ordered by both `start` and `end`", {
  x <- data_frame(start = c(4L, 4L, 1L), end = c(6L, 5L, 2L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

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
  x <- data_frame(start = NA, end = NA)

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

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
  x <- data_frame(start = NA, end = NA)

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_missing = TRUE)

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
  x <- data_frame(start = 1, end = 1)

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = TRUE)

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
  x <- data_frame(
    start = c(1, NA, 2, 1, 7, NA, 9),
    end = c(1, NA, 3, 5, 8, NA, 9)
  )

  expect_identical(
    vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = FALSE, keep_missing = FALSE),
    data_frame(
      key = data_frame(start = c(4L, 5L), end = c(4L, 5L)),
      loc = list(c(4L, 3L), 5L)
    )
  )

  expect_identical(
    vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = TRUE, keep_missing = FALSE),
    data_frame(
      key = data_frame(start = c(1L, 5L, 7L), end = c(4L, 5L, 7L)),
      loc = list(c(1L, 4L, 3L), 5L, 7L)
    )
  )

  expect_identical(
    vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = FALSE, keep_missing = TRUE),
    data_frame(
      key = data_frame(start = c(4L, 5L, NA), end = c(4L, 5L, NA)),
      loc = list(c(4L, 3L), 5L, c(2L, 6L))
    )
  )

  expect_identical(
    vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = TRUE, keep_missing = TRUE),
    data_frame(
      key = data_frame(start = c(1L, 5L, 7L, NA), end = c(4L, 5L, 7L, NA)),
      loc = list(c(1L, 4L, 3L), 5L, 7L, c(2L, 6L))
    )
  )
})

test_that("missing intervals don't affect the result by default", {
  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

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
  x <- data_frame(start = c(1L, NA, 1L), end = c(NA, 1L, 2L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

  expect_identical(out$key, data_frame(start = 3L, end = 3L))
  expect_identical(out$loc, list(3L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_missing = TRUE)

  expect_identical(out$key, data_frame(start = c(3L, NA), end = c(3L, NA)))
  expect_identical(out$loc, list(3L, c(2L, 1L)))
})

test_that("can set `keep_missing = TRUE` without any missings", {
  x <- data_frame(start = c(1, 3), end = c(3, 5))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_missing = TRUE)

  expect_identical(out$key, data_frame(start = 1L, end = 2L))
  expect_identical(out$loc, list(c(1L, 2L)))
})

test_that("`keep_missing = true` recognizes incomplete rows in data frames", {
  start <- data_frame(year = c(2019, NA, 2019, 2019, 2019), month = c(12, 11, NA, 12, 12))
  end <- data_frame(year = c(2020, 2020, 2020, NA, 2020), month = c(2, 11, 11, 11, NA))
  x <- data_frame(start = start, end = end)

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

  expect_identical(out$key, data_frame(start = 1L, end = 1L))
  expect_identical(out$loc, list(1L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_missing = TRUE)

  expect_identical(out$key, data_frame(start = c(1L, NA), end = c(1L, NA)))
  expect_identical(out$loc, list(1L, 2:5))
})

test_that("works on various types", {
  x <- data_frame(start = c(1.5, 2, 3.1, 1.2), end = c(1.7, 3.2, 4.5, NA))

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

  expect_identical(out$key, data_frame(start = c(1L, 2L), end = c(1L, 3L)))
  expect_identical(out$loc, list(1L, 2:3))

  x <- data_frame(start = c("a", "c", "f", NA), end = c("b", "g", "h", "l"))

  out <- vec_interval_locate_minimal_groups(x$start, x$end)

  expect_identical(out$key, data_frame(start = c(1L, 2L), end = c(1L, 3L)))
  expect_identical(out$loc, list(1L, 2:3))
})

test_that("can't have `start > end`", {
  x <- data_frame(start = 1L, end = 0L)
  expect_snapshot((expect_error(vec_interval_locate_minimal_groups(x$start, x$end))))
})

test_that("common type is taken", {
  expect_snapshot((expect_error(vec_interval_locate_minimal_groups(1, "x"))))
})

# ------------------------------------------------------------------------------
# vec_interval_complement()

test_that("computes the complement", {
  x <- data_frame(
    start = c(6L, 1L, 2L, 12L),
    end = c(9L, 3L, 4L, 14L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end),
    data_frame(start = c(4L, 9L), end = c(6L, 12L))
  )
})

test_that("treats intervals as half-open like [a, b)", {
  x <- data_frame(
    start = c(1L, 5L),
    end = c(4L, 6L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end),
    data_frame(start = 4L, end = 5L)
  )
})

test_that("`[a, b)` and `[b, c)` result in no complement values", {
  x <- data_frame(
    start = c(1L, 5L),
    end = c(5L, 6L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `lower == upper`", {
  x <- data_frame(
    start = c(1L, 2L, 12L, NA),
    end = c(10L, 5L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 10L, upper = 10L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    vec_interval_complement(x$start, x$end, lower = -1L, upper = -1L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 20L, upper = 20L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `lower` before any values", {
  x <- data_frame(
    start = c(1L, 2L, 12L, NA),
    end = c(10L, 5L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = -1L),
    data_frame(start = c(-1L, 10L), end = c(1L, 12L))
  )
})

test_that("works if both `lower` and `upper` are before any values", {
  x <- data_frame(
    start = c(2L, 1L, 12L, NA),
    end = c(5L, 10L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = -5L, upper = -2L),
    data_frame(start = -5L, end = -2L)
  )
})

test_that("works with `upper` after any values", {
  x <- data_frame(
    start = c(2L, 1L, 13L, 12L, NA),
    end = c(5L, 10L, 17L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, upper = 20L),
    data_frame(start = c(10L, 17L), end = c(12L, 20L))
  )
})

test_that("works if both `lower` and `upper` are after any values", {
  x <- data_frame(
    start = c(2L, 1L, 12L, NA),
    end = c(5L, 10L, 15L, NA)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 17L, upper = 19L),
    data_frame(start = 17L, end = 19L)
  )
})

test_that("works with only NA and `lower`", {
  x <- data_frame(start = NA_integer_, end = NA_integer_)
  expect_identical(vec_interval_complement(x$start, x$end, lower = 5L), data_frame(start = integer(), end = integer()))
})

test_that("works with only NA and `upper`", {
  x <- data_frame(start = NA_integer_, end = NA_integer_)
  expect_identical(vec_interval_complement(x$start, x$end, upper = 5L), data_frame(start = integer(), end = integer()))
})

test_that("works with only NA and both `lower` and `upper`", {
  x <- data_frame(start = NA_integer_, end = NA_integer_)
  expect_identical(vec_interval_complement(x$start, x$end, lower = 2L, upper = 5L), data_frame(start = 2L, end = 5L))
})

test_that("works with `lower` that is on the max set value", {
  x <- data_frame(
    start = c(1L, 12L),
    end = c(9L, 13L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 9L),
    data_frame(start = 9L, end = 12L)
  )
})

test_that("works with `upper` that is on the max set value", {
  x <- data_frame(
    start = c(-5L, 1L, 2L, 12L),
    end = c(0L, 10L, 5L, 15L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, upper = 10L),
    data_frame(start = 0L, end = 1L)
  )

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 10L, upper = 10L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("size zero case generally returns nothing", {
  expect_identical(
    vec_interval_complement(integer(), integer()),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    vec_interval_complement(integer(), integer(), lower = 5L),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    vec_interval_complement(integer(), integer(), upper = 5L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("size zero case with both `lower` and `upper` returns an data_frame", {
  expect_identical(
    vec_interval_complement(integer(), integer(), lower = 5L, upper = 10L),
    data_frame(start = 5L, end = 10L)
  )
})

test_that("size zero case with `lower == upper` doesn't return anything", {
  expect_identical(
    vec_interval_complement(integer(), integer(), lower = 5L, upper = 5L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works when `lower` is contained in an data_frame", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 3),
    data_frame(start = 5, end = 10)
  )
})

test_that("works when `lower` is in a gap between intervals", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 7),
    data_frame(start = 7, end = 10)
  )
})

test_that("works when `upper` is in a gap between intervals", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), upper = 7),
    data_frame(start = c(-3, 5), end = c(1, 7))
  )
})

test_that("works when `lower` and `upper` are in a gap between intervals", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 6, upper = 7),
    data_frame(start = 6, end = 7)
  )
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 7, upper = 7),
    data_frame(start = double(), end = double())
  )
})

test_that("works when `lower` and `upper` have an data_frame between them", {
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = 0, upper = 7),
    data_frame(start = c(0, 5), end = c(1, 7))
  )
  expect_identical(
    vec_interval_complement(c(-5, 1, 10), c(-3, 5, 15), lower = -6, upper = 7),
    data_frame(start = c(-6, -3, 5), end = c(-5, 1, 7))
  )
})

test_that("bounds of empty intervals don't affect complement", {
  expect_identical(
    vec_interval_complement(5L, 5L),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    vec_interval_complement(5L, 5L, lower = 1L, upper = 7L),
    data_frame(start = 1L, end = 7L)
  )

  expect_identical(
    vec_interval_complement(start = c(5L, 6L), end = c(5L, 8L)),
    data_frame(start = integer(), end = integer())
  )
})

test_that("allow `lower > upper` which returns an empty data_frame", {
  x <- data_frame(start = c(1, 2), end = c(5, 12))
  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 10, upper = 9),
    data_frame(start = double(), end = double())
  )
})


test_that("complement works when `lower` and `upper` are in the same data_frame", {
  x <- data_frame(start = 1, end = 5)

  expect_identical(
    vec_interval_complement(x$start, x$end, lower = 2, upper = 4),
    data_frame(start = double(), end = double())
  )
})

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
  x <- interval(start = NA, end = NA)
  expect_identical(interval_minimize(x), interval(start = logical(), end = logical()))
})

test_that("missing intervals don't affect the result", {
  x <- interval(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))
  expect_identical(interval_minimize(x), interval(2, 5))
})

# ------------------------------------------------------------------------------
# interval_update_minimal()

test_that("updates values to their minimal interval", {
  x <- interval(start = c(1, 3, 6, 10), end = c(3, 7, 9, 12))

  expect_identical(
    interval_update_minimal(x),
    interval(start = c(1, 1, 1, 10), end = c(9, 9, 9, 12))
  )
})

test_that("abutting and overlapping empty intervals are updated", {
  x <- interval(start = c(1, 1, 3, 10), end = c(1, 5, 3, 12))

  expect_identical(
    interval_update_minimal(x),
    interval(start = c(1, 1, 1, 10), end = c(5, 5, 5, 12))
  )
})

test_that("retains missing intervals", {
  x <- interval(start = c(NA, 1, NA), end = c(NA, 5, NA))

  expect_identical(interval_update_minimal(x), x)
})

test_that("retains unmerged empty intervals", {
  x <- interval(start = c(0, 1, 2, 11), end = c(0, 2, 5, 11))

  expect_identical(
    interval_update_minimal(x),
    interval(start = c(0, 1, 1, 11), end = c(0, 5, 5, 11))
  )
})

# ------------------------------------------------------------------------------
# interval_set_union()

test_that("union links", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))
  y <- interval(start = 5L, end = 6L)

  expect_identical(
    interval_set_union(x, y),
    interval(start = c(1L, 5L), end = c(3L, 6L))
  )
})

test_that("union treats intervals as half open `[a, b)`", {
  x <- interval(start = 1L, end = 2L)
  y <- interval(start = 3L, end = 5L)

  expect_identical(
    interval_set_union(x, y),
    interval(start = c(1L, 3L), end = c(2L, 5L))
  )
})

test_that("union drops NAs", {
  x <- interval(c(1, NA), c(2, NA))
  y <- interval(2, 3)

  expect_identical(
    interval_set_union(x, y),
    interval(1, 3)
  )
})

test_that("union is the union of minimal interval vectors", {
  x <- interval(1, 1)
  y <- interval(2, 2)
  z <- interval(1, 3)

  expect_identical(interval_set_union(x, x), interval(double(), double()))
  expect_identical(interval_set_union(x, y), interval(double(), double()))
  expect_identical(interval_set_union(x, z), z)
})

# ------------------------------------------------------------------------------
# interval_set_intersect()

test_that("intersect links", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))

  expect_identical(
    interval_set_intersect(x, x),
    interval(start = 1L, end = 3L)
  )
})

test_that("intersect works", {
  x <- interval(start = c(1L, 6L), end = c(4L, 8L))
  y <- interval(start = 2L, end = 3L)
  z <- interval(start = 3L, end = 7L)

  expect_identical(
    interval_set_intersect(x, y),
    interval(start = 2L, end = 3L)
  )
  expect_identical(
    interval_set_intersect(x, z),
    interval(start = c(3L, 6L), end = c(4L, 7L))
  )
})

test_that("intersect works with size zero inputs", {
  x <- interval(start = integer(), end = integer())
  expect_identical(interval_set_intersect(x, x), x)
})

test_that("intersect drops NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(c(1, NA), c(4, NA))

  expect_identical(
    interval_set_intersect(x, y),
    interval(1, 2)
  )
})

test_that("intersect is the intersection of minimal interval vectors", {
  x <- interval(1, 5)

  a <- interval(1, 1)
  b <- interval(2, 2)
  c <- interval(5, 5)
  d <- interval(6, 6)
  e <- interval(0, 0)

  empty <- interval(double(), double())

  expect_identical(interval_set_intersect(x, a), empty)
  expect_identical(interval_set_intersect(a, x), empty)

  expect_identical(interval_set_intersect(x, b), empty)
  expect_identical(interval_set_intersect(b, x), empty)

  expect_identical(interval_set_intersect(x, c), empty)
  expect_identical(interval_set_intersect(c, x), empty)

  expect_identical(interval_set_intersect(x, d), empty)
  expect_identical(interval_set_intersect(d, x), empty)

  expect_identical(interval_set_intersect(x, e), empty)
  expect_identical(interval_set_intersect(e, x), empty)

  # Empty interval with itself
  expect_identical(interval_set_intersect(a, a), empty)
  expect_identical(interval_set_intersect(a, a), empty)
})

test_that("takes ptype on early exits", {
  x <- interval(integer(), integer())
  y <- interval(c(1L, 3L), c(3L, 4L))

  expect_identical(interval_set_intersect(x, y), x)
  expect_identical(interval_set_intersect(y, x), x)

  z <- interval(NA_integer_, NA_integer_)

  expect_identical(interval_set_intersect(z, y), x)
  expect_identical(interval_set_intersect(y, z), x)
})

# ------------------------------------------------------------------------------
# interval_set_difference()

test_that("difference links", {
  x <- interval(start = c(1L, 2L), end = c(2L, 3L))
  y <- interval(start = integer(), end = integer())

  expect_identical(
    interval_set_difference(x, y),
    interval(start = 1L, end = 3L)
  )
})

test_that("difference works", {
  x <- interval(start = c(1L, 6L), end = c(4L, 8L))
  y <- interval(start = 2L, end = 3L)
  z <- interval(start = 3L, end = 7L)

  expect_identical(
    interval_set_difference(x, y),
    interval(start = c(1L, 3L, 6L), end = c(2L, 4L, 8L))
  )
  expect_identical(
    interval_set_difference(x, z),
    interval(start = c(1L, 7L), end = c(3L, 8L))
  )
})

test_that("difference works with size zero inputs", {
  x <- interval(start = integer(), end = integer())
  expect_identical(interval_set_difference(x, x), x)
})

test_that("difference drops NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(1, 4)

  expect_identical(
    interval_set_difference(x, y),
    interval(0, 1)
  )
  expect_identical(
    interval_set_difference(y, x),
    interval(2, 4)
  )
})

test_that("difference is the difference of minimal interval vectors", {
  x <- interval(1, 5)

  a <- interval(1, 1)
  b <- interval(2, 2)
  c <- interval(5, 5)
  d <- interval(6, 6)
  e <- interval(0, 0)

  empty <- interval(double(), double())

  expect_identical(interval_set_difference(x, a), x)
  expect_identical(interval_set_difference(a, x), empty)

  expect_identical(interval_set_difference(x, b), x)
  expect_identical(interval_set_difference(b, x), empty)

  expect_identical(interval_set_difference(x, c), x)
  expect_identical(interval_set_difference(c, x), empty)

  expect_identical(interval_set_difference(x, d), x)
  expect_identical(interval_set_difference(d, x), empty)

  expect_identical(interval_set_difference(x, e), x)
  expect_identical(interval_set_difference(e, x), empty)

  # Empty interval with itself
  expect_identical(interval_set_difference(a, a), empty)
  expect_identical(interval_set_difference(a, a), empty)
})

test_that("minimizes on early exits", {
  x <- interval(integer(), integer())
  y <- interval(c(1L, 3L), c(3L, 4L))

  expect_identical(interval_set_difference(x, y), x)
  expect_identical(interval_set_difference(y, x), interval(1L, 4L))

  z <- interval(NA_integer_, NA_integer_)

  expect_identical(interval_set_difference(z, y), x)
  expect_identical(interval_set_difference(y, z), interval(1L, 4L))
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

  x <- interval(1, 1)
  y <- interval(2, 2)

  expect_snapshot((expect_error(interval_parallel_union(x, y))))

  x <- interval(1, 1)
  y <- interval(3, 5)

  expect_snapshot((expect_error(interval_parallel_union(x, y))))
})

test_that("can force gaps to be filled", {
  x <- interval(1, 3)
  y <- interval(4, 5)

  expect_identical(
    interval_parallel_union(x, y, fill = TRUE),
    interval(1, 5)
  )

  x <- interval(1, 1)
  y <- interval(2, 2)

  expect_identical(
    interval_parallel_union(x, y, fill = TRUE),
    interval(1, 2)
  )

  x <- interval(1, 1)
  y <- interval(3, 5)

  expect_identical(
    interval_parallel_union(x, y, fill = TRUE),
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

test_that("union of two empty intervals is allowed", {
  x <- interval(1, 1)

  expect_identical(interval_parallel_union(x, x), x)
})

test_that("union of empty interval and abutting/overlapping interval is allowed", {
  x <- interval(1, 1)
  y <- interval(c(0, 0, 1), c(2, 1, 2))

  expect_identical(interval_parallel_union(x, y), y)
})

