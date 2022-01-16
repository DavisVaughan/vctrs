# ------------------------------------------------------------------------------
# vec_interval_locate_minimal()

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

test_that("can keep abutting intervals separate", {
  # after
  x <- data_frame(start = c(1L, 2L, 0L), end = c(2L, 3L, 2L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_abutting = TRUE)

  expect_identical(out$key, data_frame(start = c(3L, 2L), end = c(3L, 2L)))
  expect_identical(out$loc, list(c(3L, 1L), 2L))

  # before
  x <- data_frame(start = c(1L, 0L), end = c(2L, 1L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_abutting = TRUE)

  expect_identical(out$key, data_frame(start = c(2L, 1L), end = c(2L, 1L)))
  expect_identical(out$loc, list(2L, 1L))

  # both
  x <- data_frame(start = c(1L, 0L, 2L), end = c(2L, 1L, 3L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_abutting = TRUE)

  expect_identical(out$key, data_frame(start = c(2L, 1L, 3L), end = c(2L, 1L, 3L)))
  expect_identical(out$loc, list(2L, 1L, 3L))
})

test_that("can keep abutting empty intervals separate", {
  x <- data_frame(start = c(1L, 2L, 2L), end = c(2L, 2L, 3L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = TRUE, keep_abutting = TRUE)

  expect_identical(out$key, data_frame(start = 1:3, end = 1:3))
  expect_identical(out$loc, list(1L, 2L, 3L))
})

test_that("repeated empty intervals are in different groups if `keep_abutting = TRUE`", {
  # Because [1, 1) abuts but does not overlap [1, 1)
  x <- data_frame(start = c(1L, 1L, 1L, 1L), end = c(1L, 1L, 1L, 5L))

  out <- vec_interval_locate_minimal_groups(x$start, x$end, keep_empty = TRUE, keep_abutting = TRUE)

  expect_identical(out$key, data_frame(start = 1:4, end = 1:4))
  expect_identical(out$loc, list(1L, 2L, 3L, 4L))
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
# interval()

test_that("`start` must be less than `end`", {
  expect_snapshot((expect_error(interval(2, 1))))
  expect_snapshot((expect_error(interval(2, 2))))
})

test_that("incomplete values are propagated", {
  expect_identical(interval(NA, TRUE), interval(NA, NA))
  expect_identical(interval(TRUE, NA), interval(NA, NA))

  # Propagates incompleteness, not missingness!
  # Seen as incomplete even though start is "less" than end
  x <- data_frame(x = 1, y = NA)
  y <- data_frame(x = 2, y = 1)

  expect <- data_frame(x = NA_real_, y = NA_real_)

  expect_identical(interval(x, y), interval(expect, expect))
})

# ------------------------------------------------------------------------------
# interval_complement()

test_that("complement is generic over container", {
  x <- integer_interval(start = c(1, 5), end = c(3, 7))

  expect_identical(
    interval_complement(x),
    integer_interval(start = 3, end = 5)
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

test_that("missing intervals are removed", {
  x <- interval(start = NA, end = NA)
  expect_identical(interval_minimize(x), interval(start = logical(), end = logical()))
})

test_that("missing intervals don't affect the result", {
  x <- interval(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))
  expect_identical(interval_minimize(x), interval(2, 5))
})

test_that("can choose to keep abutting intervals separate", {
  x <- interval(start = c(1, 2, 3), end = c(2, 5, 8))

  expect_identical(
    interval_minimize(x, keep_abutting = TRUE),
    interval(c(1, 2), c(2, 8))
  )
})

test_that("minimize is generic over container", {
  x <- integer_interval(start = c(1, 3), end = c(3, 7))

  expect_identical(
    interval_minimize(x),
    integer_interval(start = 1, end = 7)
  )
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

test_that("retains missing intervals", {
  x <- interval(start = c(NA, 1, NA), end = c(NA, 5, NA))

  expect_identical(interval_update_minimal(x), x)
})

test_that("update is generic over container", {
  x <- integer_interval(start = c(1, 3, 6, 10), end = c(3, 7, 9, 12))

  expect_identical(
    interval_update_minimal(x),
    integer_interval(start = c(1, 1, 1, 10), end = c(9, 9, 9, 12))
  )
})

test_that("can update and keep abutting separate", {
  x <- interval(start = c(1, 2, 3), end = c(2, 5, 8))

  expect_identical(
    interval_update_minimal(x, keep_abutting = TRUE),
    interval(c(1, 2, 2), c(2, 8, 8))
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

test_that("union is generic over container", {
  x <- integer_interval(1, 3)
  y <- integer_interval(2, 5)
  expect_identical(interval_set_union(x, y), integer_interval(1, 5))
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

test_that("takes ptype on early exits", {
  x <- interval(integer(), integer())
  y <- interval(c(1L, 3L), c(3L, 4L))

  expect_identical(interval_set_intersect(x, y), x)
  expect_identical(interval_set_intersect(y, x), x)

  z <- interval(NA_integer_, NA_integer_)

  expect_identical(interval_set_intersect(z, y), x)
  expect_identical(interval_set_intersect(y, z), x)
})

test_that("intersect is generic over container", {
  x <- integer_interval(1, 3)
  y <- integer_interval(2, 3)
  expect_identical(interval_set_intersect(x, y), integer_interval(2, 3))
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

test_that("minimizes on early exits", {
  x <- interval(integer(), integer())
  y <- interval(c(1L, 3L), c(3L, 4L))

  expect_identical(interval_set_difference(x, y), x)
  expect_identical(interval_set_difference(y, x), interval(1L, 4L))

  z <- interval(NA_integer_, NA_integer_)

  expect_identical(interval_set_difference(z, y), x)
  expect_identical(interval_set_difference(y, z), interval(1L, 4L))
})

test_that("difference is generic over container", {
  x <- integer_interval(1, 3)
  y <- integer_interval(2, 3)
  expect_identical(interval_set_difference(x, y), integer_interval(1, 2))
})

# ------------------------------------------------------------------------------
# interval_set_symmetric_difference()

test_that("difference works with size zero inputs", {
  x <- interval(start = integer(), end = integer())
  expect_identical(interval_set_symmetric_difference(x, x), x)
})

test_that("minimizes on early exits", {
  x <- interval(integer(), integer())
  y <- interval(c(1L, 3L), c(3L, 4L))

  expect_identical(interval_set_symmetric_difference(x, y), interval(1L, 4L))
  expect_identical(interval_set_symmetric_difference(y, x), interval(1L, 4L))

  z <- interval(NA_integer_, NA_integer_)

  expect_identical(interval_set_symmetric_difference(z, y), interval(1L, 4L))
  expect_identical(interval_set_symmetric_difference(y, z), interval(1L, 4L))
})

test_that("symmetric difference performs xor", {
  x <- interval(c(3, 0, 7), c(8, 2, 10))
  y <- interval(c(1, 8), c(5, 12))

  expect_identical(
    interval_set_symmetric_difference(x, y),
    interval(c(0, 2, 5, 10), c(1, 3, 8, 12))
  )
})

test_that("symmetric difference drops NAs", {
  x <- interval(c(0, NA), c(2, NA))
  y <- interval(1, 4)

  expect_identical(
    interval_set_symmetric_difference(x, y),
    interval(c(0, 2), c(1, 4))
  )
  expect_identical(
    interval_set_symmetric_difference(y, x),
    interval(c(0, 2), c(1, 4))
  )
})

test_that("symmetric difference is generic over container", {
  x <- integer_interval(1, 3)
  y <- integer_interval(2, 5)
  expect_identical(interval_set_symmetric_difference(x, y), integer_interval(c(1, 3), c(2, 5)))
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

  expect_snapshot((expect_error(interval_parallel_union(y, x))))
})

test_that("can force gaps to be filled", {
  x <- interval(1, 3)
  y <- interval(4, 5)

  expect_identical(
    interval_parallel_union(x, y, fill = TRUE),
    interval(1, 5)
  )

  expect_identical(
    interval_parallel_union(y, x, fill = TRUE),
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

test_that("parallel union is generic over container", {
  x <- integer_interval(1, 2)
  y <- integer_interval(2, 3)
  expect_identical(interval_parallel_union(x, y), integer_interval(1, 3))
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

test_that("parallel intersection between non-overlapping intervals errors", {
  x <- interval(start = 1L, end = 4L)

  y <- interval(start = 5L, end = 6L)

  expect_snapshot(
    (expect_error(interval_parallel_intersect(x, y)))
  )

  y <- interval(start = -1L, end = 0L)

  expect_snapshot(
    (expect_error(interval_parallel_intersect(x, y)))
  )

  y <- interval(start = 4L, end = 5L)

  expect_snapshot(
    (expect_error(interval_parallel_intersect(x, y)))
  )

  y <- interval(start = 0L, end = 1L)

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

test_that("parallel intersect is generic over container", {
  x <- integer_interval(1, 3)
  y <- integer_interval(2, 3)
  expect_identical(interval_parallel_intersect(x, y), integer_interval(2, 3))
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

test_that("parallel complement of interval with itself is not allowed", {
  x <- interval(1, 2)
  expect_snapshot((expect_error(interval_parallel_complement(x, x))))
})

test_that("parallel complement of abutting intervals is not allowed", {
  x <- interval(1, 2)

  y <- interval(0, 1)
  expect_snapshot((expect_error(interval_parallel_complement(x, y))))

  y <- interval(2, 3)
  expect_snapshot((expect_error(interval_parallel_complement(x, y))))
})

test_that("parallel complement of overlapping intervals is not allowed", {
  x <- interval(1, 3)

  expect_snapshot(
    (expect_error(interval_parallel_complement(x, x)))
  )

  y <- interval(0, 4)

  expect_snapshot({
    (expect_error(interval_parallel_complement(x, y)))
    (expect_error(interval_parallel_complement(y, x)))
  })

  y <- interval(2, 4)

  expect_snapshot({
    (expect_error(interval_parallel_complement(x, y)))
    (expect_error(interval_parallel_complement(y, x)))
  })
})

test_that("parallel complement is generic over container", {
  x <- integer_interval(1, 3)
  y <- integer_interval(-1, 0)
  expect_identical(interval_parallel_complement(x, y), integer_interval(0, 1))
})

# ------------------------------------------------------------------------------
# interval_parallel_difference()

test_that("can parallel difference from all sides of `x`", {
  x <- interval(1, 10)

  y <- interval(-1, 0)
  expect_identical(
    interval_parallel_difference(x, y),
    interval(1, 10)
  )

  y <- interval(-1, 1)
  expect_identical(
    interval_parallel_difference(x, y),
    interval(1, 10)
  )

  y <- interval(1, 3)
  expect_identical(
    interval_parallel_difference(x, y),
    interval(3, 10)
  )

  y <- interval(7, 10)
  expect_identical(
    interval_parallel_difference(x, y),
    interval(1, 7)
  )

  y <- interval(10, 12)
  expect_identical(
    interval_parallel_difference(x, y),
    interval(1, 10)
  )

  y <- interval(11, 12)
  expect_identical(
    interval_parallel_difference(x, y),
    interval(1, 10)
  )
})

test_that("parallel difference between interval and itself is not allowed", {
  x <- interval(1, 3)
  expect_snapshot((expect_error(interval_parallel_difference(x, x))))
})

test_that("throws error when `y` is contained within `x`", {
  x <- interval(1, 4)
  y <- interval(2, 3)

  expect_snapshot((expect_error(interval_parallel_difference(x, y))))
})

test_that("throws error when `y` contains `x`", {
  x <- interval(2, 3)
  y <- interval(1, 4)

  expect_snapshot((expect_error(interval_parallel_difference(x, y))))
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
# vec_within()

test_that("works with vectors", {
  x <- 3
  y <- interval(c(0, 1), c(5, 2))

  expect_identical(vec_within(x, y), c(TRUE, FALSE))
})

test_that("propagates incomparable", {
  x <- 2
  y <- interval(c(NA, 1), c(NA, 3))

  expect_identical(vec_within(x, y), c(NA, TRUE))

  x <- NA

  expect_identical(vec_within(x, y), c(NA, NA))
})

test_that("uses `[, )` conditions for containment", {
  x <- interval(1, 2)

  expect_identical(vec_within(1, x), TRUE)
  expect_identical(vec_within(2, x), FALSE)
})

test_that("works with empty inputs", {
  x <- integer()
  y <- interval(integer(), integer())

  expect_identical(vec_within(x, y), logical())
})

test_that("takes the common type with interval fields", {
  x <- "x"
  y <- interval(c(0, 1), c(5, 2))

  expect_snapshot((expect_error(vec_within(x, y))))
})
