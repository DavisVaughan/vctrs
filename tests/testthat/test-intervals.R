# ------------------------------------------------------------------------------
# vec_locate_interval_merge_bounds()

test_that("can compute merge bounds", {
  x <- data_frame(
    start = c(1L, 9L,  2L, 2L, 10L),
    end = c(5L, 11L, 6L, 8L, 12L)
  )

  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end),
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )
})

test_that("can locate bounds with size one input", {
  x <- data_frame(start = 1L, end = 2L)

  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end),
    data_frame(start = 1L, end = 1L)
  )
})

test_that("can locate bounds with size zero input", {
  x <- data_frame(start = integer(), end = integer())

  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("missing intervals are retained by default, but can be dropped", {
  x <- data_frame(start = NA, end = NA)

  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end),
    data_frame(start = NA_integer_, end = NA_integer_)
  )

  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end, missing = "drop"),
    data_frame(start = integer(), end = integer())
  )
})

test_that("missing intervals don't affect the result if dropped", {
  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))
  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end, missing = "drop"),
    data_frame(start = 3L, end = 1L)
  )
})

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- data_frame(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    vec_locate_interval_merge_bounds(x$start, x$end),
    data_frame(start = c(1L, 3L), end = c(1L, 3L))
  )
})

# ------------------------------------------------------------------------------
# vec_locate_interval_merge_groups()

test_that("can locate merge bounds and groups", {
  x <- data_frame(
    start = c(1L, 9L,  2L, 2L, 10L),
    end = c(5L, 11L, 6L, 8L, 12L)
  )

  out <- vec_locate_interval_merge_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )

  expect_identical(
    out$loc,
    list(c(1L, 3L, 4L), c(2L, 5L))
  )
})

test_that("can locate groups with size one input", {
  expect_identical(
    vec_locate_interval_merge_groups(1L, 2L),
    data_frame(
      key = data_frame(start = 1L, end = 1L),
      loc = list(1L)
    )
  )
})

test_that("can locate groups with size zero input", {
  expect_identical(
    vec_locate_interval_merge_groups(integer(), integer()),
    data_frame(
      key = data_frame(start = integer(), end = integer()),
      loc = list()
    )
  )
})

test_that("locations are ordered by both `start` and `end`", {
  x <- data_frame(start = c(4L, 4L, 1L), end = c(6L, 5L, 2L))

  out <- vec_locate_interval_merge_groups(x$start, x$end)

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

test_that("missing intervals are retained by default", {
  x <- data_frame(start = NA, end = NA)

  out <- vec_locate_interval_merge_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = NA_integer_, end = NA_integer_)
  )
  expect_identical(
    out$loc,
    list(1L)
  )
})

test_that("missing intervals can be dropped", {
  x <- data_frame(start = NA, end = NA)

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "drop")

  expect_identical(
    out$key,
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    out$loc,
    list()
  )
})

test_that("missing intervals can cause an error", {
  expect_snapshot({
    (expect_error(vec_locate_interval_merge_groups(NA, NA, missing = "error")))
    (expect_error(vec_locate_interval_merge_groups(1, NA, missing = "error")))
    (expect_error(vec_locate_interval_merge_groups(NA, 1, missing = "error")))
  })
})

test_that("empty intervals are retained by default", {
  x <- data_frame(start = 1, end = 1)

  out <- vec_locate_interval_merge_groups(x$start, x$end)

  expect_identical(
    out$key,
    data_frame(start = 1L, end = 1L)
  )
  expect_identical(
    out$loc,
    list(1L)
  )
})

test_that("empty intervals can be dropped", {
  x <- data_frame(start = 1, end = 1)

  out <- vec_locate_interval_merge_groups(x$start, x$end, empty = "drop")

  expect_identical(
    out$key,
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    out$loc,
    list()
  )
})

test_that("empty intervals can cause an error", {
  expect_snapshot(
    (expect_error(vec_locate_interval_merge_groups(1, 1, empty = "error")))
  )
})

test_that("missing intervals don't cause an empty interval error", {
  expect_identical(
    vec_locate_interval_merge_groups(NA, NA, empty = "error"),
    vec_locate_interval_merge_groups(NA, NA)
  )
})

test_that("all combinations of `empty` and `missing` work", {
  x <- data_frame(
    start = c(1, NA, 2, 1, 7, NA, 9),
    end = c(1, NA, 3, 5, 8, NA, 9)
  )

  expect_identical(
    vec_locate_interval_merge_groups(x$start, x$end, empty = "drop", missing = "drop"),
    data_frame(
      key = data_frame(start = c(4L, 5L), end = c(4L, 5L)),
      loc = list(c(4L, 3L), 5L)
    )
  )

  expect_identical(
    vec_locate_interval_merge_groups(x$start, x$end, empty = "overlap", missing = "drop"),
    data_frame(
      key = data_frame(start = c(1L, 5L, 7L), end = c(4L, 5L, 7L)),
      loc = list(c(1L, 4L, 3L), 5L, 7L)
    )
  )

  expect_identical(
    vec_locate_interval_merge_groups(x$start, x$end, empty = "drop", missing = "overlap"),
    data_frame(
      key = data_frame(start = c(4L, 5L, NA), end = c(4L, 5L, NA)),
      loc = list(c(4L, 3L), 5L, c(2L, 6L))
    )
  )

  expect_identical(
    vec_locate_interval_merge_groups(x$start, x$end, empty = "overlap", missing = "overlap"),
    data_frame(
      key = data_frame(start = c(1L, 5L, 7L, NA), end = c(4L, 5L, 7L, NA)),
      loc = list(c(1L, 4L, 3L), 5L, 7L, c(2L, 6L))
    )
  )
})

test_that("missing intervals don't affect the result if dropped", {
  x <- data_frame(start = c(3, NA, 2, NA), end = c(5, NA, 3, NA))

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "drop")

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

  out <- vec_locate_interval_merge_groups(x$start, x$end)

  expect_identical(out$key, data_frame(start = c(3L, NA), end = c(3L, NA)))
  expect_identical(out$loc, list(3L, c(2L, 1L)))

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "drop")

  expect_identical(out$key, data_frame(start = 3L, end = 3L))
  expect_identical(out$loc, list(3L))
})

test_that("`missing = 'overlap'` works without any missings", {
  x <- data_frame(start = c(1, 3), end = c(3, 5))

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "overlap")

  expect_identical(out$key, data_frame(start = 1L, end = 2L))
  expect_identical(out$loc, list(c(1L, 2L)))
})

test_that("`missing = 'overlap'` recognizes incomplete rows in data frames", {
  start <- data_frame(year = c(2019, NA, 2019, 2019, 2019), month = c(12, 11, NA, 12, 12))
  end <- data_frame(year = c(2020, 2020, 2020, NA, 2020), month = c(2, 11, 11, 11, NA))
  x <- data_frame(start = start, end = end)

  out <- vec_locate_interval_merge_groups(x$start, x$end)

  expect_identical(out$key, data_frame(start = c(1L, NA), end = c(1L, NA)))
  expect_identical(out$loc, list(1L, 2:5))

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "drop")

  expect_identical(out$key, data_frame(start = 1L, end = 1L))
  expect_identical(out$loc, list(1L))
})

test_that("works on various types", {
  x <- data_frame(start = c(1.5, 2, 3.1, 1.2), end = c(1.7, 3.2, 4.5, NA))

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "drop")

  expect_identical(out$key, data_frame(start = c(1L, 2L), end = c(1L, 3L)))
  expect_identical(out$loc, list(1L, 2:3))

  x <- data_frame(start = c("a", "c", "f", NA), end = c("b", "g", "h", "l"))

  out <- vec_locate_interval_merge_groups(x$start, x$end, missing = "drop")

  expect_identical(out$key, data_frame(start = c(1L, 2L), end = c(1L, 3L)))
  expect_identical(out$loc, list(1L, 2:3))
})

test_that("can keep abutting intervals separate", {
  # after
  x <- data_frame(start = c(1L, 2L, 0L), end = c(2L, 3L, 2L))

  out <- vec_locate_interval_merge_groups(x$start, x$end, abutting = FALSE)

  expect_identical(out$key, data_frame(start = c(3L, 2L), end = c(3L, 2L)))
  expect_identical(out$loc, list(c(3L, 1L), 2L))

  # before
  x <- data_frame(start = c(1L, 0L), end = c(2L, 1L))

  out <- vec_locate_interval_merge_groups(x$start, x$end, abutting = FALSE)

  expect_identical(out$key, data_frame(start = c(2L, 1L), end = c(2L, 1L)))
  expect_identical(out$loc, list(2L, 1L))

  # both
  x <- data_frame(start = c(1L, 0L, 2L), end = c(2L, 1L, 3L))

  out <- vec_locate_interval_merge_groups(x$start, x$end, abutting = FALSE)

  expect_identical(out$key, data_frame(start = c(2L, 1L, 3L), end = c(2L, 1L, 3L)))
  expect_identical(out$loc, list(2L, 1L, 3L))
})

test_that("can keep abutting empty intervals separate", {
  x <- data_frame(start = c(1L, 2L, 2L), end = c(2L, 2L, 3L))

  out <- vec_locate_interval_merge_groups(x$start, x$end, empty = "overlap", abutting = FALSE)

  expect_identical(out$key, data_frame(start = 1:3, end = 1:3))
  expect_identical(out$loc, list(1L, 2L, 3L))
})

test_that("repeated empty intervals are in different groups if `abutting = FALSE`", {
  # Because [1, 1) abuts but does not overlap [1, 1)
  x <- data_frame(start = c(1L, 1L, 1L, 1L), end = c(1L, 1L, 1L, 5L))

  out <- vec_locate_interval_merge_groups(x$start, x$end, empty = "overlap", abutting = FALSE)

  expect_identical(out$key, data_frame(start = 1:4, end = 1:4))
  expect_identical(out$loc, list(1L, 2L, 3L, 4L))
})

test_that("can't have `start > end`", {
  x <- data_frame(start = 1L, end = 0L)
  expect_snapshot((expect_error(vec_locate_interval_merge_groups(x$start, x$end))))
})

test_that("common type is taken", {
  expect_snapshot((expect_error(vec_locate_interval_merge_groups(1, "x"))))
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
