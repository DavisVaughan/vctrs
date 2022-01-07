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
