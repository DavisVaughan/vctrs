# ------------------------------------------------------------------------------
# vec_merge_overlaps()

test_that("can merge overlaps", {
  expect_identical(
    vec_merge_overlaps(
      c(1L, 10L,  2L, 2L, 9L),
      c(5L, 12L, 6L, 8L, 11L)
    ),
    data_frame(start = c(1L, 9L), end = c(8L, 12L))
  )
})

test_that("treats intervals as half-open, like `[a, b)`", {
  # [9, 10) doesn't overlap with [10, 12)
  expect_identical(
    vec_merge_overlaps(
      c(10L, 9L),
      c(12L, 10L)
    ),
    data_frame(start = c(9L, 10L), end = c(10L, 12L))
  )
})

test_that("empty intervals are merged if they fall fully within another interval", {
  df <- data_frame(start = c(2L, 1L), end = c(2L, 3L))

  expect_identical(
    vec_merge_overlaps(df$start, df$end),
    data_frame(start = 1L, end = 3L)
  )
})

test_that("empty intervals touching another interval on either side are not merged", {
  df <- data_frame(start = c(2L, 2L), end = c(2L, 3L))

  expect_identical(
    vec_merge_overlaps(df$start, df$end),
    data_frame(start = c(2L, 2L), end = c(2L, 3L))
  )

  df <- data_frame(start = c(2L, 1L), end = c(2L, 2L))

  expect_identical(
    vec_merge_overlaps(df$start, df$end),
    data_frame(start = c(1L, 2L), end = c(2L, 2L))
  )
})

test_that("can merge overlaps and append locations", {
  out <- vec_merge_overlaps(
    c(1L, 9L,  2L, 2L, 10L),
    c(5L, 11L, 6L, 8L, 12L),
    locations = TRUE
  )

  expect_identical(
    out$loc,
    list(c(1L, 3L, 4L), c(2L, 5L))
  )

  expect_identical(
    vec_merge_overlaps(2L, 2L, locations = TRUE),
    data_frame(start = 2L, end = 2L, loc = list(1L))
  )
})

test_that("keys are returned ordered", {
  x <- data_frame(start = c(4L, 3L, 1L), end = c(6L, 5L, 2L))

  expect_identical(
    vec_merge_overlaps(x$start, x$end),
    data_frame(start = c(1L, 3L), end = c(2L, 6L))
  )
})

test_that("locations are ordered by both `start` and `end`", {
  x <- data_frame(start = c(4L, 4L, 1L), end = c(6L, 5L, 2L))

  out <- vec_merge_overlaps(x$start, x$end, locations = TRUE)

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

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- data_frame(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    vec_merge_overlaps(x$start, x$end),
    data_frame(start = c(1L, 12L), end = c(10L, 15L))
  )
})

test_that("can merge overlaps with size zero input", {
  expect_identical(
    vec_merge_overlaps(integer(), integer()),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    vec_merge_overlaps(integer(), integer(), locations = TRUE),
    data_frame(start = integer(), end = integer(), loc = list())
  )
})

test_that("can merge overlaps with size one input", {
  expect_identical(
    vec_merge_overlaps(2L, 2L),
    data_frame(start = 2L, end = 2L)
  )

  expect_identical(
    vec_merge_overlaps(2L, 2L, locations = TRUE),
    data_frame(start = 2L, end = 2L, loc = list(1L))
  )
})

# ------------------------------------------------------------------------------
# interval_complement()

test_that("computes the complement", {
  start <- c(6L, 1L, 2L, 12L)
  end <- c(9L, 3L, 4L, 14L)

  expect_identical(
    interval_complement(start, end),
    data_frame(start = c(4L, 9L), end = c(6L, 12L))
  )
})

test_that("treats intervals as half-open like [a, b)", {
  start <- c(1L, 5L)
  end <- c(4L, 6L)

  expect_identical(
    interval_complement(start, end),
    data_frame(start = 4L, end = 5L)
  )
})

test_that("touching intervals like [a, b) [b, c) don't generate gaps", {
  start <- c(1L, 5L)
  end <- c(5L, 6L)

  expect_identical(
    interval_complement(start, end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `force_start >= force_end`", {
  start <- c(1L, 2L, 12L)
  end <- c(10L, 5L, 15L)

  expect_identical(
    interval_complement(start, end, force_start = 10L, force_end = 9L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(start, end, force_start = 10L, force_end = 10L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `force_start >= force_end` before any values", {
  start <- c(1L, 2L, 12L)
  end <- c(10L, 5L, 15L)

  expect_identical(
    interval_complement(start, end, force_start = -1L, force_end = -3L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(start, end, force_start = -1L, force_end = -1L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `force_start >= force_end` after any values", {
  start <- c(1L, 2L, 12L)
  end <- c(10L, 5L, 15L)

  expect_identical(
    interval_complement(start, end, force_start = 20L, force_end = 18L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(start, end, force_start = 20L, force_end = 20L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("works with `force_start` before any values", {
  start <- c(2L, 1L, 12L)
  end <- c(5L, 10L, 15L)

  expect_identical(
    interval_complement(start, end, force_start = -1L),
    data_frame(start = c(-1L, 10L), end = c(1L, 12L))
  )
})

test_that("works if both `force_start` and `force_end` are before any values", {
  start <- c(2L, 1L, 12L)
  end <- c(5L, 10L, 15L)

  expect_identical(
    interval_complement(start, end, force_start = -5L, force_end = -2L),
    data_frame(start = -5L, end = -2L)
  )
})

test_that("works with `force_end` after any values", {
  start <- c(2L, 1L, 13L, 12L)
  end <- c(5L, 10L, 17L, 15L)

  expect_identical(
    interval_complement(start, end, force_end = 20L),
    data_frame(start = c(10L, 17L), end = c(12L, 20L))
  )
})

test_that("works if both `force_start` and `force_end` are after any values", {
  start <- c(2L, 1L, 12L)
  end <- c(5L, 10L, 15L)

  expect_identical(
    interval_complement(start, end, force_start = 17L, force_end = 19L),
    data_frame(start = 17L, end = 19L)
  )
})

test_that("works with `force_start` that is on the max set value", {
  start <- c(1L, 12L)
  end <- c(9L, 13L)

  expect_identical(
    interval_complement(start, end, force_start = 9L),
    data_frame(start = 9L, end = 12L)
  )
})

test_that("works with `force_end` that is on the max set value", {
  start <- c(1L, 2L, 12L)
  end <- c(10L, 5L, 15L)

  expect_identical(
    interval_complement(start, end, force_end = 10L),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(start, end, force_start = 10L, force_end = 10L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("size zero case generally returns nothing", {
  expect_identical(
    interval_complement(integer(), integer()),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(integer(), integer(), force_start = 5L),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    interval_complement(integer(), integer(), force_end = 5L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("size zero case with both `force_start` and `force_end` returns an interval", {
  expect_identical(
    interval_complement(integer(), integer(), force_start = 5L, force_end = 10L),
    data_frame(start = 5L, end = 10L)
  )
})

test_that("size zero case with `force_start >= force_end` doesn't return anything", {
  expect_identical(
    interval_complement(integer(), integer(), force_start = 5L, force_end = 5L),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    interval_complement(integer(), integer(), force_start = 10L, force_end = 5L),
    data_frame(start = integer(), end = integer())
  )
})
