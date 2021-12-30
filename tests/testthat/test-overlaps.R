test_that("can merge overlaps", {
  expect_identical(
    vec_merge_overlaps(
      c(1L, 10L,  2L, 2L, 9L),
      c(5L, 12L, 6L, 8L, 10L)
    ),
    data_frame(start = c(1L, 9L), end = c(8L, 12L))
  )
})

test_that("can merge overlaps and append locations", {
  out <- vec_merge_overlaps(
    c(1L, 9L,  2L, 2L, 10L),
    c(5L, 10L, 6L, 8L, 12L),
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
