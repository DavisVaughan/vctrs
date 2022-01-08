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
