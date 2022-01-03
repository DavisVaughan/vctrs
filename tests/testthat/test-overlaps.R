# ------------------------------------------------------------------------------
# interval_link()

test_that("can link up overlaps", {
  expect_identical(
    interval_link(
      c(1L, 10L,  2L, 2L, 9L),
      c(5L, 12L, 6L, 8L, 11L)
    ),
    data_frame(start = c(1L, 9L), end = c(8L, 12L))
  )
})

test_that("`[a, b)` links with `[b, c)`", {
  expect_identical(
    interval_link(
      c(10L, 9L),
      c(12L, 10L)
    ),
    data_frame(start = 9L, end = 12L)
  )
})

test_that("empty intervals are merged if they fall fully within another interval", {
  df <- data_frame(start = c(2L, 1L), end = c(2L, 3L))

  expect_identical(
    interval_link(df$start, df$end),
    data_frame(start = 1L, end = 3L)
  )
})

test_that("empty intervals touching another interval on either side are linked", {
  df <- data_frame(start = c(2L, 2L), end = c(2L, 3L))

  expect_identical(
    interval_link(df$start, df$end),
    data_frame(start = 2L, end = 3L)
  )

  df <- data_frame(start = c(2L, 1L), end = c(2L, 2L))

  expect_identical(
    interval_link(df$start, df$end),
    data_frame(start = 1L, end = 2L)
  )
})

test_that("keys are returned ordered", {
  x <- data_frame(start = c(4L, 3L, 1L), end = c(6L, 5L, 2L))

  expect_identical(
    interval_link(x$start, x$end),
    data_frame(start = c(1L, 3L), end = c(2L, 6L))
  )
})

test_that("max endpoint is retained even if it isn't the last in the group", {
  # 10 is max end of first group, but 5 is last value in that group
  x <- data_frame(start = c(1L, 2L, 12L), end = c(10L, 5L, 15L))

  expect_identical(
    interval_link(x$start, x$end),
    data_frame(start = c(1L, 12L), end = c(10L, 15L))
  )
})

test_that("can link with size zero input", {
  expect_identical(
    interval_link(integer(), integer()),
    data_frame(start = integer(), end = integer())
  )
})

test_that("empty intervals get dropped", {
  expect_identical(
    interval_link(2L, 2L),
    data_frame(start = integer(), end = integer())
  )
})

test_that("can link with size one input", {
  expect_identical(
    interval_link(1L, 2L),
    data_frame(start = 1L, end = 2L)
  )
})

test_that("can set a maximum gap of `>0` to link intervals with gaps", {
  x <- data_frame(start = c(1L, 3L, 4L), end = c(2L, 4L, 5L))

  expect_identical(
    interval_link(x$start, x$end, gap = 1L),
    data_frame(start = 1L, end = 5L)
  )

  x <- data_frame(start = c(1L, 3L, 6L), end = c(2L, 4L, 7L))

  expect_identical(
    interval_link(x$start, x$end, gap = 1L),
    data_frame(start = c(1L, 6L), end = c(4L, 7L))
  )
})

test_that("`gap` must be 0 or positive", {
  expect_snapshot((expect_error(interval_link(1L, 2L, gap = -1L))))
})

# ------------------------------------------------------------------------------
# interval_locate_links()

test_that("can compute link locations", {
  out <- interval_locate_links(
    c(1L, 9L,  2L, 2L, 10L),
    c(5L, 11L, 6L, 8L, 12L)
  )

  expect_identical(
    out,
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )
})

test_that("can link with size one input", {
  expect_identical(
    interval_locate_links(1L, 2L),
    data_frame(start = 1L, end = 1L)
  )
})

test_that("can link with size zero input", {
  expect_identical(
    interval_locate_links(integer(), integer()),
    data_frame(start = integer(), end = integer())
  )
})

# ------------------------------------------------------------------------------
# interval_locate_link_groups()

test_that("can compute link locations and groups", {
  out <- interval_locate_link_groups(
    c(1L, 9L,  2L, 2L, 10L),
    c(5L, 11L, 6L, 8L, 12L)
  )

  expect_identical(
    out$key,
    data_frame(start = c(1L, 2L), end = c(4L, 5L))
  )

  expect_identical(
    out$loc,
    list(c(1L, 3L, 4L), c(2L, 5L))
  )
})

test_that("can link with size one input", {
  expect_identical(
    interval_locate_link_groups(1L, 2L),
    data_frame(
      key = data_frame(start = 1L, end = 1L),
      loc = list(1L)
    )
  )
})

test_that("can link with size zero input", {
  expect_identical(
    interval_locate_link_groups(integer(), integer()),
    data_frame(
      key = data_frame(start = integer(), end = integer()),
      loc = list()
    )
  )
})

test_that("locations are ordered by both `start` and `end`", {
  x <- data_frame(start = c(4L, 4L, 1L), end = c(6L, 5L, 2L))

  out <- interval_locate_link_groups(x$start, x$end)

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

test_that("`[a, b)` and `[b, c)` result in no complement values", {
  start <- c(1L, 5L)
  end <- c(5L, 6L)

  expect_identical(
    interval_complement(start, end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("complement is invertible", {
  start <- c(1L, 5L)
  end <- c(5L, 6L)

  force_start <- min(start)
  force_end <- max(end)

  # Should always be seen as invertible as long as linking is done first
  x <- interval_link(start, end)

  x_c <- interval_complement(x$start, x$end, force_start = force_start, force_end = force_end)
  x2 <- interval_complement(x_c$start, x_c$end, force_start = force_start, force_end = force_end)

  expect_identical(x, x2)
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

test_that("complement of empty interval is correct", {
  x <- data_frame(start = 5L, end = 5L)

  expect_identical(
    interval_complement(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )

  x <- data_frame(start = c(5L, 5L), end = c(5L, 5L))

  expect_identical(
    interval_complement(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("complement isn't affected by contained empty interval", {
  x <- data_frame(start = c(1L, 3L), end = c(5L, 3L))

  expect_identical(
    interval_complement(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("complement isn't affected by empty interval", {
  x <- data_frame(start = c(1L, 7L), end = c(5L, 7L))

  expect_identical(
    interval_complement(x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("complement isn't affected by empty interval when `force_start` is set", {
  x <- data_frame(start = 3L, end = 3L)

  expect_identical(
    interval_complement(x$start, x$end, force_start = 1L),
    data_frame(start = integer(), end = integer())
  )

  x <- data_frame(start = c(3L, 5L), end = c(3L, 6L))

  expect_identical(
    interval_complement(x$start, x$end, force_start = 1L),
    data_frame(start = 1L, end = 5L)
  )
})

test_that("complement isn't affected by empty interval when `force_end` is set", {
  x <- data_frame(start = 3L, end = 3L)

  expect_identical(
    interval_complement(x$start, x$end, force_end = 8L),
    data_frame(start = integer(), end = integer())
  )

  x <- data_frame(start = c(3L, 5L), end = c(3L, 6L))

  expect_identical(
    interval_complement(x$start, x$end, force_end = 8L),
    data_frame(start = 6L, end = 8L)
  )
})

test_that("complement isn't affected by empty interval when `force_start` and `force_end` are set", {
  x <- data_frame(start = 3L, end = 3L)

  expect_identical(
    interval_complement(x$start, x$end, force_start = 1L, force_end = 8L),
    data_frame(start = 1L, end = 8L)
  )
  expect_identical(
    interval_complement(x$start, x$end, force_start = 3L, force_end = 4L),
    data_frame(start = 3L, end = 4L)
  )
})

# ------------------------------------------------------------------------------
# interval_union()

test_that("union links", {
  x <- data_frame(start = c(1L, 2L), end = c(2L, 3L))
  y <- data_frame(start = 5L, end = 6L)

  expect_identical(
    interval_union(x$start, x$end, y$start, y$end),
    data_frame(start = c(1L, 5L), end = c(3L, 6L))
  )
})

test_that("union with empty interval ignores the empty interval", {
  x <- data_frame(start = c(1L, 2L), end = c(2L, 3L))
  y <- data_frame(start = 5L, end = 5L)

  expect_identical(
    interval_union(x$start, x$end, y$start, y$end),
    data_frame(start = 1L, end = 3L)
  )
  expect_identical(
    interval_union(y$start, y$end, x$start, x$end),
    data_frame(start = 1L, end = 3L)
  )
})

test_that("union treats intervals as half open `[a, b)`", {
  x <- data_frame(start = 1L, end = 2L)
  y <- data_frame(start = 3L, end = 5L)

  expect_identical(
    interval_union(x$start, x$end, y$start, y$end),
    data_frame(start = c(1L, 3L), end = c(2L, 5L))
  )
})

test_that("union with empty interval works on either side", {
  x <- data_frame(start = 1L, end = 2L)
  y <- data_frame(start = 1L, end = 1L)
  z <- data_frame(start = 2L, end = 2L)

  expect_identical(
    interval_union(x$start, x$end, y$start, y$end),
    x
  )

  expect_identical(
    interval_union(x$start, x$end, z$start, z$end),
    x
  )
})

# ------------------------------------------------------------------------------
# interval_intersect()

test_that("intersect links", {
  x <- data_frame(start = c(1L, 2L), end = c(2L, 3L))

  expect_identical(
    interval_intersect(x$start, x$end, x$start, x$end),
    data_frame(start = 1L, end = 3L)
  )
})

test_that("intersect with empty intervals on either side doesn't result in values", {
  x <- data_frame(start = 1L, end = 2L)
  y <- data_frame(start = 1L, end = 1L)
  z <- data_frame(start = 2L, end = 2L)

  expect_identical(
    interval_intersect(x$start, x$end, y$start, y$end),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    interval_intersect(y$start, y$end, x$start, x$end),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    interval_intersect(x$start, x$end, z$start, z$end),
    data_frame(start = integer(), end = integer())
  )
  expect_identical(
    interval_intersect(z$start, z$end, x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

test_that("intersect with contained empty interval doesn't result in values", {
  x <- data_frame(start = 1L, end = 3L)
  y <- data_frame(start = 2L, end = 2L)

  expect_identical(
    interval_intersect(x$start, x$end, y$start, y$end),
    data_frame(start = integer(), end = integer())
  )

  expect_identical(
    interval_intersect(y$start, y$end, x$start, x$end),
    data_frame(start = integer(), end = integer())
  )
})

# ------------------------------------------------------------------------------
# interval_setdiff()

test_that("difference links", {
  x <- data_frame(start = c(1L, 2L), end = c(2L, 3L))
  y <- data_frame(start = integer(), end = integer())

  expect_identical(
    interval_setdiff(x$start, x$end, y$start, y$end),
    data_frame(start = 1L, end = 3L)
  )
})

test_that("difference with empty intervals on either side doesn't drop values", {
  x <- data_frame(start = 1L, end = 2L)
  y <- data_frame(start = 1L, end = 1L)
  z <- data_frame(start = 2L, end = 2L)

  expect_identical(
    interval_setdiff(x$start, x$end, y$start, y$end),
    x
  )

  expect_identical(
    interval_setdiff(x$start, x$end, z$start, z$end),
    x
  )
})

test_that("difference with contained empty interval doesn't drop values", {
  x <- data_frame(start = 1L, end = 3L)
  y <- data_frame(start = 2L, end = 2L)

  expect_identical(
    interval_setdiff(x$start, x$end, y$start, y$end),
    x
  )
})
