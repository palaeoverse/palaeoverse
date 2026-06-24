test_that("basic behavior works", {
  bins <- data.frame(
    bin = 1:9,
    min = seq(from = 40, to = -40, by = -10),
    mid = seq(from = 45, to = -35, by = -10),
    max = seq(from = 50, to = -30, by = -10)
  )

  taxdf <- data.frame(
    name = c("A", "B", "C"),
    max_lat = c(20, 50, -10),
    min_lat = c(-10, 20, -40)
  )

  # Example: "B" has min_lat = 20 and max_lat = 50 so it fits in bins 1, 2, and 3
  expect_equal(
    tax_expand_lat(taxdf = taxdf, bins = bins),
    data.frame(
      name = rep(c("B", "A", "C"), each = 3),
      max_lat = rep(c(50, 20, -10), each = 3),
      min_lat = rep(c(20, -10, -40), each = 3),
      bin = 1:9,
      min = seq(40, -40, by = -10),
      mid = seq(45, -35, by = -10),
      max = seq(50, -30, by = -10)
    )
  )

  # input checks
  expect_snapshot(tax_expand_lat(taxdf = 5), error = TRUE)
  expect_snapshot(tax_expand_lat(taxdf), error = TRUE)
  expect_snapshot(tax_expand_lat(taxdf, bins = 1), error = TRUE)
  expect_snapshot(
    tax_expand_lat(taxdf, bins = bins, max_lat = "lat"),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_lat(taxdf, bins = bins, min_lat = "lat"),
    error = TRUE
  )

  # min-max of latitude must be between -90 and 90
  expect_snapshot(
    tax_expand_lat(
      taxdf = data.frame(
        name = c("A", "B", "C"),
        max_lat = c(92, 20, -10),
        min_lat = c(20, -40, -60)
      ),
      bins = bins
    ),
    error = TRUE
  )

  expect_snapshot(
    tax_expand_lat(
      taxdf = data.frame(
        name = c("A", "B", "C"),
        max_lat = c(60, 20, -10),
        min_lat = c(-92, -40, -60)
      ),
      bins = bins
    ),
    error = TRUE
  )

  # wrong column types
  expect_snapshot(
    tax_expand_lat(
      taxdf = data.frame(
        name = c("A", "B", "C"),
        max_lat = c("60", "20", "-10"),
        min_lat = c(-92, -40, -60)
      ),
      bins = bins
    ),
    error = TRUE
  )

  expect_snapshot(
    tax_expand_lat(
      taxdf = data.frame(
        name = c("A", "B", "C"),
        max_lat = c(60, 20, -10),
        min_lat = c("20", -40, -60)
      ),
      bins = bins
    ),
    error = TRUE
  )

  # can't have min latitude > max latitude
  expect_snapshot(
    tax_expand_lat(
      taxdf = data.frame(
        name = c("A", "B", "C"),
        max_lat = c(60, 20, -10),
        min_lat = c(72, -40, -60)
      ),
      bins = bins
    ),
    error = TRUE
  )

  # must have unique rows
  expect_snapshot(
    tax_expand_lat(
      taxdf = data.frame(
        name = c("A", "A", "C"),
        max_lat = c(60, 60, -10),
        min_lat = c(20, 20, -60)
      ),
      bins = bins
    ),
    error = TRUE
  )

  # TODO: should we error on this? Doesn't make sense to have the same name with
  # different lat/lon
  # taxdf <- data.frame(
  #   name = c("A", "A", "C"),
  #   max_lat = c(60, 50, -10),
  #   min_lat = c(20, 40, -60)
  # )
  # expect_snapshot(tax_expand_lat(taxdf = taxdf, bins = bins), error = TRUE)

  # missing column
  taxdf <- data.frame(
    name = c("A", "B", "C"),
    max_lat = c(60, 20, -10),
    min_lat = c(20, -40, -60)
  )
  bins <- bins[, -1]
  expect_snapshot(tax_expand_lat(taxdf = taxdf, bins = bins), error = TRUE)
})

test_that("args 'min_lat' and 'max_lat' work", {
  bins <- data.frame(
    bin = 1:9,
    min = seq(from = 40, to = -40, by = -10),
    mid = seq(from = 45, to = -35, by = -10),
    max = seq(from = 50, to = -30, by = -10)
  )

  taxdf <- data.frame(
    name = c("A", "B", "C"),
    foo_max = c(20, 50, -10),
    foo_min = c(-10, 20, -40)
  )

  expect_snapshot(tax_expand_lat(taxdf = taxdf, bins = bins), error = TRUE)

  # Example: "B" has min_lat = 20 and max_lat = 50 so it fits in bins 1, 2, and 3
  expect_equal(
    tax_expand_lat(
      taxdf = taxdf,
      bins = bins,
      max_lat = "foo_max",
      min_lat = "foo_min"
    ),
    data.frame(
      name = rep(c("B", "A", "C"), each = 3),
      foo_max = rep(c(50, 20, -10), each = 3),
      foo_min = rep(c(20, -10, -40), each = 3),
      bin = 1:9,
      min = seq(40, -40, by = -10),
      mid = seq(45, -35, by = -10),
      max = seq(50, -30, by = -10)
    )
  )
})
