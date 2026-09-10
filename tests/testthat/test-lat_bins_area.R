test_that("lat_bins_area works", {
  # default is to output 12 rows
  expect_snapshot(lat_bins_area())

  expect_snapshot(lat_bins_area(n_bins = 6))
  expect_snapshot(lat_bins_area(n_bins = 6, min = 0, max = 90))
})

test_that("different radius affects the results", {
  orig <- lat_bins_area(n_bins = 6)
  new <- lat_bins_area(n_bins = 6, r = 6000)
  expect_all_false(orig$area == new$area)
})

test_that("lat_bins_area errors if min == max", {
  expect_snapshot(
    lat_bins_area(min = 90, max = 90),
    error = TRUE
  )
})

test_that("lat_bins_area errors with unnamed args", {
  expect_snapshot(lat_bins_area(10, 1), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = 10, 1), error = TRUE)
  expect_snapshot(lat_bins_area(10, 1, 2), error = TRUE)
  expect_snapshot(lat_bins_area(10, 1, max = 2), error = TRUE)
})

test_that("partial matching of argument names is forbidden", {
  expect_snapshot(lat_bins_area(10, mi = 1), error = TRUE)
  expect_snapshot(lat_bins_area(10, mi = 1, ma = 2), error = TRUE)
})

test_that("lat_bins_area errors with wrong inputs", {
  expect_snapshot(lat_bins_area(n_bins = "10"), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = -1), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = 3.5), error = TRUE)

  expect_snapshot(lat_bins_area(max = 100), error = TRUE)
  expect_snapshot(lat_bins_area(max = numeric(0)), error = TRUE)

  expect_snapshot(lat_bins_area(min = 100), error = TRUE)
  expect_snapshot(lat_bins_area(min = numeric(0)), error = TRUE)

  expect_snapshot(lat_bins_area(min = 90, max = -90), error = TRUE)

  expect_snapshot(lat_bins_area(plot = "TRUE"), error = TRUE)
  expect_snapshot(lat_bins_area(plot = logical(0)), error = TRUE)

  expect_snapshot(lat_bins_area(r = "Earth"), error = TRUE)
  expect_snapshot(lat_bins_area(r = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_area(r = -1), error = TRUE)
})

test_that("lat_bins_area plotting works", {
  expect_doppelganger("lat_bins_area", function() {
    lat_bins_area(n_bins = 12, plot = TRUE)
  })
})

test_that("n is deprecated but still works", {
  # deprecated
  expect_snapshot(lat_bins_area(n = 6))

  # still works
  expect_equal(lat_bins_area(n = 6), lat_bins_area(n_bins = 6))

  # still input checking
  expect_snapshot(lat_bins_area(n = "6"), error = TRUE)

  # can't specify both n and n_bins
  expect_snapshot(lat_bins_area(n_bins = 6, n = 6), error = TRUE)
})
