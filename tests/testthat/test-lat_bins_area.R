test_that("lat_bins_area works", {
  # default is to output 12 rows
  expect_snapshot(lat_bins_area())

  expect_snapshot(lat_bins_area(n = 6))
  expect_snapshot(lat_bins_area(n = 6, min = 0, max = 90))
})

test_that("different radius affects the results", {
  orig <- lat_bins_area(n = 6)
  new <- lat_bins_area(n = 6, r = 6000)
  expect_all_false(orig$area == new$area)
})

# TODO: what should happen here? Currently it returns area = 0 and area_prop = NaN
# test_that("lat_bins_area handles case where min == max", {
#   lat_bins_area(min = 90, max = 90)
# })

test_that("lat_bins_area errors with wrong inputs", {
  expect_snapshot(lat_bins_area(n = "10"), error = TRUE)
  expect_snapshot(lat_bins_area(n = -1), error = TRUE)
  expect_snapshot(lat_bins_area(n = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_area(n = 3.5), error = TRUE)

  expect_snapshot(lat_bins_area(max = 100), error = TRUE)
  expect_snapshot(lat_bins_area(max = numeric(0)), error = TRUE)

  expect_snapshot(lat_bins_area(min = 100), error = TRUE)
  expect_snapshot(lat_bins_area(min = numeric(0)), error = TRUE)

  expect_snapshot(lat_bins_area(min = 90, max = -90), error = TRUE)

  expect_snapshot(lat_bins_area(plot = "TRUE"), error = TRUE)
  expect_snapshot(lat_bins_area(plot = logical(0)), error = TRUE)

  expect_snapshot(lat_bins_area(r = "Earth"), error = TRUE)
  expect_snapshot(lat_bins_area(r = numeric(0)), error = TRUE)

  # TODO: I suppose this should error? Currently it doesn't
  # expect_snapshot(lat_bins_area(r = -1), error = TRUE)
})

test_that("lat_bins_area plotting works", {
  expect_doppelganger("lat_bins_area", function() {
    lat_bins_area(n = 12, plot = TRUE)
  })
})
