test_that("lat_bins_area works", {
  # Expect equal
  expect_equal(nrow(lat_bins_area(n = 12)), 12)
  expect_equal(nrow(lat_bins_area(n = 6, min = 0, max = 90)), 6)

  # Expect error
  expect_snapshot(lat_bins_area(n = "10"), error = TRUE)
  expect_snapshot(lat_bins_area(max = 100), error = TRUE)
  expect_snapshot(lat_bins_area(min = 90, max = -90), error = TRUE)
  expect_snapshot(lat_bins_area(min = 100), error = TRUE)
  expect_snapshot(lat_bins_area(plot = "TRUE"), error = TRUE)
  expect_snapshot(lat_bins_area(r = "Earth"), error = TRUE)
  expect_snapshot(lat_bins_area(n = 3.5), error = TRUE)
})

test_that("lat_bins_area plotting works", {
  # Expect plot
  expect_doppelganger("lat_bins_area", function() {
    lat_bins_area(n = 12, plot = TRUE)
  })
})
