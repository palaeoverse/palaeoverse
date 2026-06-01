test_that("lat_bins_degrees() works", {
  #expect equal
  expect_equal(nrow(lat_bins_degrees()), 18)
  expect_equal(nrow(lat_bins_degrees(size = 13)), 13)
  expect_equal(nrow(lat_bins_degrees(size = 13, fit = TRUE)), 15)
  expect_equal(nrow(lat_bins_degrees(size = 13, fit = TRUE, plot = TRUE)), 15)

  #expect error
  expect_snapshot(lat_bins_degrees(size = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(max = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(min = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(max = 10, min = 20), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = 1), error = TRUE)
  expect_snapshot(lat_bins_degrees(size = "10"), error = TRUE)
  expect_snapshot(lat_bins_degrees(plot = "TRUE"), error = TRUE)
})
