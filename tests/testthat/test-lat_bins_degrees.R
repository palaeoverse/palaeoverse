test_that("lat_bins_degrees() works", {

  #expect equal
  expect_equal(nrow(lat_bins_degrees()), 18)
  expect_equal(nrow(lat_bins_degrees(size = 13)), 13)
  expect_equal(nrow(lat_bins_degrees(size = 13, fit = TRUE)), 15)
  expect_equal(nrow(lat_bins_degrees(size = 13, fit = TRUE, plot = TRUE)), 15)

  #expect error
  expect_error(lat_bins_degrees(size = 100))
  expect_error(lat_bins_degrees(max = 100))
  expect_error(lat_bins_degrees(min = 100))
  expect_error(lat_bins_degrees(max = 10, min = 20))
  expect_error(lat_bins_degrees(fit = 1))
  expect_error(lat_bins_degrees(size = "10"))
  expect_error(lat_bins_degrees(plot = "TRUE"))

})
