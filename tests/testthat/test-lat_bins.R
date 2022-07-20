test_that("lat_bins() works", {

  #expect equal
  expect_equal(nrow(lat_bins()), 18)
  expect_equal(nrow(lat_bins(size = 13)), 13)
  expect_equal(nrow(lat_bins(size = 13, fit = TRUE)), 15)
  expect_equal(nrow(lat_bins(size = 13, fit = TRUE, plot = TRUE)), 15)

  #expect true
  expect_true(is.list(lat_bins(assign = c(-20, 45, 11, 67))))

  #expect error
  expect_error(lat_bins(assign = c(-100)))
  expect_error(lat_bins(size = 100))
  expect_error(lat_bins(fit = 1))
  expect_error(lat_bins(size = "10"))
  expect_error(lat_bins(plot = "TRUE"))

})
