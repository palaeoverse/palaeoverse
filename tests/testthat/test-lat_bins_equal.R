test_that("lat_bins_equal works", {

  # Expect equal
  expect_equal(nrow(lat_bins_equal(n = 12)), 12)
  expect_equal(nrow(lat_bins_equal(n = 6, min = 0, max = 90)), 6)

  # Expect error
  expect_error(lat_bins_equal(n = "10"))
  expect_error(lat_bins_equal(max = 100))
  expect_error(lat_bins_equal(min = 90, max = -90))
  expect_error(lat_bins_equal(min = 100))
  expect_error(lat_bins_equal(plot = "TRUE"))
  expect_error(lat_bins_equal(r = "Earth"))
})

test_that("lat_bins_equal plotting works", {

  # Expect plot
  expect_doppelganger("lat_bins_equal", function() {
    lat_bins_equal(n = 12, plot = TRUE)
  })

})
