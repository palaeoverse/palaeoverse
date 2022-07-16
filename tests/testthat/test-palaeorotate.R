test_that("palaeorotate() works", {
  x <- data.frame(lng = c(2, 95, 12),
                  lat = c(46, 12, -65),
                  age = c(88, 203, 467))

  expect_equal(nrow(palaeorotate(x = x)), 3)

  expect_equal(
    ncol(
      palaeorotate(x = x,
                   uncertainty = TRUE)[, c("uncertainty_p_lng",
                                          "uncertainty_p_lat")]), 2)

  expect_equal(nrow(palaeorotate(x = x, model = "Scotese2018")), 3)

  expect_error(palaeorotate(x = x, uncertainty = "TRUE"))

  expect_error(palaeorotate(x = x, uncertainty = 2))

  expect_error(palaeorotate(x = x, model = "Mirdith2021"))

  expect_error(palaeorotate(x = c(55, 46, 88)))

  x <- data.frame(x = c(2, 95, 12), y = c(46, 12, -65), age = c(88, 203, 467))
  expect_error(palaeorotate(x = x))

})
