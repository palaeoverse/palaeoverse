test_that("time_bins() works", {

  expect_true(is.data.frame(time_bins(interval = 10)))

  expect_equal(nrow(time_bins(interval = c("Maastrichtian"))), 1)

  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"), size = 10)), 50)

  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"), size = 6)), 76)

  expect_true(is.list(time_bins(interval = c("Fortunian", "Meghalayan"), assign = c(232, 167, 33), plot = TRUE)))

  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"), scale = "GTS2020")), 102)

  expect_equal(nrow(time_bins(interval = c("Fortunian", "Holocene"), scale = "GTS2012")), 100)

  expect_equal(nrow(time_bins(interval = c("Fortunian", "Holocene"), scale = "GTS2012", size = 10)), 51)

  expect_true(is.list(time_bins(interval = c("Fortunian", "Holocene"), scale = "GTS2012", assign = c(232, 167, 33), plot = TRUE)))

  expect_true(is.list(time_bins(interval = c("Fortunian", "Holocene"), scale = "GTS2012", assign = c(232, 167, 33), plot = FALSE)))

  expect_equal(nrow(time_bins(interval = c(500, 0), scale = "GTS2012")), 94)

  expect_equal(nrow(time_bins(interval = "Mesozoic", scale = "GTS2012", plot = TRUE)), 1)

  expect_error(time_bins(interval = "Mastrichtian", scale = "GTS2012", plot = TRUE))

  expect_error(time_bins(interval = -1, plot = TRUE))

  expect_error(time_bins(interval = c(50, 10, 20), plot = TRUE))

  expect_error(time_bins(interval = "Mesozoic", plot = "TRUE"))

  expect_error(time_bins(interval = "Mesozoic", plot = "TRUE"))

  expect_error(time_bins(interval = "Mesozoic", assign = 40))

  expect_error(time_bins(interval = "Mesozoic", assign = -40))

  expect_error(time_bins(interval = "Mesozoic", assign = "test"))

  expect_error(time_bins(interval = "Mesozoic", size = "ten"))

})
