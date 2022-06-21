test_that("time_bins() works", {

  expect_true(is.data.frame(time_bins(interval = 10)))

  expect_equal(nrow(time_bins(interval = c("Maastrichtian"))), 1)

  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"), equal = TRUE, size = 10)), 50)

  expect_true(is.list(time_bins(interval = c("Fortunian", "Meghalayan"), assign = c(232, 167, 33), plot = TRUE)))
})
