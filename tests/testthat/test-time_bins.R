test_that("time_bins() works with macrostrat", {
  # Skip if offline
  skip_if_offline(host = "macrostrat.org")

  # Test macrostrat
  expect_equal(nrow(time_bins(scale = "North american land mammal ages")), 19)
  expect_equal(nrow(time_bins(size = 10,
                              scale = "North american land mammal ages")), 7)
})

test_that("time_bins() works", {
  #correct format
  expect_true(is.data.frame(time_bins(interval = 10)))
  expect_true(is.list(time_bins(interval = c("Fortunian", "Meghalayan"),
                                assign = c(232, 167, 33), plot = TRUE)))
  expect_true(is.list(time_bins(
    interval = c("Fortunian", "Holocene"), scale = "GTS2012",
    assign = c(232, 167, 33), plot = TRUE)))
  expect_true(is.vector(time_bins(interval = c("Fortunian", "Meghalayan"),
                                assign = c(232, 167, 33))$Assignation))

  #correct amount of data returned
  expect_equal(nrow(time_bins(interval = c("Maastrichtian"))), 1)
  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"),
                              size = 10, plot = TRUE)), 50)
  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"),
                              size = 6)), 74)
  expect_equal(nrow(time_bins(interval = c("Fortunian", "Meghalayan"),
                              scale = "GTS2020")), 102)
  expect_equal(nrow(time_bins(interval = c("Fortunian", "Holocene"),
                              scale = "GTS2012")), 100)
  expect_equal(nrow(time_bins(interval = c("Fortunian", "Holocene"),
                              scale = "GTS2012", size = 10)), 51)
  expect_equal(nrow(time_bins(interval = c(500, 0), scale = "GTS2012")), 94)
  expect_equal(nrow(time_bins(interval = "Mesozoic", scale = "GTS2012",
                              plot = TRUE)), 30)
  expect_equal(nrow(time_bins(interval = "Mesozoic", rank = "period")), 3)
  expect_equal(nrow(time_bins(interval = c(0, 200), rank = "period")), 5)
  expect_equal(nrow(time_bins(interval = c(0, 200),
                              rank = "period", size = 20)), 4)
  expect_equal(nrow(time_bins(interval = c("Albian", "Danian"),
                              rank = "stage")), 8)
  expect_equal(nrow(time_bins(interval = c("Permian", "Danian"),
                              rank = "period",
                              scale = "GTS2012")), 5)
  expect_equal(colnames(time_bins()), c("bin", "interval_name",
                                        "rank",
                                       "max_ma", "mid_ma", "min_ma",
                                       "duration_myr", "abbr", "colour",
                                       "font"))
  expect_equal(colnames(time_bins(size = 10)), c("bin",
                                        "max_ma", "mid_ma", "min_ma",
                                        "duration_myr", "grouping_rank",
                                        "intervals"))
  expect_equal(nrow(time_bins(size = 50)), 11)

  # Test edge effect resolve
  expect_equal(nrow(time_bins(interval = c("Phanerozoic"),
                              size = 25, rank = "stage")), 22)

  # Test user-input bins
  scale <- data.frame(interval_name = 1:5,
                      min_ma = c(0, 18, 32, 38, 45),
                      max_ma = c(18, 32, 38, 45, 53))
  expect_equal(nrow(time_bins(scale = scale, size = 15)), 4)

  #error handling
  expect_error(time_bins(interval = c("Mastrichtian", "Danian")))
  expect_error(time_bins(interval = "Mastrichtian", scale = "GTS2012",
                         plot = TRUE))
  expect_error(time_bins(interval = "Mastrichtian", scale = "2012",
                         plot = TRUE))
  expect_error(time_bins(interval = 700, scale = "GTS2020", plot = TRUE))
  expect_error(time_bins(interval = -1, plot = TRUE))
  expect_error(time_bins(interval = data.frame()))
  expect_error(time_bins(interval = c(50, 10, 20), plot = TRUE))
  expect_error(time_bins(interval = "Mesozoic", plot = "TRUE"))
  expect_error(time_bins(interval = "Mesozoic", assign = 40))
  expect_error(time_bins(interval = "Mesozoic", assign = -40))
  expect_error(time_bins(interval = "Mesozoic", assign = "30"))
  expect_error(time_bins(interval = "Mesozoic", size = "ten"))
  expect_error(time_bins(interval = "Mesozoic", rank = "stages"))
  expect_error(time_bins(interval = "Mesozoic", rank = c("stage", "period")))
  expect_error(time_bins(interval = "Mesozoic", scale = 1))
  expect_error(time_bins(interval = NULL, scale = "GTS2020"))
  expect_error(time_bins(interval = c(10000, 100), scale = "GTS2020"))
  scale <- data.frame(name = 1:5,
                      min_ma = c(0, 18, 32, 38, 45),
                      max_ma = c(18, 32, 38, 45, 53))
  expect_error(time_bins(scale = scale, size = 15))
})
