test_that("filter_time works", {

  occdf <- data.frame(genus   = c("A", "A", "B", "B", "B", "C", "D", "E", "E", "F"),
                      max_ma  = c(5,   5,   5,   4,   4,   4,   4,   3,   2,   2),
                      min_ma  = c(4,   4,   4,   3,   3,   2,   2,   1,   0,   1),
                      with_na = c(5, NA,   5,   4,   4,   4,   4,   3,   2,   2),
                      with_ch = letters[6:15])
  bins <- data.frame(bin = 1:5,
                     interval_name = letters[1:5],
                     max_ma = 5:1,
                     min_ma = 4:0,
                     with_ch = letters[6:10])

  # Expect equal
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", youngest_bin = "a", oldest_bin = "a")), 3)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", youngest_bin = "b", oldest_bin = "a")), 5)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 2.5, max_age = 6)), 5)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 0, max_age = 4, uncertainty = 1)), 3)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 0, max_age = 4, uncertainty = 2)), 7)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 0, max_age = 4, uncertainty = "bins")), 3)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 0, max_age = 4, uncertainty = "bins", n_bins = 2)), 8)
  expect_equal(nrow(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 0, max_age = 4, uncertainty = "bins", retain.taxa = T, taxa = "genus")), 9)
  expect_equal(ncol(filter_time(occdf, bins = bins, interval_name = "interval_name", min_age = 0, max_age = 4, uncertainty = "bins", retain.taxa = T, taxa = "genus")), 6)

  # Expect error
  expect_error(filter_time(occdf = 1))
  expect_error(filter_time(occdf))
  expect_error(filter_time(occdf, bins = bins))
  expect_error(filter_time(occdf, max_ma = "oldest", min_ma = "youngest", bins = bins))
  expect_error(filter_time(occdf, max_ma = "with_na", bins = bins))
  expect_error(filter_time(occdf, max_ma = "with_ch", bins = bins))
  expect_error(filter_time(occdf, bins = bins[,c(1, 3, 4)], youngest_bin = "a", oldest_bin = "a"))
  expect_error(filter_time(occdf, bins = bins, youngest_bin = "a"))
  expect_error(filter_time(occdf, bins = bins, min_age = 5, max_age = 2))
  expect_error(filter_time(occdf, bins = bins, min_age = 5))
  expect_error(filter_time(occdf, min_ma = "Min_Ma", max_ma = "Max_Ma", bins = bins[,c(2, 3, 4)], youngest_bin = "a", oldest_bin = "a"))
  expect_error(filter_time(occdf, bins = bins, uncertainty = "1"))
  expect_error(filter_time(occdf, bins = bins, uncertainty = -1))
  expect_error(filter_time(occdf, bins = bins, uncertainty = "bins", n_bins = 0))
  expect_error(filter_time(occdf, bins = bins, uncertainty = "bins", n_bins = "5"))
  expect_error(filter_time(occdf, bins = bins, uncertainty = "bins", retain.taxa = T))
  expect_error(filter_time(occdf, bins = bins, uncertainty = "bins", retain.taxa = "a", taxa = "genus"))
  expect_error(filter_time(cbind.data.frame(occdf, retained = 1:10), bins = bins, uncertainty = "bin", retain.taxa = T, taxa = "genus"))

})
