test_that("time_binning() works", {

  #error handling
  expect_error(time_binning(occdf = c(50, 20, 10)))

  expect_error(time_binning(bins = c(50, 20, 10)))

  expect_error(time_binning(occdf = data.frame(),
                            bins = c(50, 20, 10)))

  expect_error(time_binning(occdf = data.frame(),
                            bins = data.frame(),
                            method = "assign"))

  expect_error(time_binning(occdf = data.frame(),
                            bins = data.frame(),
                            method = "mid",
                            reps = TRUE))

  expect_error(time_binning(occdf = data.frame(),
                            bins = data.frame(),
                            method = "mid",
                            scale = "2012"))

  expect_error(time_binning(occdf = data.frame(),
                            bins = data.frame(),
                            method = "mid",
                            scale = "GTS2020",
                            return_error = "TRUE"))

  #expect equal
  occdf <- read.csv("https://paleobiodb.org/data1.2/colls/list.csv?base_name=Scleractinia")
  bins <- data.frame(bin = 1:54, max_ma = seq(10, 540, 10), min_ma = seq(0, 530, 10))
  #tests
  expect_equal(class(time_binning(occdf = occdf, bins = bins, method = "mid")$bin_assignment), "integer")

  expect_equal(nrow(time_binning(occdf = occdf, bins = bins, method = "random")), nrow(occdf))

  expect_equal(is.list(time_binning(occdf = occdf, bins = bins, method = "point")$point_estimates), TRUE)

  expect_equal(length(time_binning(occdf = occdf, bins = bins, method = "random")$bin_assignment[[1]]), 100)

  expect_equal(any(colnames(time_binning(occdf = occdf, bins = bins, method = "majority")) == "overlap_percentage"), TRUE)

  expect_equal(nrow(time_binning(occdf = occdf, bins = bins, method = "all")) > nrow(occdf), TRUE)

  occdf <- data.frame(max_ma = c("Mastrichtian", "Albian"), min_ma = c("Mastrichtian", "Albian"))
  expect_equal(is.integer(time_binning(occdf = occdf, bins = bins, return_error = TRUE)), TRUE)
})
