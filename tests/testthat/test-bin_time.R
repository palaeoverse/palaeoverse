test_that("bin_time() works", {

  #error handling
  expect_error(bin_time(occdf = c(50, 20, 10)))

  expect_error(bin_time(bins = c(50, 20, 10)))

  expect_error(bin_time(occdf = data.frame(),
                            bins = c(50, 20, 10)))

  expect_error(bin_time(occdf = data.frame(),
                            bins = data.frame(),
                            method = "assign"))

  expect_error(bin_time(occdf = data.frame(),
                            bins = data.frame(),
                            method = "mid",
                            reps = TRUE))

  expect_error(bin_time(occdf = data.frame(),
                            bins = data.frame(),
                            method = "mid"))

  expect_error(bin_time(occdf = data.frame(),
                            bins = data.frame(),
                            method = "mid"))

  expect_error(bin_time(occdf = occdf, bins = bins, method = "point",
                        prob = "test"))

  #expect equal
  occdf <- tetrapods

  bins <- data.frame(bin = 1:54,
                     max_ma = seq(10, 540, 10),
                     min_ma = seq(0, 530, 10))
  #tests
  expect_equal(class(bin_time(occdf = occdf, bins = bins,
                                  method = "mid")$bin_assignment), "integer")

  expect_equal(length(bin_time(occdf = occdf, bins = bins,
                                 method = "random")), 100)

  expect_equal(is.list(bin_time(occdf = occdf, bins = bins,
                                    method = "point")), TRUE)

  expect_equal(is.list(bin_time(occdf = occdf, bins = bins,
                                    reps = 1,
                                    method = "random")$bin_midpoint), FALSE)

  expect_equal(length(bin_time(occdf = occdf,
                                   bins = bins,
                                   method = "random")),
               100)

  expect_equal(any(colnames(bin_time(
    occdf = occdf,
    bins = bins, method = "majority")) == "overlap_percentage"), TRUE)

  expect_equal(nrow(bin_time(occdf = occdf,
                                 bins = bins,
                                 method = "all")) > nrow(occdf), TRUE)

  occdf <- data.frame(max_ma = c("Mastrichtian", "Albian"),
                      min_ma = c("Mastrichtian", "Albian"))
  expect_error(bin_time(occdf = occdf,
                                       bins = bins,
                                       return_error = TRUE))
})
