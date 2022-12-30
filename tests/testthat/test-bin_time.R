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
                        fun = NULL))

  expect_error(bin_time(occdf = occdf, bins = bins, method = "point",
                        fun = dnorm, x = 1))

  expect_error(bin_time(occdf = occdf, bins = bins, method = "point",
                        fun = dnorm, test = 1))

  expect_error(bin_time(occdf = occdf, bins = bins, method = "point",
                        fun = dnorm, test1 = 1, test2 = 1))

  #expect equal
  occdf <- tetrapods[1:100, ]

  bins <- data.frame(bin = 1:54,
                     max_ma = seq(10, 540, 10),
                     min_ma = seq(0, 530, 10))
  #tests
  expect_equal(class(bin_time(occdf = occdf, bins = bins,
                                  method = "mid")$bin_assignment), "integer")

  expect_equal(length(bin_time(occdf = occdf, bins = bins,
                                 method = "random")), 100)

  expect_equal(is.list(bin_time(occdf = occdf, bins = bins,
                                    method = "point", reps = 5,
                                fun = dnorm, mean = 0.5, sd = 0.25)), TRUE)

  occdf$min_ma[1] <- occdf$max_ma[1]

  expect_equal(is.list(bin_time(occdf = occdf, bins = bins,
                                method = "point", reps = 5,
                                fun = dnorm, mean = 0.5, sd = 0.25)), TRUE)

  drm <- 1

  expect_error(bin_time(occdf = occdf, bins = bins,
                        method = "point", reps = 5,
                        fun = drm, mean = 0.5, sd = 0.25))

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

  occdf$min_ma[1] <- -5000
  expect_error(length(bin_time(occdf = occdf,
                               bins = bins,
  )))

  occdf$max_ma[1] <- 5000
  expect_error(length(bin_time(occdf = occdf,
                               bins = bins,
  )))

  occdf$max_ma[1] <- NA
  expect_error(length(bin_time(occdf = occdf,
                               bins = bins,
                               )))

})
