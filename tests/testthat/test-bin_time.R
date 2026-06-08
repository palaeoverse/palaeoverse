test_that("bin_time() works with method 'mid'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  bin_mid <- bin_time(occdf = occdf, bins = bins, method = "mid")
  expect_named(
    bin_mid,
    c(names(occdf), "id", "n_bins", "bin_assignment", "bin_midpoint")
  )
  expect_type(bin_mid$id, "integer")
  expect_type(bin_mid$n_bins, "integer")
  expect_type(bin_mid$bin_assignment, "integer")
  expect_type(bin_mid$bin_midpoint, "double")

  # Test the actual values
  expect_snapshot(bin_mid[, c(
    "id",
    "n_bins",
    "bin_assignment",
    "bin_midpoint"
  )])
})

test_that("bin_time() works with method 'majority'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  bin_majority <- bin_time(occdf = occdf, bins = bins, method = "majority")
  expect_named(
    bin_majority,
    c(
      names(occdf),
      "id",
      "n_bins",
      "bin_assignment",
      "bin_midpoint",
      "overlap_percentage"
    )
  )
  expect_type(bin_majority$id, "integer")
  expect_type(bin_majority$n_bins, "integer")
  expect_type(bin_majority$bin_assignment, "integer")
  expect_type(bin_majority$bin_midpoint, "double")
  expect_type(bin_majority$overlap_percentage, "double")

  # Test the actual values
  expect_snapshot(bin_majority[, c(
    "id",
    "n_bins",
    "bin_assignment",
    "bin_midpoint"
  )])
})

test_that("bin_time() works with method 'all'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  bin_all <- bin_time(occdf = occdf, bins = bins, method = "all")
  expect_named(
    bin_all,
    c(names(occdf), "id", "n_bins", "bin_assignment", "bin_midpoint")
  )
  expect_type(bin_all$id, "integer")
  expect_type(bin_all$n_bins, "integer")
  expect_type(bin_all$bin_assignment, "integer")
  expect_type(bin_all$bin_midpoint, "double")

  # Test the actual values
  expect_snapshot(bin_all[, c(
    "id",
    "n_bins",
    "bin_assignment",
    "bin_midpoint"
  )])
})

test_that("bin_time() works with method 'random'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  set.seed(1234)
  bin_random <- bin_time(
    occdf = occdf,
    bins = bins,
    method = "random",
    reps = 5
  )
  expect_type(bin_random, "list")
  expect_length(bin_random, 5)
  invisible(
    lapply(bin_random, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(names(occdf), "id", "n_bins", "bin_assignment", "bin_midpoint")
      )
      expect_type(x$id, "integer")
      expect_type(x$n_bins, "integer")
      expect_type(x$bin_assignment, "integer")
      expect_type(x$bin_midpoint, "double")
    })
  )

  # Test the actual values
  expect_snapshot(bin_random[[1]][, c(
    "id",
    "n_bins",
    "bin_assignment",
    "bin_midpoint"
  )])
})

test_that("bin_time() works with method 'point'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  bin_point <- bin_time(
    occdf = occdf,
    bins = bins,
    method = "point",
    reps = 5
  )
  expect_type(bin_point, "list")
  expect_length(bin_point, 5)
  invisible(
    lapply(bin_point, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(names(occdf), "id", "n_bins", "bin_assignment", "point_estimates")
      )
      expect_type(x$id, "integer")
      expect_type(x$n_bins, "integer")
      expect_type(x$bin_assignment, "integer")
      expect_type(x$point_estimates, "double")
    })
  )

  # Test the actual values
  expect_snapshot(bin_point[[1]][, c(
    "id",
    "n_bins",
    "bin_assignment",
    "point_estimates"
  )])
})

test_that("user can pass custom function to method 'point'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  set.seed(1234)
  bin_point <- bin_time(
    occdf = occdf,
    bins = bins,
    method = "point",
    reps = 5,
    fun = dnorm,
    mean = 0.5,
    sd = 0.25
  )
  expect_type(bin_point, "list")
  expect_length(bin_point, 5)
  invisible(
    lapply(bin_point, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(names(occdf), "id", "n_bins", "bin_assignment", "point_estimates")
      )
      expect_type(x$id, "integer")
      expect_type(x$n_bins, "integer")
      expect_type(x$bin_assignment, "integer")
      expect_type(x$point_estimates, "double")
    })
  )

  expect_snapshot(bin_point[[1]][, c(
    "id",
    "n_bins",
    "bin_assignment",
    "point_estimates"
  )])
})

test_that("user can pass custom function to method 'random'", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  set.seed(1234)
  bin_random <- bin_time(
    occdf = occdf,
    bins = bins,
    method = "random",
    reps = 5,
    fun = dnorm,
    mean = 0.5,
    sd = 0.25
  )
  expect_type(bin_random, "list")
  expect_length(bin_random, 5)
  invisible(
    lapply(bin_random, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(names(occdf), "id", "n_bins", "bin_assignment", "bin_midpoint")
      )
      expect_type(x$id, "integer")
      expect_type(x$n_bins, "integer")
      expect_type(x$bin_assignment, "integer")
      expect_type(x$bin_midpoint, "double")
    })
  )

  expect_snapshot(bin_random[[1]][, c(
    "id",
    "n_bins",
    "bin_assignment",
    "bin_midpoint"
  )])
})

test_that("method 'all' returns more rows than in the input", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  bin_random <- bin_time(occdf = occdf, bins = bins, method = "all")
  expect_true(nrow(bin_random) > nrow(occdf))
})

test_that("wrong input for occdf", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  # "occdf" must be a non-empty dataframe and must be provided
  expect_snapshot(bin_time(occdf = c(50, 20, 10)), error = TRUE)
  expect_snapshot(bin_time(bins = c(50, 20, 10)), error = TRUE)
  expect_snapshot(
    bin_time(occdf = data.frame(), bins = c(50, 20, 10)),
    error = TRUE
  )
  expect_snapshot(
    bin_time(occdf = data.frame(), bins = data.frame(), method = "mid"),
    error = TRUE
  )
  expect_snapshot(
    bin_time(occdf = data.frame(), bins = data.frame(), method = "mid"),
    error = TRUE
  )

  # dataframe that doesn't have the expected columns
  # TODO: this error message should be clearer
  expect_snapshot(
    bin_time(mtcars, occdf = c(50, 20, 10)),
    error = TRUE
  )
})

test_that("wrong input for method", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  expect_snapshot(
    bin_time(occdf = occdf, bins = bins, method = "foo"),
    error = TRUE
  )
})

test_that("wrong input for reps", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  expect_snapshot(
    bin_time(occdf = occdf, bins = bins, method = "random", reps = TRUE),
    error = TRUE
  )
})

test_that("wrong input for fun", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  # "fun" must be a function
  expect_snapshot(
    bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      fun = NULL
    ),
    error = TRUE
  )
  expect_snapshot(
    bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      fun = 1
    ),
    error = TRUE
  )

  # "x" shouldn't be provided
  expect_snapshot(
    bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      fun = dnorm,
      x = 1
    ),
    error = TRUE
  )

  # "test" is an invalid arg
  expect_snapshot(
    bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      fun = dnorm,
      test = 1
    ),
    error = TRUE
  )

  # multiple invalid args
  expect_snapshot(
    bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      fun = dnorm,
      test1 = 1,
      test2 = 1
    ),
    error = TRUE
  )
})

test_that("errors in data for min and max age", {
  occdf <- tetrapods[1:5, ]
  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )

  # Min age in data cannot be less than min age of bins
  occdf$min_ma[1] <- -5000
  expect_snapshot(bin_time(occdf = occdf, bins = bins), error = TRUE)

  # Max age in data cannot be less than max age of bins
  occdf$max_ma[1] <- 5000
  expect_snapshot(bin_time(occdf = occdf, bins = bins), error = TRUE)

  # Min or max age cannot have missing values
  occdf$max_ma[1] <- NA
  expect_snapshot(bin_time(occdf = occdf, bins = bins), error = TRUE)
})

# TODO: shouldn't this error?
# test_that("arg 'fun' is only used if method is 'point'", {
#   occdf <- tetrapods[1:5, ]
#   bins <- data.frame(
#     bin = 1:54,
#     max_ma = seq(10, 540, 10),
#     min_ma = seq(0, 530, 10)
#   )

#   expect_snapshot(
#     bin_time(occdf = occdf, bins = bins, method = "random", fun = dnorm),
#     error = TRUE
#   )
# })

# TODO: shouldn't this error?
# test_that("arg 'reps' is only used if method is 'point' or 'random'", {
#   occdf <- tetrapods[1:5, ]
#   bins <- data.frame(
#     bin = 1:54,
#     max_ma = seq(10, 540, 10),
#     min_ma = seq(0, 530, 10)
#   )

#   expect_snapshot(
#     bin_time(occdf = occdf, bins = bins, method = "all", reps = 10),
#     error = TRUE
#   )
# })
