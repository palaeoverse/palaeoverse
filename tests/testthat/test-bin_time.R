### Prepare two small datasets with several edge cases to ensure that
### there are no logic errors.

test_bins <- data.frame(
  bin = 1:5,
  min_ma = c(0, 10, 20, 30, 40),
  max_ma = c(10, 20, 30, 40, 50)
)

test_occdf <- rbind(
  # Single-bin cases ------------------------------------

  # Falls entirely within bin 1: all methods should agree.
  c("occ1", 2, 8),
  # Range exactly equals bin 2's boundaries so it lands only in bin 2, not the neighbours.
  c("occ2", 10, 20),

  # Two-bin cases ------------------------------------

  # Spans exactly two bins (1 and 2), 50/50 split. Edge cases:
  # - "majority" hits a tie -> which.max picks the first (bin 1)
  # - "mid": midpoint (10) is a bin boundary, so we assign NA and throw a warning
  c("occ3", 5, 15),
  # Spans two bins but unevenly. "majority" clearly picks bin 2, midpoint (14) is within
  # bin 2, so "mid" also bin 2.
  c("occ4", 9, 19),

  # Multi-bin cases ------------------------------------

  # Spans three bins (1, 2, 3). "all" duplicates this row 3 times; "random"/"point" sample
  # across all three.
  c("occ5", 5, 25),
  # Spans the full set of bins (1-5). Min/max sit exactly on the outer edges of the bin range
  c("occ6", 0, 50),

  # Midpoint warning case (relevant to method "mid") ------------------------------------

  # "mid" both assigns bin 1 and emits the "equivalent to a bin boundary" warning - highlights
  # the warning/assignment mismatch.
  c("occ7", 0, 10),

  # Zero-duration (point) occurrence ------------------------------------

  # min_ma == max_ma (single point in time). Special-cased in "point"/"random" (age is just
  # replicated); lands in bin 2.
  c("occ8", 15, 15)
) |>
  as.data.frame()

colnames(test_occdf) <- c("name", "min_ma", "max_ma")
test_occdf$min_ma <- as.numeric(test_occdf$min_ma)
test_occdf$max_ma <- as.numeric(test_occdf$max_ma)


test_that("bin_time() works with method 'mid'", {
  # occ7's midpoint equals a bin midpoint, which triggers a warning
  expect_warning(
    bin_mid <- bin_time(occdf = test_occdf, bins = test_bins, method = "mid"),
    "equivalent to a bin boundary"
  )
  # occ3's midpoint (10) falls on a bin boundary, so it is NA
  expect_equal(
    bin_mid[, c("name", "id", "n_bins", "bin_assignment", "bin_midpoint")],
    data.frame(
      name = paste0("occ", 1:8),
      id = 1:8,
      n_bins = c(1, 1, 2, 2, 3, 5, 1, 1),
      bin_assignment = c(1, 2, NA, 2, 2, 3, 1, 2),
      bin_midpoint = c(5, 15, NA, 15, 15, 25, 5, 15)
    )
  )
  # TODO: regression test for https://github.com/palaeoverse/palaeoverse/issues/236
  # expect_silent(
  #   bin_time(occdf = test_occdf[5, ], bins = test_bins, method = "mid")
  # )
})

test_that("bin_time() works with method 'majority'", {
  bin_majority <- bin_time(
    occdf = test_occdf,
    bins = test_bins,
    method = "majority"
  )
  # occ3 is a 50/50 tie, resolved to the first bin (bin 1)
  expect_equal(
    bin_majority[, c(
      "name",
      "id",
      "n_bins",
      "bin_assignment",
      "bin_midpoint",
      "overlap_percentage"
    )],
    data.frame(
      name = paste0("occ", 1:8),
      id = 1:8,
      n_bins = c(1, 1, 2, 2, 3, 5, 1, 1),
      bin_assignment = c(1, 2, 1, 2, 2, 1, 1, 2),
      bin_midpoint = c(5, 15, 5, 15, 15, 5, 5, 15),
      overlap_percentage = c(100, 100, 50, 90, 50, 20, 100, 100)
    )
  )
})

test_that("bin_time() works with method 'all'", {
  bin_all <- bin_time(occdf = test_occdf, bins = test_bins, method = "all")
  # Occurrences spanning several bins are duplicated, one row per bin
  # fmt: skip
  expect_equal(
    bin_all[, c("name", "id", "n_bins", "bin_assignment", "bin_midpoint")],
    data.frame(
      name = c(
        "occ1", "occ2", "occ3", "occ3", "occ4", "occ4", "occ5", "occ5", "occ5",
        "occ6", "occ6", "occ6", "occ6", "occ6", "occ7", "occ8"
      ),
      id = c(1, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 7, 8),
      n_bins = c(1, 1, 2, 2, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 1, 1),
      bin_assignment = c(1, 2, 1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 4, 5, 1, 2),
      bin_midpoint = c(5, 15, 5, 15, 5, 15, 5, 15, 25, 5, 15, 25, 35, 45, 5, 15)
    )
  )
})

test_that("bin_time() works with method 'random'", {
  set.seed(1234)
  bin_random <- bin_time(
    occdf = test_occdf,
    bins = test_bins,
    method = "random",
    reps = 5
  )

  # We test that:
  # - all elements have the correct column names and columns
  # - values that shouldn't change across samples are correct
  # - all values in a single element of the list are correct
  expect_type(bin_random, "list")
  expect_length(bin_random, 5)
  invisible(
    lapply(bin_random, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(names(test_occdf), "id", "n_bins", "bin_assignment", "bin_midpoint")
      )

      # Those observations are contained in a single interval so they shouldn't
      # change across elements
      constant <- x[
        c(1, 2, 7, 8),
        c(
          "name",
          "id",
          "n_bins",
          "bin_assignment",
          "bin_midpoint"
        )
      ]
      expect_equal(
        constant,
        data.frame(
          name = c("occ1", "occ2", "occ7", "occ8"),
          id = c(1, 2, 7, 8),
          n_bins = 1,
          bin_assignment = rep(1:2, 2),
          bin_midpoint = rep(c(5, 15), 2),
          row.names = c(1L, 2L, 7L, 8L)
        )
      )
    })
  )
  # Check one full dataframe:
  expect_equal(
    bin_random[[1]][, c(
      "name",
      "id",
      "n_bins",
      "bin_assignment",
      "bin_midpoint"
    )],
    data.frame(
      name = paste0("occ", 1:8),
      id = 1:8,
      n_bins = c(1, 1, 2, 2, 3, 5, 1, 1),
      bin_assignment = c(1, 2, 2, 2, 2, 4, 1, 2),
      bin_midpoint = c(5, 15, 15, 15, 15, 35, 5, 15)
    )
  )
})

test_that("bin_time() works with method 'point'", {
  set.seed(1234)
  bin_point <- bin_time(
    occdf = test_occdf,
    bins = test_bins,
    method = "point",
    reps = 5
  )

  # We test that:
  # - all elements have the correct column names and columns
  # - values that shouldn't change across samples are correct
  # - all values in a single element of the list are correct
  expect_type(bin_point, "list")
  expect_length(bin_point, 5)
  invisible(
    lapply(bin_point, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(
          names(test_occdf),
          "id",
          "n_bins",
          "bin_assignment",
          "point_estimates"
        )
      )
      expect_type(x$id, "integer")
      expect_type(x$n_bins, "integer")
      expect_type(x$bin_assignment, "integer")
      expect_type(x$point_estimates, "double")

      # occ8 has min_ma == max_ma, so its point estimate is always exactly 15
      constant <- x[
        8,
        c(
          "name",
          "id",
          "n_bins",
          "bin_assignment",
          "point_estimates"
        )
      ]
      expect_equal(
        constant,
        data.frame(
          name = "occ8",
          id = 8,
          n_bins = 1,
          bin_assignment = 2,
          point_estimates = 15,
          row.names = 8L
        )
      )
    })
  )

  # Check one full dataframe:
  expect_equal(
    bin_point[[1]][, c(
      "name",
      "id",
      "n_bins",
      "bin_assignment",
      "point_estimates"
    )],
    data.frame(
      name = paste0("occ", 1:8),
      id = 1:8,
      n_bins = c(1, 1, 2, 2, 3, 5, 1, 1),
      bin_assignment = c(1, 2, 1, 2, 1, 3, 1, 2),
      point_estimates = c(
        3.003,
        12.373,
        5.995,
        18.594,
        5.297,
        20.191,
        9.346,
        15
      )
    )
  )
})

test_that("user can pass custom function to method 'point'", {
  set.seed(1234)
  bin_point <- bin_time(
    occdf = test_occdf,
    bins = test_bins,
    method = "point",
    reps = 5,
    fun = dnorm,
    mean = 0.5,
    sd = 0.25
  )

  # We test that:
  # - all elements have the correct column names and columns
  # - values that shouldn't change across samples are correct
  # - all values in a single element of the list are correct
  expect_type(bin_point, "list")
  expect_length(bin_point, 5)
  invisible(
    lapply(bin_point, function(x) {
      expect_true(is.data.frame(x))
      expect_named(
        x,
        c(
          names(test_occdf),
          "id",
          "n_bins",
          "bin_assignment",
          "point_estimates"
        )
      )
      expect_type(x$id, "integer")
      expect_type(x$n_bins, "integer")
      expect_type(x$bin_assignment, "integer")
      expect_type(x$point_estimates, "double")

      # This interval has the same min and max so it should never change
      constant <- x[
        8,
        c(
          "name",
          "id",
          "n_bins",
          "bin_assignment",
          "point_estimates"
        )
      ]
      expect_equal(
        constant,
        data.frame(
          name = "occ8",
          id = 8,
          n_bins = 1,
          bin_assignment = 2,
          point_estimates = 15,
          row.names = 8L
        )
      )
    })
  )

  # Check one full dataframe
  expect_equal(
    bin_point[[1]][, c(
      "id",
      "n_bins",
      "bin_assignment",
      "point_estimates"
    )],
    data.frame(
      id = 1:8,
      n_bins = c(1, 1, 2, 2, 3, 5, 1, 1),
      bin_assignment = c(1, 2, 1, 2, 1, 3, 1, 2),
      point_estimates = c(
        3.003,
        12.373,
        5.995,
        14.027,
        5.297,
        20.191,
        5.195,
        15
      )
    )
  )
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
