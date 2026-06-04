test_that("bin_time() works", {
  #error handling
  expect_snapshot(bin_time(occdf = c(50, 20, 10)), error = TRUE)

  expect_snapshot(bin_time(bins = c(50, 20, 10)), error = TRUE)

  expect_snapshot(
    bin_time(occdf = data.frame(), bins = c(50, 20, 10)),
    error = TRUE
  )

  expect_snapshot(
    bin_time(
      occdf = data.frame(),
      bins = data.frame(),
      method = "assign"
    ),
    error = TRUE
  )

  expect_snapshot(
    bin_time(
      occdf = data.frame(),
      bins = data.frame(),
      method = "mid",
      reps = TRUE
    ),
    error = TRUE
  )

  expect_snapshot(
    bin_time(
      occdf = data.frame(),
      bins = data.frame(),
      method = "mid"
    ),
    error = TRUE
  )

  expect_snapshot(
    bin_time(
      occdf = data.frame(),
      bins = data.frame(),
      method = "mid"
    ),
    error = TRUE
  )

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
      fun = dnorm,
      x = 1
    ),
    error = TRUE
  )

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

  #expect equal
  occdf <- tetrapods[1:100, ]

  bins <- data.frame(
    bin = 1:54,
    max_ma = seq(10, 540, 10),
    min_ma = seq(0, 530, 10)
  )
  #tests
  expect_equal(
    class(bin_time(occdf = occdf, bins = bins, method = "mid")$bin_assignment),
    "integer"
  )

  expect_length(bin_time(occdf = occdf, bins = bins, method = "random"), 100)

  expect_true(
    is.list(bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      reps = 5,
      fun = dnorm,
      mean = 0.5,
      sd = 0.25
    ))
  )

  occdf$min_ma[1] <- occdf$max_ma[1]

  expect_true(
    is.list(bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      reps = 5,
      fun = dnorm,
      mean = 0.5,
      sd = 0.25
    ))
  )

  drm <- 1

  expect_snapshot(
    bin_time(
      occdf = occdf,
      bins = bins,
      method = "point",
      reps = 5,
      fun = drm,
      mean = 0.5,
      sd = 0.25
    ),
    error = TRUE
  )

  expect_false(is.list(
    bin_time(
      occdf = occdf,
      bins = bins,
      reps = 1,
      method = "random"
    )$bin_midpoint
  ))

  expect_length(bin_time(occdf = occdf, bins = bins, method = "random"), 100)

  expect_true(any(
    colnames(bin_time(
      occdf = occdf,
      bins = bins,
      method = "majority"
    )) ==
      "overlap_percentage"
  ))

  expect_true(
    nrow(bin_time(occdf = occdf, bins = bins, method = "all")) > nrow(occdf)
  )

  occdf$min_ma[1] <- -5000
  expect_snapshot(length(bin_time(occdf = occdf, bins = bins, )), error = TRUE)

  occdf$max_ma[1] <- 5000
  expect_snapshot(length(bin_time(occdf = occdf, bins = bins, )), error = TRUE)

  occdf$max_ma[1] <- NA
  expect_snapshot(length(bin_time(occdf = occdf, bins = bins, )), error = TRUE)
})
