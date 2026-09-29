test_that("bin_space() works", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 100)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(occdf = occdf, spacing = 250)),
      nrow(occdf)
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_equal(
      nrow(
        bin_space(occdf = occdf, spacing = 1000, sub_grid = 250)
      ),
      nrow(occdf)
    ),
    "H3 resolution: 1"
  )

  # Check output type
  expect_message(
    expect_type(
      bin_space(occdf = occdf, spacing = 250, return = TRUE),
      "list"
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_type(
      bin_space(occdf = occdf, spacing = 500, sub_grid = 200, return = TRUE),
      "list"
    ),
    "H3 resolution: 1"
  )
})

test_that("piping and not piping the first argument give the same result", {
  occdf <- head(tetrapods, n = 100)

  expect_equal(
    suppressMessages(occdf |> bin_space(spacing = 250)),
    suppressMessages(bin_space(occdf, spacing = 250))
  )
})

test_that("bin_space errors with unnamed args", {
  occdf <- head(tetrapods, n = 100)

  expect_snapshot(bin_space(occdf, "lng"), error = TRUE)
  expect_snapshot(bin_space(occdf = occdf, "lng"), error = TRUE)
  expect_snapshot(bin_space(occdf, "lng", "lat"), error = TRUE)
  expect_snapshot(bin_space(occdf, "lng", lat = "lat"), error = TRUE)
})

test_that("bin_space error handling", {
  # We modify this data so copy it first
  occdf <- tetrapods

  # wrong input type
  expect_snapshot(bin_space(occdf = matrix(tetrapods)), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, spacing = NA), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, spacing = 1:2), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, spacing = -1), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, spacing = 0), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, sub_grid = 1:2), error = TRUE)
  expect_snapshot(
    bin_space(occdf = tetrapods, spacing = 1000, sub_grid = NA),
    error = TRUE
  )
  expect_snapshot(bin_space(occdf = tetrapods, sub_grid = -1), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, sub_grid = 0), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, return = "TRUE"), error = TRUE)

  # wrong columns
  expect_snapshot(
    bin_space(occdf = tetrapods, lng = "long", lat = "latit"),
    error = TRUE
  )

  # spacing and sub_grid give the same resolution
  expect_snapshot(
    bin_space(occdf = tetrapods, spacing = 1000, sub_grid = 1000),
    error = TRUE
  )
  # lat must be a numeric value between -90 and 90
  occdf$lat[1] <- 94
  expect_snapshot(bin_space(occdf = occdf), error = TRUE)
  occdf$lat[1] <- "94"
  expect_snapshot(bin_space(occdf = occdf), error = TRUE)

  # lng must be a numeric value between -180 and 180
  occdf <- tetrapods
  occdf$lng[1] <- 184
  expect_snapshot(bin_space(occdf = occdf), error = TRUE)
  occdf$lng[1] <- "184"
  expect_snapshot(bin_space(occdf = occdf), error = TRUE)
})

test_that("argument 'plot' works", {
  dat <- reefs[1:50, ]
  expect_doppelganger("bin_space", function() {
    expect_message(
      plot(bin_space(occdf = dat, spacing = 1000)),
      "set to 725.17 km"
    )
  })
})

test_that("bin_space plotting with extra args works", {
  dat <- reefs[1:50, ]
  expect_doppelganger("bin_space-axis-labels", function() {
    expect_message(
      plot(
        bin_space(occdf = dat, spacing = 1000),
        xlab = "x axis",
        ylab = "y axis"
      ),
      "set to 725.17 km"
    )
  })
  expect_doppelganger("bin_space-axes", function() {
    expect_message(
      plot(bin_space(occdf = dat, spacing = 1000), axes = FALSE),
      "set to 725.17 km"
    )
  })
  # `main` is passed through `...`
  expect_doppelganger("bin_space-dots", function() {
    expect_message(
      plot(bin_space(occdf = dat, spacing = 1000), main = "hello there"),
      "set to 725.17 km"
    )
  })

  # forbidden args
  expect_snapshot(
    plot(bin_space(occdf = dat, spacing = 1000), setParUsrBB = FALSE),
    error = TRUE
  )
})

test_that("plot is deprecated but still works", {
  dat <- reefs[1:50, ]
  expect_doppelganger("bin_space_deprecated", function() {
    expect_message(
      expect_warning(
        bin_space(occdf = dat, spacing = 1000, plot = TRUE),
        "is deprecated as of palaeoverse 2.0.0",
        fixed = TRUE
      ),
      "set to 725.17 km"
    )
  })

  # still input checking
  expect_snapshot(
    bin_space(occdf = dat, spacing = 1000, plot = "6"),
    error = TRUE
  )
})
