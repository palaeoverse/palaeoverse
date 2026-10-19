test_that("bin_space() works", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 50)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(occdf = occdf, bins = space_bins(spacing = 250))),
      nrow(occdf)
    ),
    "H3 resolution: 2"
  )

  # three new columns: cell id, centroid lat, centroid lon
  expect_named(
    bin_space(occdf = occdf, bins = space_bins(spacing = 250)) |>
      suppressMessages(),
    c(
      names(occdf),
      "cell_ID_2",
      "cell_centroid_lng_2",
      "cell_centroid_lat_2"
    )
  )
})

test_that("we can chain several bin_space()", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 50)

  expect_message(
    expect_message(
      expect_equal(
        occdf |>
          bin_space(bins = space_bins(spacing = 1000)) |>
          bin_space(bins = space_bins(spacing = 250)) |>
          nrow(),
        nrow(occdf)
      ),
      "H3 resolution: 2"
    ),
    "H3 resolution: 1"
  )
  expect_named(
    occdf |>
      bin_space(bins = space_bins(spacing = 1000)) |>
      bin_space(bins = space_bins(spacing = 250)) |>
      suppressMessages(),
    c(
      names(occdf),
      "cell_ID_1",
      "cell_centroid_lng_1",
      "cell_centroid_lat_1",
      "cell_ID_2",
      "cell_centroid_lng_2",
      "cell_centroid_lat_2"
    )
  )

  # We can chain from larger to smaller spacing, or from smaller to larger (column ordering
  # is the only thing that changes)
  large_then_small <- occdf |>
    bin_space(bins = space_bins(spacing = 1000)) |>
    bin_space(bins = space_bins(spacing = 250)) |>
    suppressMessages()
  small_then_large <- occdf |>
    bin_space(bins = space_bins(spacing = 250)) |>
    bin_space(bins = space_bins(spacing = 1000)) |>
    suppressMessages()

  expect_equal(
    large_then_small,
    small_then_large[, c(
      names(occdf),
      "cell_ID_1",
      "cell_centroid_lng_1",
      "cell_centroid_lat_1",
      "cell_ID_2",
      "cell_centroid_lng_2",
      "cell_centroid_lat_2"
    )]
  )
})

test_that("bin_space() overwrites existing cell columns", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 50)

  expect_snapshot(
    out <- occdf |>
      bin_space(bins = space_bins(spacing = 1000)) |>
      bin_space(bins = space_bins(spacing = 900))
  )
  expect_named(
    out,
    c(names(occdf), "cell_ID_1", "cell_centroid_lng_1", "cell_centroid_lat_1")
  )
})

test_that("piping and not piping the first argument give the same result", {
  occdf <- head(tetrapods, n = 100)

  expect_equal(
    suppressMessages(occdf |> bin_space(bins = space_bins(spacing = 1000))),
    suppressMessages(bin_space(occdf, bins = space_bins(spacing = 1000)))
  )
})

test_that("bin_space errors with unnamed args", {
  occdf <- head(tetrapods, n = 100)

  expect_snapshot(
    bin_space(occdf, space_bins(spacing = 1000), "lng"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = occdf, space_bins(spacing = 1000), "lng"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf, space_bins(spacing = 1000), "lng", "lat"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf, space_bins(spacing = 1000), "lng", lat = "lat"),
    error = TRUE
  )
})

test_that("bin_space error handling", {
  # We modify this data so copy it first
  occdf <- tetrapods

  # wrong input type
  expect_snapshot(bin_space(occdf = matrix(tetrapods)), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, bins = NA), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, bins = 1:2), error = TRUE)

  # wrong columns
  expect_snapshot(
    bin_space(
      occdf = tetrapods,
      bins = space_bins(spacing = 1000),
      lng = "long",
      lat = "latit"
    ),
    error = TRUE
  )

  # lat must be a numeric value between -90 and 90
  occdf$lat[1] <- 94
  expect_snapshot(
    bin_space(occdf, bins = space_bins(spacing = 1000)),
    error = TRUE
  )
  occdf$lat[1] <- "94"
  expect_snapshot(
    bin_space(occdf, bins = space_bins(spacing = 1000)),
    error = TRUE
  )

  # lng must be a numeric value between -180 and 180
  occdf <- tetrapods
  occdf$lng[1] <- 184
  expect_snapshot(
    bin_space(occdf, bins = space_bins(spacing = 1000)),
    error = TRUE
  )
  occdf$lng[1] <- "184"
  expect_snapshot(
    bin_space(occdf, bins = space_bins(spacing = 1000)),
    error = TRUE
  )
})

test_that("plot() works", {
  skip_if_not_installed("rnaturalearthdata")
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 100)

  out <- suppressMessages(
    bin_space(occdf = occdf, bins = space_bins(spacing = 1000))
  )
  expect_doppelganger("bin_space", function() plot(out))
})

test_that("plot() works with several resolutions", {
  skip_if_not_installed("rnaturalearthdata")
  occdf <- head(tetrapods, n = 100)

  expect_doppelganger("bin_space-two-resolutions", function() {
    occdf |>
      bin_space(bins = space_bins(resolution = 2)) |>
      bin_space(bins = space_bins(resolution = 1)) |>
      suppressMessages() |>
      plot()
  })
  expect_doppelganger("bin_space-three-resolutions", function() {
    occdf |>
      bin_space(bins = space_bins(resolution = 1)) |>
      bin_space(bins = space_bins(resolution = 2)) |>
      bin_space(bins = space_bins(resolution = 3)) |>
      suppressMessages() |>
      plot()
  })

  # No cell columns
  out <- suppressMessages(
    bin_space(occdf = occdf, bins = space_bins(resolution = 1))
  )
  out$cell_ID_1 <- NULL
  expect_snapshot(plot(out), error = TRUE)
})

test_that("bin_space plotting with extra args works", {
  skip_if_not_installed("rnaturalearthdata")
  occdf <- head(tetrapods, n = 100)
  out <- suppressMessages(
    bin_space(occdf = occdf, bins = space_bins(spacing = 1000))
  )

  # `main` is passed through `...`
  expect_doppelganger("bin_space-dots", function() {
    plot(out, main = "hello there")
  })

  # forbidden args
  expect_snapshot(plot(out, add = TRUE), error = TRUE)
})

test_that("bin_space plotting with custom colours works", {
  skip_if_not_installed("rnaturalearthdata")
  occdf <- head(tetrapods, n = 100)
  out <- occdf |>
    bin_space(bins = space_bins(resolution = 1)) |>
    bin_space(bins = space_bins(resolution = 2)) |>
    suppressMessages()

  expect_doppelganger("bin_space-colour", function() {
    plot(out, col = "red")
  })
  expect_doppelganger("bin_space-colour-vector", function() {
    plot(out, col = c("red", "blue"))
  })

  expect_snapshot(plot(out, col = c("red", "blue", "green")), error = TRUE)
  expect_snapshot(plot(out, col = 1), error = TRUE)
})

test_that("bin_space plotting with custom centres works", {
  skip_if_not_installed("rnaturalearthdata")
  occdf <- head(tetrapods, n = 100)
  out <- suppressMessages(
    bin_space(occdf = occdf, bins = space_bins(spacing = 1000))
  )

  expect_doppelganger("bin_space-one-centre", function() {
    plot(out, centres = list(c(45, 10)))
  })
  expect_doppelganger("bin_space-four-centres", function() {
    plot(out, centres = list(c(0, 0), c(0, 180), c(90, 0), c(-90, 0)))
  })

  expect_snapshot(plot(out, centres = c(0, 0)), error = TRUE)
  expect_snapshot(plot(out, centres = rep(list(c(0, 0)), 5)), error = TRUE)
  expect_snapshot(plot(out, centres = list(c(100, 0))), error = TRUE)
})

test_that("plot is deprecated but still works", {
  skip_if_not_installed("rnaturalearthdata")
  occdf <- head(tetrapods, n = 100)

  expect_doppelganger("bin_space_deprecated", function() {
    expect_warning(
      bin_space(
        occdf = occdf,
        bins = space_bins(spacing = 1000),
        plot = TRUE
      ) |>
        suppressMessages(),
      "is deprecated as of palaeoverse 2.0.0",
      fixed = TRUE
    )
  })

  # input checks
  expect_snapshot(
    bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = "foo"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(
      occdf = occdf,
      bins = space_bins(spacing = 1000),
      plot = logical(0)
    ),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = occdf, bins = space_bins(spacing = 1000), plot = 1),
    error = TRUE
  )
})

test_that("spacing, sub_grid, and return give good error messages", {
  occdf <- head(tetrapods, n = 100)

  expect_snapshot(
    bin_space(occdf = occdf, spacing = 1000),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = occdf, sub_grid = 1000),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = occdf, bins = space_bins(spacing = 1000), return = TRUE),
    error = TRUE
  )
})
