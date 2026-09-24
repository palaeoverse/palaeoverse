test_that("bin_space() works", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 50)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(occdf = occdf, bins = space_bins(250))),
      nrow(occdf)
    ),
    "H3 resolution: 2"
  )

  # three new columns: cell id, centroid lat, centroid lon
  expect_named(
    bin_space(occdf = occdf, bins = space_bins(250)) |>
      suppressMessages(),
    c(
      names(occdf),
      "cell_ID_250",
      "cell_centroid_lat_250",
      "cell_centroid_lng_250"
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
          bin_space(bins = space_bins(1000)) |>
          bin_space(bins = space_bins(250)) |>
          nrow(),
        nrow(occdf)
      ),
      "H3 resolution: 2"
    ),
    "H3 resolution: 1"
  )
  expect_named(
    occdf |>
      bin_space(bins = space_bins(1000)) |>
      bin_space(bins = space_bins(250)) |>
      suppressMessages(),
    c(
      names(occdf),
      "cell_ID_1000",
      "cell_centroid_lat_1000",
      "cell_centroid_lng_1000",
      "cell_ID_250",
      "cell_centroid_lat_250",
      "cell_centroid_lng_250"
    )
  )

  # We can chain from larger to smaller spacing, or from smaller to larger (column ordering
  # is the only thing that changes)
  large_then_small <- occdf |>
    bin_space(bins = space_bins(1000)) |>
    bin_space(bins = space_bins(250)) |>
    suppressMessages()
  small_then_large <- occdf |>
    bin_space(bins = space_bins(250)) |>
    bin_space(bins = space_bins(1000)) |>
    suppressMessages()

  expect_equal(
    large_then_small,
    small_then_large[, c(
      names(occdf),
      "cell_ID_1000",
      "cell_centroid_lat_1000",
      "cell_centroid_lng_1000",
      "cell_ID_250",
      "cell_centroid_lat_250",
      "cell_centroid_lng_250"
    )]
  )
})

test_that("piping and not piping the first argument give the same result", {
  occdf <- head(tetrapods, n = 100)

  expect_equal(
    suppressMessages(occdf |> bin_space(bins = space_bins(1000))),
    suppressMessages(bin_space(occdf, bins = space_bins(1000)))
  )
})

test_that("bin_space errors with unnamed args", {
  occdf <- head(tetrapods, n = 100)

  expect_snapshot(bin_space(occdf, space_bins(1000), "lng"), error = TRUE)
  expect_snapshot(
    bin_space(occdf = occdf, space_bins(1000), "lng"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf, space_bins(1000), "lng", "lat"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf, space_bins(1000), "lng", lat = "lat"),
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
      bins = space_bins(1000),
      lng = "long",
      lat = "latit"
    ),
    error = TRUE
  )

  # lat must be a numeric value between -90 and 90
  occdf$lat[1] <- 94
  expect_snapshot(bin_space(occdf, space_bins(1000)), error = TRUE)
  occdf$lat[1] <- "94"
  expect_snapshot(bin_space(occdf, space_bins(1000)), error = TRUE)

  # lng must be a numeric value between -180 and 180
  occdf <- tetrapods
  occdf$lng[1] <- 184
  expect_snapshot(bin_space(occdf, space_bins(1000)), error = TRUE)
  occdf$lng[1] <- "184"
  expect_snapshot(bin_space(occdf, space_bins(1000)), error = TRUE)
})

test_that("plot argument works", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 100)

  expect_doppelganger("bin_space", {
    out <- bin_space(occdf = occdf, bins = space_bins(1000), plot = TRUE)
  })

  # input checks
  expect_snapshot(
    bin_space(occdf = occdf, bins = space_bins(1000), plot = "foo"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = occdf, bins = space_bins(1000), plot = logical(0)),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = occdf, bins = space_bins(1000), plot = 1),
    error = TRUE
  )
})

test_that("using defunct arguments gives a good error message", {
  expect_snapshot(
    bin_space(tetrapods, spacing = 1000),
    error = TRUE
  )
  expect_snapshot(
    bin_space(tetrapods, sub_grid = 1000),
    error = TRUE
  )
  expect_snapshot(
    bin_space(tetrapods, return = TRUE),
    error = TRUE
  )
})
