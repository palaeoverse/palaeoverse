test_that("bin_space() works", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 100)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(occdf = occdf, bins = space_bins(250))),
      nrow(occdf)
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_equal(
      occdf |>
        bin_space(bins = space_bins(1000)) |>
        bin_space(bins = space_bins(250)) |>
        nrow(),
      nrow(occdf)
    ),
    "H3 resolution: 1"
  )
})

test_that("piping and not piping the first and second arguments give the same result", {
  occdf <- head(tetrapods, n = 100)

  expect_equal(
    suppressMessages(occdf |> bin_space(bins = space_bins(1000))),
    suppressMessages(bin_space(occdf, bins = space_bins(1000)))
  )

  # TODO: this should work
  # expect_equal(
  #   suppressMessages(bins |> bin_space(occdf = occdf, space_bins(1000))),
  #   suppressMessages(bin_space(occdf, bins = space_bins(1000)))
  # )
  # expect_equal(
  #   suppressMessages(bins |> bin_space(occdf, space_bins(250))),
  #   suppressMessages(bin_space(occdf, bins = space_bins(250)))
  # )
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

  # TODO: should error
  # expect_snapshot(
  #   tetrapods |> bin_space(space_bins(1000)) |> bin_space(space_bins(1000)),
  #   error = TRUE
  # )

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
