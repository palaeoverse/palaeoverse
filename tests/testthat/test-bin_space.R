test_that("bin_space() works", {
  # Reduce data size for faster testing
  data <- head(tetrapods, n = 50)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(data = data, bins = space_bins(spacing = 250))),
      nrow(data)
    ),
    "H3 resolution: 2"
  )

  # three new columns: cell id, centroid lat, centroid lon
  expect_named(
    bin_space(data = data, bins = space_bins(spacing = 250)) |>
      suppressMessages(),
    c(
      names(data),
      "cell_ID_2",
      "cell_centroid_lng_2",
      "cell_centroid_lat_2"
    )
  )
})

test_that("we can chain several bin_space()", {
  # Reduce data size for faster testing
  data <- head(tetrapods, n = 50)

  expect_message(
    expect_message(
      expect_equal(
        data |>
          bin_space(bins = space_bins(spacing = 1000)) |>
          bin_space(bins = space_bins(spacing = 250)) |>
          nrow(),
        nrow(data)
      ),
      "H3 resolution: 2"
    ),
    "H3 resolution: 1"
  )
  expect_named(
    data |>
      bin_space(bins = space_bins(spacing = 1000)) |>
      bin_space(bins = space_bins(spacing = 250)) |>
      suppressMessages(),
    c(
      names(data),
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
  large_then_small <- data |>
    bin_space(bins = space_bins(spacing = 1000)) |>
    bin_space(bins = space_bins(spacing = 250)) |>
    suppressMessages()
  small_then_large <- data |>
    bin_space(bins = space_bins(spacing = 250)) |>
    bin_space(bins = space_bins(spacing = 1000)) |>
    suppressMessages()

  expect_equal(
    large_then_small,
    small_then_large[, c(
      names(data),
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
  data <- head(tetrapods, n = 50)

  expect_snapshot(
    out <- data |>
      bin_space(bins = space_bins(spacing = 1000)) |>
      bin_space(bins = space_bins(spacing = 900))
  )
  expect_named(
    out,
    c(names(data), "cell_ID_1", "cell_centroid_lng_1", "cell_centroid_lat_1")
  )
})

test_that("piping and not piping the first argument give the same result", {
  data <- head(tetrapods, n = 100)

  expect_equal(
    suppressMessages(data |> bin_space(bins = space_bins(spacing = 1000))),
    suppressMessages(bin_space(data, bins = space_bins(spacing = 1000)))
  )
})

test_that("bin_space errors with unnamed args", {
  data <- head(tetrapods, n = 100)

  expect_snapshot(
    bin_space(data, space_bins(spacing = 1000), "lng"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(data = data, space_bins(spacing = 1000), "lng"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(data, space_bins(spacing = 1000), "lng", "lat"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(data, space_bins(spacing = 1000), "lng", lat = "lat"),
    error = TRUE
  )
})

test_that("bin_space error handling", {
  # We modify this data so copy it first
  data <- tetrapods

  # wrong input type
  expect_snapshot(bin_space(data = matrix(tetrapods)), error = TRUE)
  expect_snapshot(bin_space(data = tetrapods, bins = NA), error = TRUE)
  expect_snapshot(bin_space(data = tetrapods, bins = 1:2), error = TRUE)

  # wrong columns
  expect_snapshot(
    bin_space(
      data = tetrapods,
      bins = space_bins(spacing = 1000),
      lng = "long",
      lat = "latit"
    ),
    error = TRUE
  )

  # lat must be a numeric value between -90 and 90
  data$lat[1] <- 94
  expect_snapshot(
    bin_space(data, bins = space_bins(spacing = 1000)),
    error = TRUE
  )
  data$lat[1] <- "94"
  expect_snapshot(
    bin_space(data, bins = space_bins(spacing = 1000)),
    error = TRUE
  )

  # lng must be a numeric value between -180 and 180
  data <- tetrapods
  data$lng[1] <- 184
  expect_snapshot(
    bin_space(data, bins = space_bins(spacing = 1000)),
    error = TRUE
  )
  data$lng[1] <- "184"
  expect_snapshot(
    bin_space(data, bins = space_bins(spacing = 1000)),
    error = TRUE
  )
})

test_that("plot argument works", {
  # Reduce data size for faster testing
  data <- head(tetrapods, n = 100)

  expect_doppelganger("bin_space", {
    out <- bin_space(
      data = data,
      bins = space_bins(spacing = 1000),
      plot = TRUE
    )
  })

  # input checks
  expect_snapshot(
    bin_space(data = data, bins = space_bins(spacing = 1000), plot = "foo"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(
      data = data,
      bins = space_bins(spacing = 1000),
      plot = logical(0)
    ),
    error = TRUE
  )
  expect_snapshot(
    bin_space(data = data, bins = space_bins(spacing = 1000), plot = 1),
    error = TRUE
  )
})

test_that("spacing, sub_grid, and return give good error messages", {
  data <- head(tetrapods, n = 100)

  expect_snapshot(
    bin_space(data = data, spacing = 1000),
    error = TRUE
  )
  expect_snapshot(
    bin_space(data = data, sub_grid = 1000),
    error = TRUE
  )
  expect_snapshot(
    bin_space(data = data, bins = space_bins(spacing = 1000), return = TRUE),
    error = TRUE
  )
})
