test_that("bin_space() works", {
  # Reduce data size for faster testing
  data <- head(tetrapods, n = 100)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(data = data, spacing = 250, plot = TRUE)),
      nrow(data)
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_equal(
      nrow(
        bin_space(data = data, spacing = 1000, sub_grid = 250, plot = TRUE)
      ),
      nrow(data)
    ),
    "H3 resolution: 1"
  )

  # Check output type
  expect_message(
    expect_type(
      bin_space(data = data, spacing = 250, return = TRUE, plot = TRUE),
      "list"
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_type(
      bin_space(
        data = data,
        spacing = 500,
        sub_grid = 200,
        return = TRUE,
        plot = TRUE
      ),
      "list"
    ),
    "H3 resolution: 1"
  )
})

test_that("piping and not piping the first argument give the same result", {
  data <- head(tetrapods, n = 100)

  expect_equal(
    suppressMessages(data |> bin_space(spacing = 250)),
    suppressMessages(bin_space(data, spacing = 250))
  )
})

test_that("bin_space errors with unnamed args", {
  data <- head(tetrapods, n = 100)

  expect_snapshot(bin_space(data, "lng"), error = TRUE)
  expect_snapshot(bin_space(data = data, "lng"), error = TRUE)
  expect_snapshot(bin_space(data, "lng", "lat"), error = TRUE)
  expect_snapshot(bin_space(data, "lng", lat = "lat"), error = TRUE)
})

test_that("bin_space error handling", {
  # We modify this data so copy it first
  data <- tetrapods

  # wrong input type
  expect_snapshot(bin_space(data = matrix(tetrapods)), error = TRUE)
  expect_snapshot(bin_space(data = tetrapods, spacing = NA), error = TRUE)
  expect_snapshot(bin_space(data = tetrapods, spacing = 1:2), error = TRUE)
  expect_snapshot(bin_space(data = tetrapods, sub_grid = 1:2), error = TRUE)
  expect_snapshot(
    bin_space(data = tetrapods, spacing = 1000, sub_grid = NA),
    error = TRUE
  )
  expect_snapshot(bin_space(data = tetrapods, return = "TRUE"), error = TRUE)

  # wrong columns
  expect_snapshot(
    bin_space(data = tetrapods, lng = "long", lat = "latit"),
    error = TRUE
  )

  # spacing and sub_grid give the same resolution
  expect_snapshot(
    bin_space(data = tetrapods, spacing = 1000, sub_grid = 1000),
    error = TRUE
  )
  # lat must be a numeric value between -90 and 90
  data$lat[1] <- 94
  expect_snapshot(bin_space(data = data), error = TRUE)
  data$lat[1] <- "94"
  expect_snapshot(bin_space(data = data), error = TRUE)

  # lng must be a numeric value between -180 and 180
  data <- tetrapods
  data$lng[1] <- 184
  expect_snapshot(bin_space(data = data), error = TRUE)
  data$lng[1] <- "184"
  expect_snapshot(bin_space(data = data), error = TRUE)
})
