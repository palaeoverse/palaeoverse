test_that("bin_space() works", {
  # Reduce data size for faster testing
  occdf <- head(tetrapods, n = 100)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_space(occdf = occdf, spacing = 250, plot = TRUE)),
      nrow(occdf)
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_equal(
      nrow(
        bin_space(occdf = occdf, spacing = 1000, sub_grid = 250, plot = TRUE)
      ),
      nrow(occdf)
    ),
    "H3 resolution: 1"
  )

  # Check output type
  expect_message(
    expect_type(
      bin_space(occdf = occdf, spacing = 250, return = TRUE, plot = TRUE),
      "list"
    ),
    "H3 resolution: 2"
  )
  expect_message(
    expect_type(
      bin_space(
        occdf = occdf,
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

test_that("bin_space error handling", {
  # We modify this data so copy it first
  occdf <- tetrapods

  # wrong input type
  expect_snapshot(bin_space(occdf = matrix(tetrapods)), error = TRUE)
  expect_snapshot(bin_space(occdf = tetrapods, spacing = NA), error = TRUE)
  expect_snapshot(
    bin_space(occdf = tetrapods, spacing = 1000, sub_grid = NA),
    error = TRUE
  )
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
