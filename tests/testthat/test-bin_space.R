test_that("bin_space() works", {
  occdf <- tetrapods
  lng <- "lng"
  lat <- "lat"
  spacing <- 1000
  sub_grid <- 250
  return <- FALSE
  plot <- FALSE

  # Expect equal
  expect_equal(
    nrow(bin_space(occdf = tetrapods, spacing = 250, plot = TRUE)),
    nrow(tetrapods)
  )
  expect_equal(
    nrow(bin_space(
      occdf = tetrapods,
      spacing = 1000,
      sub_grid = 250,
      plot = TRUE
    )),
    nrow(tetrapods)
  )

  # Expect true
  expect_true(
    is.list(
      bin_space(
        occdf = tetrapods,
        spacing = 250,
        return = TRUE,
        plot = TRUE
      )
    )
  )

  expect_true(
    is.list(
      bin_space(
        occdf = tetrapods,
        spacing = 500,
        sub_grid = 200,
        return = TRUE,
        plot = TRUE
      )
    )
  )

  # Error handling
  expect_snapshot(bin_space(occdf = matrix(tetrapods)), error = TRUE)
  expect_snapshot(
    bin_space(occdf = tetrapods, lng = "long", lat = "latit"),
    error = TRUE
  )
  expect_snapshot(
    bin_space(occdf = tetrapods, spacing = 1000, sub_grid = 1000),
    error = TRUE
  )
  expect_snapshot(bin_space(occdf = tetrapods, spacing = NA), error = TRUE)
  expect_snapshot(
    bin_space(occdf = tetrapods, spacing = 1000, sub_grid = NA),
    error = TRUE
  )
  expect_snapshot(bin_space(occdf = tetrapods, return = "TRUE"), error = TRUE)
  tetrapods$lat[1] <- 94
  expect_snapshot(bin_space(occdf = tetrapods), error = TRUE)
  tetrapods$lat[1] <- "94"
  expect_snapshot(bin_space(occdf = tetrapods), error = TRUE)
  tetrapods$lng[1] <- 184
  expect_snapshot(bin_space(occdf = tetrapods), error = TRUE)
})
