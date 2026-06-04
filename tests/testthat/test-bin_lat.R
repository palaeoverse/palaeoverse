test_that("bin_lat works", {
  # Generate latitudinal bins
  bins <- lat_bins_degrees(size = 10)

  # We don't lose or gain observations
  expect_message(
    expect_equal(
      nrow(bin_lat(occdf = tetrapods, bins = bins, lat = "lat")),
      nrow(tetrapods)
    ),
    "Occurrences assigned to upper bin"
  )

  # argument "boundary" works
  bo <- length(which(
    tetrapods[, "lat", drop = TRUE] %in% c(bins$max, bins$min)
  ))
  expect_equal(
    nrow(bin_lat(occdf = tetrapods, bins = bins, lat = "lat", boundary = TRUE)),
    nrow(tetrapods) + bo
  )
})

test_that("bin_lat error handling", {
  # We modify this data so copy it first
  occdf <- tetrapods
  # Generate latitudinal bins
  bins <- lat_bins_degrees(size = 10)

  # occdf and bins should be dataframes
  expect_snapshot(bin_lat(occdf = 2, bins = bins, lat = "lat"), error = TRUE)
  expect_snapshot(
    bin_lat(occdf = occdf, bins = 2, lat = "lat"),
    error = TRUE
  )

  # column "lat" must exist in the data
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "plat"),
    error = TRUE
  )

  # columns "bin", "max", and "min" must exist in bins
  for (i in c("bin", "min", "max")) {
    bins2 <- bins
    bins2[[i]] <- NULL
    expect_snapshot(
      bin_lat(occdf = occdf, bins = bins2, lat = "lat"),
      error = TRUE
    )
  }

  # lat cannot have missing values
  occdf$lat[1] <- NA
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "lat"),
    error = TRUE
  )

  # lat must be between -90 and 90
  occdf$lat[1] <- 91
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "lat"),
    error = TRUE
  )
})
