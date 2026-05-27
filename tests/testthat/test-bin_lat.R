test_that("bin_lat works", {
  # Load occurrence data
  occdf <- tetrapods
  # Generate latitudinal bins
  bins <- lat_bins_degrees(size = 10)
  # Bin data
  expect_equal(
    nrow(bin_lat(occdf = occdf, bins = bins, lat = "lat")),
    nrow(occdf)
  )

  # Boundary occurrences
  bo <- length(which(occdf[, "lat", drop = TRUE] %in% c(bins$max, bins$min)))
  expect_equal(
    nrow(bin_lat(occdf = occdf, bins = bins, lat = "lat", boundary = TRUE)),
    nrow(occdf) + bo
  )

  # Expect error
  expect_snapshot(bin_lat(occdf = 2, bins = bins, lat = "lat"), error = TRUE)
  expect_snapshot(bin_lat(occdf = occdf, bins = 2, lat = "lat"), error = TRUE)
  expect_snapshot(bin_lat(occdf = occdf, bins = 2, lat = "plat"), error = TRUE)
  bins <- bins[, -c(1)]
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "lat"),
    error = TRUE
  )
  occdf$lat[1] <- NA
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "lat"),
    error = TRUE
  )
  occdf$lat[1] <- 91
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "lat"),
    error = TRUE
  )
  expect_snapshot(
    bin_lat(occdf = occdf, bins = bins, lat = "latitude"),
    error = TRUE
  )
})
