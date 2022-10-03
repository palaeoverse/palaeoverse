test_that("palaeorotate() works", {

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 200))

  expect_equal(nrow(palaeorotate(occdf = occdf)), 3)

  expect_equal(
    ncol(
      palaeorotate(occdf = occdf,
                   uncertainty = TRUE)[, c("range_p_lat",
                                           "max_dist")]), 2)

  expect_equal(nrow(palaeorotate(occdf = occdf, model = "PALEOMAP")), 3)

  expect_equal(nrow(palaeorotate(occdf = occdf, model = "MERDITH2021")), 3)

  expect_equal(nrow(palaeorotate(occdf = occdf, method = "point")), 3)

  expect_equal(nrow(palaeorotate(occdf = occdf, method = "point",
                                 model = "MERDITH2021")), 3)
  expect_equal(nrow(palaeorotate(occdf = occdf, method = "point",
                                 round = 2)), 3)

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 400))

  # Expect message
  # Model does not extend to this timeframe
  msg <-
    paste0("Palaeocoordinates equal to input coordinates detected.",
           "\n",
           "Check desired model covers the temporal range of your data."
    )

  expect_message(palaeorotate(occdf = occdf,
                              method = "point",
                              model = "SETON2012"),
                 msg)
  # Plate polygon does not exist at time
  msg <-
    paste0("Palaeocoordinates could not be reconstructed for all points.",
           "\n",
           "Georeferenced plate does not exist at time of reconstruction."
    )

  occdf <- data.frame(lng = c(-41),
                      lat = c(37),
                      age = c(300))

  expect_message(palaeorotate(occdf = occdf,
                              method = "point",
                              model = "MERDITH2021"),
                 msg)

  # Wrong uncertainty input
  expect_error(palaeorotate(occdf = occdf, uncertainty = "TRUE"))

  # Wrong uncertainty input
  expect_error(palaeorotate(occdf = occdf, uncertainty = 2))

  # Model doesn't exist
  expect_error(palaeorotate(occdf = occdf, model = "Mirdith2021"))

  # Model doesn't exist
  expect_error(palaeorotate(occdf = occdf, method = "point",
                            model = "Mirdith2021"))

  expect_error(palaeorotate(occdf = occdf, method = "point",
                            model = "WRIGHT2013"))

  # Method doesn't exist
  expect_error(palaeorotate(occdf = occdf, method = "both"))

  expect_error(palaeorotate(occdf = occdf, lng = 1))

  expect_error(palaeorotate(occdf = c(55, 46, 88)))

  # Column names not correct
  occdf <- data.frame(x = c(2, 95, 12), y = c(46, 12, -65),
                      age = c(88, 203, 467))
  expect_error(palaeorotate(x = occdf))

  # Values not numeric
  occdf <- data.frame(lng = c(2, 95, "12"),
                      lat = c(46, 12, -65),
                      age = c(88, 203, 467))
  expect_error(palaeorotate(occdf = occdf))

  # Longitude too small
  occdf <- data.frame(lng = c(2, 95, -183),
                      lat = c(46, 12, -65),
                      age = c(88, 203, 467))
  expect_error(palaeorotate(occdf = occdf))

  # Latitude too great
  occdf <- data.frame(lng = c(2, 95, -178),
                      lat = c(46, 95, -65),
                      age = c(88, 203, 467))
  expect_error(palaeorotate(occdf = occdf))

})

