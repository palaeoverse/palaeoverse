test_that("palaeorotate() point method works", {
  skip_if_offline(host = "gwsdoc.gplates.org")

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 300))

  expect_equal(nrow(palaeorotate(occdf = occdf,
                                 method = "point",
                                 model = "PALEOMAP")), 3)

  expect_equal(nrow(palaeorotate(occdf = occdf,
                                 method = "point",
                                 model = "PALEOMAP")), 3)

  expect_equal(nrow(palaeorotate(occdf = occdf, method = "point",
                                 round = 2)), 3)

  # Expect warning
  expect_warning(palaeorotate(occdf = occdf,
                              method = "point",
                              model = "MULLER2022"), NULL)

  # Check that multiple models are being returned
  occdf <- palaeorotate(occdf = occdf,
                        method = "point",
                        model = c("PALEOMAP", "GOLONKA"),
                        uncertainty = FALSE)
  expect_false(any(is.na(occdf)))

  # Check chunk size is working
  occdf <- data.frame(lng = runif(1500, -180, 180),
                      lat = runif(1500, -90, 90),
                      age = rep(100, 1500))

  expect_warning(occdf <- palaeorotate(occdf = occdf,
                                       method = "point",
                                       model = "PALEOMAP",
                                       round = 3), NULL)
  # Filter out points that can't be reconstructed
  occdf <- occdf[-which(is.na(occdf$p_lng)), ]

  expect_equal(nrow(palaeorotate(occdf = occdf,
                                 method = "point",
                                 model = "PALEOMAP")), nrow(occdf))

  # Check handling of temporal range
  occdf <- data.frame(lng = runif(10, -180, 180),
                      lat = runif(10, -90, 90),
                      age = rep(500, 10))
  expect_true(
    all(
      is.na(
        palaeorotate(occdf = occdf,
                     method = "point",
                     model = "SETON2012")$p_lng
        ))
  )

  expect_true(
    all(
      is.na(
        palaeorotate(occdf = occdf,
                     method = "point",
                     model = c("SETON2012", "MULLER2016"),
                     uncertainty = TRUE)$max_dist
      )
    )
  )

  # Expect error
  occdf <- data.frame(lng = c(-41),
                      lat = c(37),
                      age = c(300))

  # Model doesn't exist
  expect_error(palaeorotate(occdf = occdf, method = "point",
                            model = "Mirdith2021"))

  expect_error(palaeorotate(occdf = occdf, method = "point",
                            model = "WRIGHT2013"))
})

test_that("palaeorotate() grid method works", {
  skip_if_offline(host = "zenodo.org")

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 200))

  expect_equal(nrow(palaeorotate(occdf = occdf)), 3)

  expect_equal(
    ncol(
      palaeorotate(occdf = occdf,
                   uncertainty = TRUE,
                   method = "grid",
                   model = c("MERDITH2021",
                                   "GOLONKA",
                                   "SETON2012"))[, c("range_p_lat",
                                           "max_dist")]), 2)

  expect_equal(nrow(palaeorotate(occdf = occdf, method = "grid",
                                 model = "PALEOMAP")), 3)

  expect_equal(nrow(palaeorotate(occdf = occdf, method = "grid",
                                 model = "MERDITH2021")), 3)



  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 400))

  # Expect warnings
  msg <- "Palaeocoordinates could not be reconstructed for all points."

  expect_warning(palaeorotate(occdf = occdf, method = "grid",
                              model = "SETON2012"), msg)

  # Wrong uncertainty input
  expect_error(palaeorotate(occdf = occdf, method = "grid",
                            uncertainty = c("GALONKA",
                                                           "MIRDITH2021")))
  expect_error(palaeorotate(occdf = occdf, method = "grid",
                            uncertainty = c("GOLONKA")))

  # Wrong uncertainty input
  expect_error(palaeorotate(occdf = occdf, method = "grid",
                            uncertainty = 2))

  # Model doesn't exist
  expect_error(palaeorotate(occdf = occdf, method = "grid",
                            model = "Mirdith2021"))

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

  # Negative values
  occdf <- data.frame(lng = c(2, 95, -178),
                      lat = c(46, 95, -65),
                      age = c(-88, 203, 467))
  expect_error(palaeorotate(occdf = occdf))

  # Incorrect round values
  occdf <- data.frame(lng = c(2, 95, -178),
                      lat = c(46, 50, -65),
                      age = c(88, 203, 467))
  expect_error(palaeorotate(occdf = occdf, round = "TEST"))

})
