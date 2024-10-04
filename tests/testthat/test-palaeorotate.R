test_that("palaeorotate() error handling works", {
  # No occdf provided
  expect_error(palaeorotate(occdf = 10))
  # Missing occdf column
  expect_error(palaeorotate(occdf = data.frame(lng = 10, lat = 5)))
  # Wrong data format
  expect_error(palaeorotate(occdf = data.frame(lng = 10, lat = 5, age = TRUE)))
  # Negative age values
  expect_error(palaeorotate(occdf = data.frame(lng = 10, lat = 5, age = -5)))
  # Incorrect latitudinal value
  expect_error(palaeorotate(occdf = data.frame(lng = 10, lat = 95, age = 25)))
  # Incorrect longitudinal value
  expect_error(palaeorotate(occdf = data.frame(lng = 210, lat = 40, age = 25)))
  # Wrong method
  expect_error(palaeorotate(occdf = data.frame(lng = 10, lat = 40, age = 25),
                            method = "API"))
  # Wrong `round` class
  expect_error(palaeorotate(occdf = data.frame(lng = 110, lat = 40, age = 25),
                            round = TRUE))
  # Wrong `uncertainty` class
  expect_error(palaeorotate(occdf = data.frame(lng = 110, lat = 40, age = 25),
                            uncertainty = "GOONTHEN"))
})

test_that("palaeorotate() point method works", {
  skip_if_offline(host = "gws.gplates.org")

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 300))

  # Same number of rows returned
  expect_equal(nrow(palaeorotate(occdf = occdf, model = "PALEOMAP")), 3)

  # Check that multiple models are being returned
  occdf <- palaeorotate(occdf = occdf,
                        model = c("PALEOMAP", "GOLONKA"))
  expect_true(all(c("p_lng_PALEOMAP", "p_lat_PALEOMAP",
                    "p_lng_GOLONKA", "p_lat_GOLONKA") %in%
                    colnames(occdf)))

  # Check chunk size is working
  set.seed(0)
  occdf <- data.frame(lng = runif(1500, -180, 180),
                      lat = runif(1500, -90, 90),
                      age = rep(100, 1500))
  expect_warning(
  expect_true(nrow(palaeorotate(occdf = occdf, model = "PALEOMAP")) == 1500),
  regexp = "Palaeocoordinates")

  # Check handling of temporal range (NAs should be returned)
  occdf <- data.frame(lng = runif(10, -180, 180),
                      lat = runif(10, -90, 90),
                      age = rep(700, 10))
  expect_warning(
  expect_true(all(is.na(palaeorotate(occdf = occdf, model = "GOLONKA")$p_lng))),
    regexp = "Palaeocoordinates")
  # Check all NA for uncertainty when one model is outside range
  expect_warning(
  expect_true(all(is.na(palaeorotate(occdf = occdf,
                                     model = c("PALEOMAP", "GOLONKA"),
                                     uncertainty = TRUE)$max_dist))),
    regexp = "Palaeocoordinates")

  ## Previously available models removed
  expect_error(palaeorotate(occdf = occdf, method = "point",
                            model = "MULLER2022"))
  ## Not a model
  expect_error(palaeorotate(occdf = occdf, method = "point",
                            model = "GPlates"))

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 700))

  # Expect warnings
  expect_warning(palaeorotate(occdf = occdf, method = "point",
                              model = c("GOLONKA", "PALEOMAP")))
})

test_that("palaeorotate() grid method works", {
  skip_if_offline(host = "zenodo.org")

  occdf <- data.frame(lng = c(2, -103, -66),
                      lat = c(46, 35, -7),
                      age = c(88, 125, 300))

  # Same number of rows returned
  expect_equal(nrow(palaeorotate(occdf = occdf, model = "PALEOMAP",
                                 method = "grid")), 3)

  # Check that multiple models are being returned
  occdf <- palaeorotate(occdf = occdf,
                        method = "grid",
                        model = c("PALEOMAP", "GOLONKA"))
  expect_true(all(c("p_lng_PALEOMAP", "p_lat_PALEOMAP",
                    "p_lng_GOLONKA", "p_lat_GOLONKA") %in%
                    colnames(occdf)))

  # Check handling of temporal range (NAs should be returned)
  occdf <- data.frame(lng = runif(10, -180, 180),
                      lat = runif(10, -90, 90),
                      age = rep(700, 10))
  expect_warning(
  expect_true(all(is.na(palaeorotate(occdf = occdf, model = "GOLONKA",
                                     method = "grid")$p_lng))),
  regexp = "Palaeocoordinates")
  # Check all NA for uncertainty when one model is outside range
  expect_warning(
  expect_true(all(is.na(palaeorotate(occdf = occdf,
                                     model = c("PALEOMAP", "GOLONKA"),
                                     method = "grid",
                                     uncertainty = TRUE)$max_dist))),
  regexp = "Palaeocoordinates")
})
