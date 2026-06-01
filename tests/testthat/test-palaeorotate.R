test_that("palaeorotate() error handling works", {
  # No occdf provided
  expect_snapshot(palaeorotate(occdf = 10), error = TRUE)
  # Missing occdf column
  expect_snapshot(
    palaeorotate(occdf = data.frame(lng = 10, lat = 5)),
    error = TRUE
  )
  # Wrong data format
  expect_snapshot(
    palaeorotate(occdf = data.frame(lng = 10, lat = 5, age = TRUE)),
    error = TRUE
  )
  # Negative age values
  expect_snapshot(
    palaeorotate(occdf = data.frame(lng = 10, lat = 5, age = -5)),
    error = TRUE
  )
  # Incorrect latitudinal value
  expect_snapshot(
    palaeorotate(occdf = data.frame(lng = 10, lat = 95, age = 25)),
    error = TRUE
  )
  # Incorrect longitudinal value
  expect_snapshot(
    palaeorotate(occdf = data.frame(lng = 210, lat = 40, age = 25)),
    error = TRUE
  )
  # Wrong method
  expect_snapshot(
    palaeorotate(
      occdf = data.frame(lng = 10, lat = 40, age = 25),
      method = "API"
    ),
    error = TRUE
  )
  # Wrong `round` class
  expect_snapshot(
    palaeorotate(
      occdf = data.frame(lng = 110, lat = 40, age = 25),
      round = TRUE
    ),
    error = TRUE
  )
  # Wrong `uncertainty` class
  expect_snapshot(
    palaeorotate(
      occdf = data.frame(lng = 110, lat = 40, age = 25),
      uncertainty = "GOONTHEN"
    ),
    error = TRUE
  )
})

test_that("palaeorotate() point method works", {
  skip_if_offline(host = "gws.gplates.org")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  vcr::use_cassette("palaeorotate-paleomap", {
    paleomap <- palaeorotate(occdf = occdf, model = "PALEOMAP")
  })

  # Same number of rows returned
  expect_equal(nrow(paleomap), 3)

  # Check that multiple models are being returned
  vcr::use_cassette("palaeorotate-multi", {
    multi <- palaeorotate(occdf = occdf, model = c("PALEOMAP", "GOLONKA"))
  })
  expect_true(all(
    c("p_lng_PALEOMAP", "p_lat_PALEOMAP", "p_lng_GOLONKA", "p_lat_GOLONKA") %in%
      colnames(multi)
  ))

  # Check chunk size is working
  set.seed(0)
  occdf <- data.frame(
    lng = runif(1500, -180, 180),
    lat = runif(1500, -90, 90),
    age = rep(100, 1500)
  )

  expect_warning(
    vcr::use_cassette("palaeorotate-paleomap-chunksize", {
      paleomap <- palaeorotate(occdf = occdf, model = "PALEOMAP")
    }),
    regexp = "Palaeocoordinates"
  )

  expect_true(nrow(paleomap) == 1500)

  # Check handling of temporal range (NAs should be returned)
  occdf <- data.frame(
    lng = runif(10, -180, 180),
    lat = runif(10, -90, 90),
    age = rep(700, 10)
  )

  expect_warning(
    vcr::use_cassette("palaeorotate-temporal", {
      paleomap <- palaeorotate(occdf = occdf, model = "GOLONKA")$p_lng
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(paleomap)))

  # Check all NA for uncertainty when one model is outside range
  expect_warning(
    vcr::use_cassette("palaeorotate-multi-outside-range", {
      paleomap <- palaeorotate(
        occdf = occdf,
        model = c("PALEOMAP", "GOLONKA"),
        uncertainty = TRUE
      )$max_dist
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(paleomap)))

  ## Previously available models removed
  expect_snapshot(
    palaeorotate(
      occdf = occdf,
      method = "point",
      model = "MULLER2022"
    ),
    error = TRUE
  )
  ## Not a model
  expect_snapshot(
    palaeorotate(occdf = occdf, method = "point", model = "GPlates"),
    error = TRUE
  )

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 700)
  )

  # Expect warnings
  expect_warning(
    vcr::use_cassette("palaeorotate-multi-point", {
      paleomap <- palaeorotate(
        occdf = occdf,
        method = "point",
        model = c("GOLONKA", "PALEOMAP")
      )
    }),
    regexp = "Palaeocoordinates"
  )
})

test_that("palaeorotate() grid method works", {
  skip_if_offline(host = "zenodo.org")
  skip_on_cran()

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )
  # Same number of rows returned
  expect_equal(
    nrow(palaeorotate(occdf = occdf, model = "PALEOMAP", method = "grid")),
    3
  )

  # Check that multiple models are being returned
  grid_multi <- palaeorotate(
    occdf = occdf,
    method = "grid",
    model = c("PALEOMAP", "GOLONKA")
  )
  expect_true(all(
    c("p_lng_PALEOMAP", "p_lat_PALEOMAP", "p_lng_GOLONKA", "p_lat_GOLONKA") %in%
      colnames(grid_multi)
  ))

  # Check handling of temporal range (NAs should be returned)
  occdf <- data.frame(
    lng = runif(10, -180, 180),
    lat = runif(10, -90, 90),
    age = rep(700, 10)
  )
  expect_warning(
    vcr::use_cassette("palaeorotate-grid-temporal", {
      grid_temporal <- palaeorotate(
        occdf = occdf,
        model = "GOLONKA",
        method = "grid"
      )
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(grid_temporal$p_lng)))

  # Check all NA for uncertainty when one model is outside range
  expect_warning(
    vcr::use_cassette("palaeorotate-grid-temporal-outside-range", {
      grid_temporal <- palaeorotate(
        occdf = occdf,
        model = c("PALEOMAP", "GOLONKA"),
        method = "grid",
        uncertainty = TRUE
      )
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(grid_temporal$max_dist)))
})
