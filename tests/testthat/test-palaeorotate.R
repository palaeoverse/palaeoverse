test_that("arg 'occdf' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  # A valid occdf returns the same number of rows
  vcr::use_cassette("palaeorotate-paleomap", {
    paleomap <- palaeorotate_point(occdf = occdf, model = "PALEOMAP")
  })
  expect_equal(nrow(paleomap), 3)

  # input checks
  expect_snapshot(palaeorotate_point(occdf = 10), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf = NA), error = TRUE)
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 10, lat = 5)),
    error = TRUE
  )
})

test_that("piping and not piping the first argument give the same result", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  vcr::use_cassette("palaeorotate-paleomap", {
    piped <- occdf |> palaeorotate_point(model = "PALEOMAP")
  })
  vcr::use_cassette("palaeorotate-paleomap", {
    not_piped <- palaeorotate_point(occdf, model = "PALEOMAP")
  })

  expect_equal(piped, not_piped)
})

test_that("palaeorotate_point() errors with unnamed args", {
  occdf <- data.frame(lng = c(2, -103), lat = c(46, 35), age = c(88, 125))
  expect_snapshot(palaeorotate_point(occdf, "lng"), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf = occdf, "lng"), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf, "lng", "lat"), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf, "lng", lat = "lat"), error = TRUE)
})

test_that("Large occdf inputs are chunked before being sent to the API", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  set.seed(0)
  big_occdf <- data.frame(
    lng = runif(1500, -180, 180),
    lat = runif(1500, -90, 90),
    age = rep(100, 1500)
  )
  expect_warning(
    vcr::use_cassette("palaeorotate-paleomap-chunksize", {
      paleomap <- palaeorotate_point(occdf = big_occdf, model = "PALEOMAP")
    }),
    regexp = "Palaeocoordinates"
  )
  expect_equal(nrow(paleomap), 1500)
})

test_that("input checks for longitude", {
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 210, lat = 40, age = 25)),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = NA, lat = 40, age = 25)),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = "a", lat = 40, age = 25)),
    error = TRUE
  )
})

test_that("input checks for latitude", {
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 160, lat = 200, age = 25)),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 40, lat = NA, age = 25)),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 40, lat = "a", age = 25)),
    error = TRUE
  )
})

test_that("input checks values for age", {
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 160, lat = 40, age = -1)),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 160, lat = 40, age = NA)),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = data.frame(lng = 160, lat = 40, age = "a")),
    error = TRUE
  )
})

test_that("arg 'lng' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    longitude = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  vcr::use_cassette("palaeorotate-custom-lng", {
    out <- palaeorotate_point(occdf = occdf, lng = "longitude")
  })
  expect_equal(nrow(out), 3)
})

test_that("arg 'lat' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    latitude = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  vcr::use_cassette("palaeorotate-custom-lat", {
    out <- palaeorotate_point(occdf = occdf, lat = "latitude")
  })
  expect_equal(nrow(out), 3)
})

test_that("arg 'age' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    custom_age = c(88, 125, 300)
  )

  vcr::use_cassette("palaeorotate-custom-age", {
    out <- palaeorotate_point(occdf = occdf, age = "custom_age")
  })
  expect_equal(nrow(out), 3)
})

test_that("arg 'model' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_offline(host = "zenodo.org")
  skip_on_cran()
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  # --- point method ---
  # Multiple models return one set of palaeocoordinates per model
  vcr::use_cassette("palaeorotate-multi", {
    multi <- palaeorotate_point(occdf = occdf, model = c("PALEOMAP", "GOLONKA"))
  })
  # fmt: skip
  expect_named(
    multi,
    c(
      "lng", "lat", "age", "p_lng_PALEOMAP", "p_lat_PALEOMAP", "p_lng_GOLONKA", 
      "p_lat_GOLONKA", "range_p_lat", "max_dist"
    )
  )

  # Occurrences beyond a model's temporal range return NA
  # fmt: skip
  outside <- data.frame(
    lng = c(
      11.3, 108.846, -7.382, -116.135, -37.032, 113.122, 158.889, -117.594, -52.221, 73.814
    ),
    lat = c(82.663, -12.996, -65.535, 56.473, 55.232, 73.46, 36.648, -7.417, -75.308, 41.534),
    age = rep(700, 10)
  )
  expect_warning(
    vcr::use_cassette("palaeorotate-temporal", {
      paleomap <- palaeorotate_point(occdf = outside, model = "GOLONKA")$p_lng
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(paleomap)))

  # Requesting several models still warns when occurrences exceed the range
  occdf_old <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 700)
  )
  expect_warning(
    vcr::use_cassette("palaeorotate-multi-point", {
      palaeorotate_point(
        occdf = occdf_old,
        model = c("GOLONKA", "PALEOMAP")
      )
    }),
    regexp = "Palaeocoordinates"
  )

  # --- grid method ---
  # Multiple models return one set of palaeocoordinates per model.
  # This live call also caches the reconstruction files used by the
  # cassette-backed test below.
  grid_multi <- palaeorotate_grid(
    occdf = occdf,
    model = c("PALEOMAP", "GOLONKA")
  )
  # fmt: skip
  expect_named(
    grid_multi,
    c(
      "lng", "lat", "age", "rot_age", "rot_lng", "rot_lat", "p_lng_PALEOMAP",
      "p_lat_PALEOMAP", "p_lng_GOLONKA", "p_lat_GOLONKA", "range_p_lat", "max_dist"
    )
  )

  # Occurrences beyond a model's temporal range return NA
  occdf_grid <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = rep(700, 3)
  )
  expect_warning(
    vcr::use_cassette("palaeorotate-grid-temporal", {
      grid_temporal <- palaeorotate_grid(
        occdf = occdf_grid,
        model = "GOLONKA"
      )
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(grid_temporal$p_lng)))

  # input checks
  expect_snapshot(
    palaeorotate_point(occdf = occdf, model = NA),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = occdf, model = character(0)),
    error = TRUE
  )

  # Previously available models have been removed
  expect_snapshot(
    palaeorotate_point(occdf = occdf, model = "MULLER2022"),
    error = TRUE
  )
  # Unknown models are rejected
  expect_snapshot(
    palaeorotate_point(occdf = occdf, model = "GPlates"),
    error = TRUE
  )
})

test_that("palaeorotate() is deprecated but still works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_offline(host = "zenodo.org")
  skip_on_cran()
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  # The "point" method calls palaeorotate_point()
  vcr::use_cassette("palaeorotate-paleomap", {
    lifecycle::expect_deprecated(
      old <- palaeorotate(occdf = occdf, model = "PALEOMAP", method = "point")
    )
  })
  vcr::use_cassette("palaeorotate-paleomap", {
    new <- palaeorotate_point(occdf = occdf, model = "PALEOMAP")
  })
  expect_equal(old, new)

  # The "grid" method calls palaeorotate_grid()
  expect_equal(
    suppressWarnings(
      palaeorotate(occdf = occdf, model = "PALEOMAP", method = "grid")
    ),
    palaeorotate_grid(occdf = occdf, model = "PALEOMAP")
  )

  # input checks
  expect_snapshot(
    suppressWarnings(palaeorotate(occdf = occdf, method = "foo")),
    error = TRUE
  )
  expect_snapshot(
    suppressWarnings(palaeorotate(occdf = occdf, method = NA)),
    error = TRUE
  )
  expect_snapshot(
    suppressWarnings(palaeorotate(occdf = occdf, method = character(0))),
    error = TRUE
  )
  expect_snapshot(
    suppressWarnings(palaeorotate(occdf = occdf, method = c("point", "grid"))),
    error = TRUE
  )
})

test_that("arg 'uncertainty' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_offline(host = "zenodo.org")
  skip_on_cran()
  skip_if_not_installed("vcr")

  # --- point method ---
  # Uncertainty is NA when one model is outside the reconstructable range
  # fmt: skip
  outside <- data.frame(
    lng = c(
      11.3, 108.846, -7.382, -116.135, -37.032, 113.122, 158.889, -117.594, -52.221, 73.814
    ),
    lat = c(82.663, -12.996, -65.535, 56.473, 55.232, 73.46, 36.648, -7.417, -75.308, 41.534),
    age = rep(700, 10)
  )
  expect_warning(
    vcr::use_cassette("palaeorotate-multi-outside-range", {
      paleomap <- palaeorotate_point(
        occdf = outside,
        model = c("PALEOMAP", "GOLONKA"),
        uncertainty = TRUE
      )$max_dist
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(paleomap)))

  # --- grid method ---
  occdf_grid <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = rep(700, 3)
  )
  # Ensure reconstruction files are cached before the cassette-backed call
  # invisible(palaeorotate_grid(
  #   occdf = data.frame(lng = c(2, -103), lat = c(46, 35), age = c(88, 125)),
  #   model = c("PALEOMAP", "GOLONKA")
  # ))
  expect_warning(
    vcr::use_cassette("palaeorotate-grid-temporal-outside-range", {
      grid_temporal <- palaeorotate_grid(
        occdf = occdf_grid,
        model = c("PALEOMAP", "GOLONKA"),
        uncertainty = TRUE
      )
    }),
    regexp = "Palaeocoordinates"
  )
  expect_true(all(is.na(grid_temporal$max_dist)))

  # input checks
  dat <- data.frame(lng = 110, lat = 40, age = 25)
  expect_snapshot(
    palaeorotate_point(occdf = dat, uncertainty = "GOONTHEN"),
    error = TRUE
  )
  expect_snapshot(
    palaeorotate_point(occdf = dat, uncertainty = character(0)),
    error = TRUE
  )
  expect_snapshot(palaeorotate_point(occdf = dat, uncertainty = 1), error = TRUE)
})

test_that("arg 'round' works", {
  skip_if_offline(host = "gws.gplates.org")
  skip_if_not_installed("vcr")

  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )

  # Disabling rounding still returns palaeocoordinates for every occurrence
  vcr::use_cassette("palaeorotate-paleomap", {
    paleomap <- palaeorotate_point(occdf = occdf, model = "PALEOMAP", round = NULL)
  })
  expect_equal(nrow(paleomap), 3)

  # input checks
  expect_snapshot(palaeorotate_point(occdf = occdf, round = TRUE), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf = occdf, round = NA), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf = occdf, round = numeric(0)), error = TRUE)
  expect_snapshot(palaeorotate_point(occdf = occdf, round = 1:2), error = TRUE)
})

test_that("good error message if GPlates or Zenodo are not available", {
  # We define a "mocked" version of nslookup() that errors on purpose, since
  # nslookup() would fail if the website is not available.
  local_mocked_bindings(
    nslookup = function(...) stop("foo")
  )
  occdf <- data.frame(
    lng = c(2, -103, -66),
    lat = c(46, 35, -7),
    age = c(88, 125, 300)
  )
  # GPlates
  expect_snapshot(
    palaeorotate_point(occdf = occdf, model = "PALEOMAP"),
    error = TRUE
  )
  # Zenodo
  expect_snapshot(
    palaeorotate_grid(occdf = occdf, model = "PALEOMAP"),
    error = TRUE
  )
})
