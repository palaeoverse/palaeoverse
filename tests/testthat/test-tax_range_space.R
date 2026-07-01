test_that("tax_range_space() works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  out <- tax_range_space(occdf)

  expect_equal(
    out,
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_lat = c(10, 50, -10),
      min_lat = c(0, 45, -10),
      range_lat = c(10, 5, 0)
    )
  )

  # There is one row per unique taxon
  # jarl-ignore expect_length: would be less readable to use expect_length in this case
  expect_equal(
    nrow(out),
    length(unique(occdf$genus))
  )

  # input checks
  expect_snapshot(tax_range_space(occdf = data.frame()), error = TRUE)
  expect_snapshot(tax_range_space(occdf = NA), error = TRUE)
  expect_snapshot(tax_range_space(occdf = "a"), error = TRUE)
})

test_that("argument 'name' works", {
  occdf <- data.frame(
    species = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(occdf, name = "species"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_lat = c(10, 50, -10),
      min_lat = c(0, 45, -10),
      range_lat = c(10, 5, 0)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_space(occdf = occdf, name = "nonexistent"),
    error = TRUE
  )
  nadf <- occdf
  nadf$genus[1] <- NA
  expect_snapshot(
    tax_range_space(occdf = nadf, name = "genus"),
    error = TRUE
  )
})

test_that("argument 'lng' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    p_lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(occdf, lng = "p_lng"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_lat = c(10, 50, -10),
      min_lat = c(0, 45, -10),
      range_lat = c(10, 5, 0)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_space(occdf, lng = "nonexistent"),
    error = TRUE
  )
  # the "lng" column must be numeric
  chardf <- data.frame(genus = "a", lng = "10", lat = 10)
  expect_snapshot(
    tax_range_space(chardf),
    error = TRUE
  )
  # the "lng" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), lng = c(10, NA), lat = 10)
  expect_snapshot(
    tax_range_space(nadf),
    error = TRUE
  )
})

test_that("argument 'lat' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    p_lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(occdf, lat = "p_lat"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_lat = c(10, 50, -10),
      min_lat = c(0, 45, -10),
      range_lat = c(10, 5, 0)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_space(occdf, lat = "nonexistent"),
    error = TRUE
  )
  # the "lat" column must be numeric
  chardf <- data.frame(genus = "a", lat = "10", lng = 10)
  expect_snapshot(
    tax_range_space(chardf),
    error = TRUE
  )
  # the "lat" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), lat = c(10, NA), lng = 10)
  expect_snapshot(
    tax_range_space(nadf),
    error = TRUE
  )
})

test_that("argument 'method' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(occdf, method = "lat"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_lat = c(10, 50, -10),
      min_lat = c(0, 45, -10),
      range_lat = c(10, 5, 0)
    )
  )

  expect_equal(
    tax_range_space(occdf, method = "con"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      area = c(1227877.192, 0, 0),
      row.names = c(1L, 5L, 7L)
    )
  )

  expect_equal(
    tax_range_space(occdf, method = "gcd"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      gcd = c(1570.278, 934.333, 0),
      row.names = c(1L, 3L, 5L)
    )
  )

  expect_equal(
    tax_range_space(occdf, method = "occ"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = c(1, 2, 3),
      n_cells = c(4L, 2L, 1L),
      proportional_occ = c(0.571, 0.286, 0.143),
      spacing = 103.595444
    )
  )

  # input checks
  expect_snapshot(
    tax_range_space(occdf, method = c("gcd", "occ")),
    error = TRUE
  )
  expect_snapshot(tax_range_space(occdf, method = "test"), error = TRUE)
  expect_snapshot(tax_range_space(occdf, method = character(0)), error = TRUE)
  expect_snapshot(tax_range_space(occdf, method = NA), error = TRUE)
  expect_snapshot(tax_range_space(occdf, method = 1), error = TRUE)
})

test_that("argument 'spacing' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  small <- tax_range_space(occdf, method = "occ", spacing = 100)
  large <- tax_range_space(occdf, method = "occ", spacing = 500)

  expect_true(unique(large$spacing) > unique(small$spacing))
  expect_length(unique(small$spacing), 1L)

  # "spacing" is only relevant for the "occ" method: it is ignored otherwise
  # TODO: should this error ?
  expect_equal(
    tax_range_space(occdf, method = "lat", spacing = 500),
    tax_range_space(occdf, method = "lat", spacing = 100)
  )

  # input checks
  expect_snapshot(
    tax_range_space(occdf, method = "occ", spacing = "a"),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(occdf, method = "occ", spacing = numeric(0)),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(occdf, method = "occ", spacing = NA),
    error = TRUE
  )
  # TODO: should error
  # expect_snapshot(
  #   tax_range_space(occdf, method = "occ", spacing = 1:2),
  #   error = TRUE
  # )
})

test_that("argument 'coords' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    tax_range_space(occdf, method = "con", coords = TRUE),
    data.frame(
      taxon = rep(c("A", "B", "C"), c(4L, 2L, 1L)),
      taxon_id = rep(1:3, c(4L, 2L, 1L)),
      lng = c(10, 0, 0, 10, 30, 40, 100),
      lat = c(0, 0, 10, 10, 45, 50, -10),
      area = rep(c(1227877.192, 0), 4:3)
    )
  )

  # For the "gcd" method, coords = TRUE returns the coordinates of the two most
  # distant points
  expect_equal(
    tax_range_space(occdf, method = "gcd", coords = TRUE),
    data.frame(
      taxon = rep(c("A", "B", "C"), each = 2L),
      taxon_id = rep(1:3, each = 2L),
      lng = c(10, 0, 40, 30, 100, 100),
      lat = c(10, 0, 50, 45, -10, -10),
      gcd = rep(c(1570.278, 934.333, 0), each = 2L)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_space(occdf, method = "gcd", coords = "a"),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(occdf, method = "gcd", coords = logical(0)),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(occdf, method = "gcd", coords = NA),
    error = TRUE
  )
})
