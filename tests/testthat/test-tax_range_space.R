test_that("tax_range_space_lat() works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  out <- tax_range_space_lat(occdf)

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
  expect_snapshot(tax_range_space_lat(occdf = data.frame()), error = TRUE)
  expect_snapshot(tax_range_space_lat(occdf = NA), error = TRUE)
  expect_snapshot(tax_range_space_lat(occdf = "a"), error = TRUE)
})

test_that("tax_range_space_con() works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    tax_range_space_con(occdf),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      area = c(1227877.192, 0, 0),
      row.names = c(1L, 5L, 7L)
    )
  )
})

test_that("tax_range_space_gcd() works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    tax_range_space_gcd(occdf),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      gcd = c(1570.278, 934.333, 0),
      row.names = c(1L, 3L, 5L)
    )
  )
})

test_that("tax_range_space_occ() works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    tax_range_space_occ(occdf),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = c(1, 2, 3),
      n_cells = c(4L, 2L, 1L),
      proportional_occ = c(0.571, 0.286, 0.143),
      spacing = 103.595444
    )
  )
})

test_that("piping and not piping the first argument give the same result", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    occdf |> tax_range_space_lat(name = "genus"),
    tax_range_space_lat(occdf, name = "genus")
  )
})

test_that("tax_range_space_*() functions error with unnamed args", {
  occdf <- data.frame(
    genus = c("A", "A", "B"),
    lng = c(0, 10, 30),
    lat = c(0, 0, 45)
  )
  expect_snapshot(tax_range_space_lat(occdf, "genus"), error = TRUE)
  expect_snapshot(tax_range_space_lat(occdf, "genus", "lng"), error = TRUE)
  expect_snapshot(
    tax_range_space_lat(occdf, "genus", lng = "lng"),
    error = TRUE
  )
})

test_that("argument 'name' works", {
  occdf <- data.frame(
    species = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space_lat(occdf, name = "species"),
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
    tax_range_space_lat(occdf = occdf, name = "nonexistent"),
    error = TRUE
  )
  nadf <- occdf
  nadf$genus[1] <- NA
  expect_snapshot(
    tax_range_space_lat(occdf = nadf, name = "genus"),
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
    tax_range_space_lat(occdf, lng = "p_lng"),
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
    tax_range_space_lat(occdf, lng = "nonexistent"),
    error = TRUE
  )
  # the "lng" column must be numeric
  chardf <- data.frame(genus = "a", lng = "10", lat = 10)
  expect_snapshot(
    tax_range_space_lat(chardf),
    error = TRUE
  )
  # the "lng" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), lng = c(10, NA), lat = 10)
  expect_snapshot(
    tax_range_space_lat(nadf),
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
    tax_range_space_lat(occdf, lat = "p_lat"),
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
    tax_range_space_lat(occdf, lat = "nonexistent"),
    error = TRUE
  )
  # the "lat" column must be numeric
  chardf <- data.frame(genus = "a", lat = "10", lng = 10)
  expect_snapshot(
    tax_range_space_lat(chardf),
    error = TRUE
  )
  # the "lat" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), lat = c(10, NA), lng = 10)
  expect_snapshot(
    tax_range_space_lat(nadf),
    error = TRUE
  )
})

test_that("argument 'spacing' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  small <- tax_range_space_occ(occdf, spacing = 100)
  large <- tax_range_space_occ(occdf, spacing = 500)

  expect_true(unique(large$spacing) > unique(small$spacing))
  expect_length(unique(small$spacing), 1L)

  # input checks
  expect_snapshot(tax_range_space_occ(occdf, spacing = "a"), error = TRUE)
  expect_snapshot(
    tax_range_space_occ(occdf, spacing = numeric(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_space_occ(occdf, spacing = NA), error = TRUE)
  expect_snapshot(tax_range_space_occ(occdf, spacing = 1:2), error = TRUE)
})

test_that("argument 'coords' works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    tax_range_space_con(occdf, coords = TRUE),
    data.frame(
      taxon = rep(c("A", "B", "C"), c(4L, 2L, 1L)),
      taxon_id = rep(1:3, c(4L, 2L, 1L)),
      lng = c(10, 0, 0, 10, 30, 40, 100),
      lat = c(0, 0, 10, 10, 45, 50, -10),
      area = rep(c(1227877.192, 0), 4:3)
    )
  )

  # For tax_range_space_gcd(), coords = TRUE returns the coordinates of the two
  # most distant points
  expect_equal(
    tax_range_space_gcd(occdf, coords = TRUE),
    data.frame(
      taxon = rep(c("A", "B", "C"), each = 2L),
      taxon_id = rep(1:3, each = 2L),
      lng = c(10, 0, 40, 30, 100, 100),
      lat = c(10, 0, 50, 45, -10, -10),
      gcd = rep(c(1570.278, 934.333, 0), each = 2L)
    )
  )

  # input checks
  expect_snapshot(tax_range_space_gcd(occdf, coords = "a"), error = TRUE)
  expect_snapshot(
    tax_range_space_gcd(occdf, coords = logical(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_space_gcd(occdf, coords = NA), error = TRUE)
})

test_that("tax_range_space() is deprecated but still works", {
  occdf <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  lifecycle::expect_deprecated(old <- tax_range_space(occdf))
  expect_equal(old, tax_range_space_lat(occdf))

  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    tax_range_space(occdf, method = "con", coords = TRUE),
    tax_range_space_con(occdf, coords = TRUE)
  )
  expect_equal(
    tax_range_space(occdf, method = "gcd", coords = TRUE),
    tax_range_space_gcd(occdf, coords = TRUE)
  )
  expect_equal(
    tax_range_space(occdf, method = "occ", spacing = 500),
    tax_range_space_occ(occdf, spacing = 500)
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
