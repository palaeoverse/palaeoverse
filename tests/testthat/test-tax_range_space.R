test_that("tax_range_space() works", {
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  out <- tax_range_space(data)

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
    length(unique(data$genus))
  )

  # input checks
  expect_snapshot(tax_range_space(data = data.frame()), error = TRUE)
  expect_snapshot(tax_range_space(data = NA), error = TRUE)
  expect_snapshot(tax_range_space(data = "a"), error = TRUE)
})

test_that("piping and not piping the first argument give the same result", {
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    data |> tax_range_space(name = "genus"),
    tax_range_space(data, name = "genus")
  )
})

test_that("tax_range_space errors with unnamed args", {
  data <- data.frame(
    genus = c("A", "A", "B"),
    lng = c(0, 10, 30),
    lat = c(0, 0, 45)
  )
  expect_snapshot(tax_range_space(data, "genus"), error = TRUE)
  expect_snapshot(tax_range_space(data, "genus", "lng"), error = TRUE)
  expect_snapshot(tax_range_space(data, "genus", lng = "lng"), error = TRUE)
})

test_that("argument 'name' works", {
  data <- data.frame(
    species = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(data, name = "species"),
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
    tax_range_space(data = data, name = "nonexistent"),
    error = TRUE
  )
  nadf <- data
  nadf$genus[1] <- NA
  expect_snapshot(
    tax_range_space(data = nadf, name = "genus"),
    error = TRUE
  )
})

test_that("argument 'lng' works", {
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    p_lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(data, lng = "p_lng"),
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
    tax_range_space(data, lng = "nonexistent"),
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
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    p_lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(data, lat = "p_lat"),
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
    tax_range_space(data, lat = "nonexistent"),
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
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  expect_equal(
    tax_range_space(data, method = "lat"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_lat = c(10, 50, -10),
      min_lat = c(0, 45, -10),
      range_lat = c(10, 5, 0)
    )
  )

  expect_equal(
    tax_range_space(data, method = "con"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      area = c(1227877.192, 0, 0),
      row.names = c(1L, 5L, 7L)
    )
  )

  expect_equal(
    tax_range_space(data, method = "gcd"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      gcd = c(1570.278, 934.333, 0),
      row.names = c(1L, 3L, 5L)
    )
  )

  expect_equal(
    tax_range_space(data, method = "occ"),
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
    tax_range_space(data, method = c("gcd", "occ")),
    error = TRUE
  )
  expect_snapshot(tax_range_space(data, method = "test"), error = TRUE)
  expect_snapshot(tax_range_space(data, method = character(0)), error = TRUE)
  expect_snapshot(tax_range_space(data, method = NA), error = TRUE)
  expect_snapshot(tax_range_space(data, method = 1), error = TRUE)
})

test_that("argument 'spacing' works", {
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )

  small <- tax_range_space(data, method = "occ", spacing = 100)
  large <- tax_range_space(data, method = "occ", spacing = 500)

  expect_true(unique(large$spacing) > unique(small$spacing))
  expect_length(unique(small$spacing), 1L)

  # "spacing" is only relevant for the "occ" method: it is ignored otherwise
  # TODO: should this error ?
  expect_equal(
    tax_range_space(data, method = "lat", spacing = 500),
    tax_range_space(data, method = "lat", spacing = 100)
  )

  # input checks
  expect_snapshot(
    tax_range_space(data, method = "occ", spacing = "a"),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(data, method = "occ", spacing = numeric(0)),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(data, method = "occ", spacing = NA),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(data, method = "occ", spacing = 1:2),
    error = TRUE
  )
})

test_that("argument 'coords' works", {
  data <- data.frame(
    genus = c("A", "A", "A", "A", "B", "B", "C"),
    lng = c(0, 10, 10, 0, 30, 40, 100),
    lat = c(0, 0, 10, 10, 45, 50, -10)
  )
  expect_equal(
    tax_range_space(data, method = "con", coords = TRUE),
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
    tax_range_space(data, method = "gcd", coords = TRUE),
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
    tax_range_space(data, method = "gcd", coords = "a"),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(data, method = "gcd", coords = logical(0)),
    error = TRUE
  )
  expect_snapshot(
    tax_range_space(data, method = "gcd", coords = NA),
    error = TRUE
  )
})
