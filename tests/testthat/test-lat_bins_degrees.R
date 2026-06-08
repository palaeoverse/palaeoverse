test_that("lat_bins_degrees() basic usage works", {
  expect_snapshot(lat_bins_degrees())
})

test_that("argument 'size' works", {
  expect_equal(
    lat_bins_degrees(40),
    data.frame(
      bin = 1:4,
      min = c(30, -10, -50, -90),
      mid = c(50, 10, -30, -70),
      max = c(70, 30, -10, -50)
    )
  )
  expect_snapshot(lat_bins_degrees(size = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(size = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(size = c(10, 20)), error = TRUE)
})

test_that("arguments 'min' and 'max' work", {
  expect_equal(
    lat_bins_degrees(40, min = -30),
    data.frame(
      bin = 1:3,
      min = c(50, 10, -30),
      mid = c(70, 30, -10),
      max = c(90, 50, 10)
    )
  )
  expect_equal(
    lat_bins_degrees(40, max = 10),
    data.frame(
      bin = 1:2,
      min = c(-50, -90),
      mid = c(-30, -70),
      max = c(-10, -50)
    )
  )

  # TODO: this looks suspicious, "min", "mid", and "max" should be column names?
  # expect_equal(
  #   lat_bins_degrees(40, min = 0, max = 10),
  #   data.frame(
  #     bin = c(1, 1, 1),
  #     df = c(0, 20, 40),
  #     row.names = c("min", "mid", "max")
  #   )
  # )

  expect_snapshot(lat_bins_degrees(min = 500), error = TRUE)
  expect_snapshot(lat_bins_degrees(min = "a"), error = TRUE)
  expect_snapshot(lat_bins_degrees(min = NA), error = TRUE)
  expect_snapshot(lat_bins_degrees(min = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(min = c(1, 2)), error = TRUE)

  expect_snapshot(lat_bins_degrees(max = 500), error = TRUE)
  expect_snapshot(lat_bins_degrees(max = "a"), error = TRUE)
  expect_snapshot(lat_bins_degrees(max = NA), error = TRUE)
  expect_snapshot(lat_bins_degrees(max = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(max = c(1, 2)), error = TRUE)

  expect_snapshot(lat_bins_degrees(min = 30, max = 10), error = TRUE)
  expect_snapshot(lat_bins_degrees(min = 30, max = 30), error = TRUE)
})

test_that("argument 'fit' works", {
  expect_message(
    expect_equal(
      lat_bins_degrees(40, fit = TRUE),
      data.frame(
        bin = 1:5,
        min = c(54, 18, -18, -54, -90),
        mid = c(72, 36, 0, -36, -72),
        max = c(90, 54, 18, -18, -54)
      )
    ),
    "Bin size set to 36 degrees to fit latitudinal range."
  )
  expect_snapshot(lat_bins_degrees(fit = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = logical(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = NA), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = c(TRUE, TRUE)), error = TRUE)
})

test_that("argument 'plot' works", {
  expect_doppelganger("lat_bins_degrees", function() {
    lat_bins_degrees(40, plot = TRUE)
  })
  expect_doppelganger("lat_bins_degrees with fit", function() {
    lat_bins_degrees(40, fit = TRUE, plot = TRUE)
  })
  expect_snapshot(lat_bins_degrees(plot = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(plot = logical(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(plot = NA), error = TRUE)
  expect_snapshot(lat_bins_degrees(plot = c(TRUE, TRUE)), error = TRUE)
})
