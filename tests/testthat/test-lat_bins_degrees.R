test_that("lat_bins_degrees() basic usage works", {
  bins <- lat_bins_degrees()
  expect_equal(
    bins,
    data.frame(
      bin = 1:18,
      min = seq(80, -90, by = -10),
      mid = seq(85, -85, by = -10),
      max = seq(90, -80, by = -10)
    ),
    ignore_attr = TRUE
  )
  expect_s3_class(bins, "palaeoverse_lat_bins_degrees")
})

test_that("argument 'size' works", {
  bins <- lat_bins_degrees(size = 40)
  expect_equal(
    bins,
    data.frame(
      bin = 1:4,
      min = c(30, -10, -50, -90),
      mid = c(50, 10, -30, -70),
      max = c(70, 30, -10, -50)
    ),
    ignore_attr = TRUE
  )
  expect_s3_class(bins, "palaeoverse_lat_bins_degrees")

  expect_snapshot(lat_bins_degrees(size = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(size = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(size = c(10, 20)), error = TRUE)
})

test_that("arguments 'min' and 'max' work", {
  expect_equal(
    lat_bins_degrees(size = 40, min = -30),
    data.frame(
      bin = 1:3,
      min = c(50, 10, -30),
      mid = c(70, 30, -10),
      max = c(90, 50, 10)
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    lat_bins_degrees(size = 40, max = 10),
    data.frame(
      bin = 1:2,
      min = c(-50, -90),
      mid = c(-30, -70),
      max = c(-10, -50)
    ),
    ignore_attr = TRUE
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
    bins <- lat_bins_degrees(size = 40, fit = TRUE),
    "Bin size set to 36 degrees to fit latitudinal range."
  )
  expect_equal(
    bins,
    data.frame(
      bin = 1:5,
      min = c(54, 18, -18, -54, -90),
      mid = c(72, 36, 0, -36, -72),
      max = c(90, 54, 18, -18, -54)
    ),
    ignore_attr = TRUE
  )
  expect_s3_class(bins, "palaeoverse_lat_bins_degrees")

  expect_snapshot(lat_bins_degrees(fit = 100), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = logical(0)), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = NA), error = TRUE)
  expect_snapshot(lat_bins_degrees(fit = c(TRUE, TRUE)), error = TRUE)
})

test_that("lat_bins errors with unnamed args", {
  expect_snapshot(lat_bins_degrees(10, -90), error = TRUE)
  expect_snapshot(lat_bins_degrees(size = 10, -90), error = TRUE)
  expect_snapshot(lat_bins_degrees(10, -90, 90), error = TRUE)
  expect_snapshot(lat_bins_degrees(10, -90, max = 90), error = TRUE)
})

test_that("argument 'plot' works", {
  expect_doppelganger("lat_bins_degrees", function() {
    plot(lat_bins_degrees(size = 40))
  })

  expect_message(
    expect_doppelganger("lat_bins_degrees with fit", function() {
      plot(lat_bins_degrees(size = 40, fit = TRUE))
    }),
    "Bin size set to 36 degrees to fit latitudinal range."
  )
})

test_that("lat_bins_degrees plotting with extra args works", {
  expect_doppelganger("lat_bins_degrees-colour", function() {
    plot(lat_bins_degrees(size = 40), col = c("red", "blue"))
  })
  expect_doppelganger("lat_bins_degrees-axis", function() {
    plot(lat_bins_degrees(size = 40), xlab = "x axis", ylab = "y axis")
  })

  expect_snapshot(
    plot(lat_bins_degrees(size = 40), col = "foo"),
    error = TRUE
  )
  expect_snapshot(plot(lat_bins_degrees(size = 40), col = 1), error = TRUE)
  expect_snapshot(plot(lat_bins_degrees(size = 40), col = NA), error = TRUE)

  # forbidden args
  expect_snapshot(
    plot(lat_bins_degrees(size = 40), type = "foo", xlim = "foo", ylim = "foo"),
    error = TRUE
  )
})

test_that("plot is deprecated but still works", {
  expect_doppelganger("lat_bins_degrees_deprecated", function() {
    expect_warning(
      lat_bins_degrees(size = 12, plot = TRUE),
      "is deprecated as of palaeoverse 2.0.0",
      fixed = TRUE
    )
  })

  # still input checking
  expect_snapshot(lat_bins_degrees(plot = "6"), error = TRUE)
})
