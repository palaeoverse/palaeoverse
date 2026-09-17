test_that("lat_bins_area works", {
  # default is to output 12 rows
  expect_snapshot(lat_bins_area())

  expect_snapshot(lat_bins_area(n_bins = 6))
  expect_snapshot(lat_bins_area(n_bins = 6, min = 0, max = 90))
})

test_that("different radius affects the results", {
  orig <- lat_bins_area(n_bins = 6)
  new <- lat_bins_area(n_bins = 6, r = 6000)
  expect_all_false(orig$area == new$area)
})

test_that("lat_bins_area errors if min == max", {
  expect_snapshot(
    lat_bins_area(min = 90, max = 90),
    error = TRUE
  )
})

test_that("lat_bins_area errors with unnamed args", {
  expect_snapshot(lat_bins_area(10, 1), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = 10, 1), error = TRUE)
  expect_snapshot(lat_bins_area(10, 1, 2), error = TRUE)
  expect_snapshot(lat_bins_area(10, 1, max = 2), error = TRUE)
})

test_that("partial matching of argument names is forbidden", {
  expect_snapshot(lat_bins_area(10, mi = 1), error = TRUE)
  expect_snapshot(lat_bins_area(10, mi = 1, ma = 2), error = TRUE)
})

test_that("lat_bins_area errors with wrong inputs", {
  expect_snapshot(lat_bins_area(n_bins = "10"), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = -1), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_area(n_bins = 3.5), error = TRUE)

  expect_snapshot(lat_bins_area(max = 100), error = TRUE)
  expect_snapshot(lat_bins_area(max = numeric(0)), error = TRUE)

  expect_snapshot(lat_bins_area(min = 100), error = TRUE)
  expect_snapshot(lat_bins_area(min = numeric(0)), error = TRUE)

  expect_snapshot(lat_bins_area(min = 90, max = -90), error = TRUE)

  expect_snapshot(lat_bins_area(r = "Earth"), error = TRUE)
  expect_snapshot(lat_bins_area(r = numeric(0)), error = TRUE)
  expect_snapshot(lat_bins_area(r = -1), error = TRUE)
})

test_that("lat_bins_area plotting works", {
  expect_doppelganger("lat_bins_area", function() {
    plot(lat_bins_area(n_bins = 12))
  })
  expect_doppelganger("lat_bins_area-extra_args", function() {
    plot(lat_bins_area(n_bins = 12), sub = "A subtitle")
  })

  expect_snapshot(plot(lat_bins_area(n_bins = 12), 1), error = TRUE)

  # Plotting options that cannot be changed
  expect_snapshot(plot(lat_bins_area(n_bins = 12), xlim = "foo"), error = TRUE)
  expect_snapshot(
    plot(lat_bins_area(n_bins = 12), xlim = "foo", ylim = "bar"),
    error = TRUE
  )
  expect_snapshot(
    plot(lat_bins_area(n_bins = 12), xlim = "foo", ylim = "bar", type = "l"),
    error = TRUE
  )

  # only one of the extra args is disallowed
  expect_snapshot(
    plot(lat_bins_area(n_bins = 12), xlim = "foo", sub = "bar"),
    error = TRUE
  )
})

test_that("lat_bins_area plotting with extra args works", {
  expect_doppelganger("lat_bins_area-colour", function() {
    plot(lat_bins_area(n_bins = 12), col = c("red", "blue"))
  })
  expect_doppelganger("lat_bins_area-axis", function() {
    plot(lat_bins_area(n_bins = 12), xlab = "x axis", ylab = "y axis")
  })

  expect_snapshot(plot(lat_bins_area(n_bins = 12), col = "foo"), error = TRUE)
  expect_snapshot(plot(lat_bins_area(n_bins = 12), col = 1), error = TRUE)
  expect_snapshot(plot(lat_bins_area(n_bins = 12), col = NA), error = TRUE)
})

test_that("n is deprecated but still works", {
  # deprecated
  expect_snapshot(lat_bins_area(n = 6))

  # still works
  expect_warning(
    expect_equal(lat_bins_area(n = 6), lat_bins_area(n_bins = 6))
  )

  # still input checking
  expect_snapshot(lat_bins_area(n = "6"), error = TRUE)

  # can't specify both n and n_bins
  expect_snapshot(lat_bins_area(n_bins = 6, n = 6), error = TRUE)
})


test_that("plot is deprecated but still works", {
  expect_doppelganger("lat_bins_area_deprecated", function() {
    expect_warning(
      lat_bins_area(n_bins = 12, plot = TRUE),
      "is deprecated as of palaeoverse 2.0.0",
      fixed = TRUE
    )
  })

  # still input checking
  expect_snapshot(lat_bins_area(plot = "6"), error = TRUE)
})
