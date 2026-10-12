test_that("as_palaeo() errors if some arguments are not provided", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeo(dat), error = TRUE)
  expect_snapshot(as_palaeo(dat, lat = "lat"), error = TRUE)
  expect_snapshot(as_palaeo(lat = "lat", lon = "long"), error = TRUE)
})

test_that("as_palaeo() uses provided values", {
  dat <- data.frame(my_lat = 1, my_lon = 2)
  dat2 <- as_palaeo(dat, lat = "my_lat", lon = "my_lon")

  expect_equal(attr(dat2, "palaeoverse_lat"), "my_lat")
  expect_equal(attr(dat2, "palaeoverse_lon"), "my_lon")
})

test_that("as_palaeo() errors if provided values don't exist in the data", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeo(dat, lat = "foo", lon = "long"), error = TRUE)
  expect_snapshot(as_palaeo(dat, lat = "lat", lon = "foo"), error = TRUE)
})

test_that("as_palaeo() custom print method works", {
  dat <- data.frame(lat = 1, long = 2)
  dat2 <- as_palaeo(dat, lat = "lat", lon = "long")
  expect_snapshot(dat2)

  dat <- data.frame(lat = 1, lon = 2)
  dat2 <- as_palaeo(dat, lat = "lat", lon = "lon")
  expect_snapshot(dat2)

  # We don't overwrite other printing methods, we just print our attributes on
  # top of them.
  skip_if_not_installed("tibble")

  dat <- tibble::tibble(lat = 1, lon = 2)
  dat2 <- as_palaeo(dat, lat = "lat", lon = "lon")
  expect_snapshot(dat2)
})

test_that("consecutive as_palaeo() work correctly", {
  dat <- data.frame(lat = 1, long = 2)
  dat <- as_palaeo(dat, lat = "lat", lon = "long")
  expect_equal(attr(dat, "palaeoverse_lat"), "lat")
  expect_equal(attr(dat, "palaeoverse_lon"), "long")

  # Can overwrite an attribute previously set
  names(dat)[names(dat) == "lat"] <- "my_lat"
  dat <- as_palaeo(dat, lat = "my_lat", lon = "long")
  expect_equal(attr(dat, "palaeoverse_lat"), "my_lat")
  expect_equal(attr(dat, "palaeoverse_lon"), "long")
})

test_that("args must be named", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeo(dat, "foo"), error = TRUE)
  expect_snapshot(as_palaeo(dat, "foo", lat = "bar"), error = TRUE)
  expect_snapshot(as_palaeo(data = dat, "foo", lat = "bar"), error = TRUE)
})
