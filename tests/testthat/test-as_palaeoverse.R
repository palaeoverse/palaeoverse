test_that("as_palaeoverse() errors if some arguments are not provided", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeoverse(dat), error = TRUE)
  expect_snapshot(as_palaeoverse(dat, lat = "lat"), error = TRUE)
  expect_snapshot(as_palaeoverse(lat = "lat", lon = "long"), error = TRUE)
})

test_that("as_palaeoverse() uses provided values", {
  dat <- data.frame(my_lat = 1, my_lon = 2)
  dat2 <- as_palaeoverse(dat, lat = "my_lat", lon = "my_lon")

  expect_equal(attr(dat2, "palaeoverse_lat"), "my_lat")
  expect_equal(attr(dat2, "palaeoverse_lon"), "my_lon")
})

test_that("as_palaeoverse() errors if provided values don't exist in the data", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeoverse(dat, lat = "foo", lon = "long"), error = TRUE)
  expect_snapshot(as_palaeoverse(dat, lat = "lat", lon = "foo"), error = TRUE)
})

test_that("calling as_palaeoverse() several times works", {
  dat <- data.frame(lat = 1, lon = 2)
  dat <- as_palaeoverse(dat, lat = "lat", lon = "lon")

  names(dat) <- c("lat2", "lon2")
  dat <- as_palaeoverse(dat, lat = "lat2", lon = "lon2")

  # jarl-ignore expect_s3_class: we want to test that "palaeoverse_data" is not
  # duplicated, which wouldn't be possible with expect_s3_class().
  expect_equal(class(dat), c("palaeoverse_data", "data.frame"))

  expect_equal(attr(dat, "palaeoverse_lat"), "lat2")
  expect_equal(attr(dat, "palaeoverse_lon"), "lon2")
})

test_that("as_palaeoverse() custom print method works", {
  dat <- data.frame(lat = 1, long = 2)
  dat2 <- as_palaeoverse(dat, lat = "lat", lon = "long")
  expect_snapshot(dat2)

  dat <- data.frame(lat = 1, lon = 2)
  dat2 <- as_palaeoverse(dat, lat = "lat", lon = "lon")
  expect_snapshot(dat2)

  # We don't overwrite other printing methods, we just print our attributes on
  # top of them.
  skip_if_not_installed("tibble")

  dat <- tibble::tibble(lat = 1, lon = 2)
  dat2 <- as_palaeoverse(dat, lat = "lat", lon = "lon")
  expect_snapshot(dat2)
})

test_that("consecutive as_palaeoverse() work correctly", {
  dat <- data.frame(lat = 1, long = 2)
  dat <- as_palaeoverse(dat, lat = "lat", lon = "long")
  expect_equal(attr(dat, "palaeoverse_lat"), "lat")
  expect_equal(attr(dat, "palaeoverse_lon"), "long")

  # Can overwrite an attribute previously set
  names(dat)[names(dat) == "lat"] <- "my_lat"
  dat <- as_palaeoverse(dat, lat = "my_lat", lon = "long")
  expect_equal(attr(dat, "palaeoverse_lat"), "my_lat")
  expect_equal(attr(dat, "palaeoverse_lon"), "long")
})

test_that("args must be named", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeoverse(dat, "foo"), error = TRUE)
  expect_snapshot(as_palaeoverse(dat, "foo", lat = "bar"), error = TRUE)
  expect_snapshot(as_palaeoverse(data = dat, "foo", lat = "bar"), error = TRUE)
})
