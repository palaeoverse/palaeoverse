test_that("as_palaeo() sets default values as attributes only if they exist in the data", {
  dat <- data.frame(lat = 1, long = 2)
  dat2 <- as_palaeo(dat)

  expect_equal(attr(dat2, "palaeo_lat"), "lat")
  expect_null(attr(dat2, "palaeo_lon"))
})

test_that("as_palaeo() uses provided values", {
  dat <- data.frame(my_lat = 1, my_lon = 2)
  dat2 <- as_palaeo(dat, lat = "my_lat", lon = "my_lon")

  expect_equal(attr(dat2, "palaeo_lat"), "my_lat")
  expect_equal(attr(dat2, "palaeo_lon"), "my_lon")
})

test_that("as_palaeo() errors if provided values don't exist in the data", {
  dat <- data.frame(lat = 1, long = 2)
  expect_snapshot(as_palaeo(dat, lat = "foo"), error = TRUE)
  expect_snapshot(as_palaeo(dat, lon = "foo"), error = TRUE)
})

test_that("as_palaeo() custom print method works", {
  dat <- data.frame(lat = 1, long = 2)
  dat2 <- as_palaeo(dat)
  expect_snapshot(dat2)
})

test_that("consecutive as_palaeo() work correctly", {
  dat <- data.frame(lat = 1, long = 2)
  dat <- as_palaeo(dat)
  dat <- as_palaeo(dat, lon = "long")

  # palaeo_lat was added by the first as_palaeo()
  # palaeo_lon was added by the second as_palaeo()
  expect_equal(attr(dat, "palaeo_lat"), "lat")
  expect_equal(attr(dat, "palaeo_lon"), "long")

  # Can overwrite an attribute previously set
  names(dat)[names(dat) == "lat"] <- "my_lat"
  dat <- as_palaeo(dat, lat = "my_lat")
  expect_equal(attr(dat, "palaeo_lat"), "my_lat")
  expect_equal(attr(dat, "palaeo_lon"), "long")
})
