test_that("bin_spatial() works", {
  data("tetrapods")
  lng = "lng"
  lat = "lat"
  dist = 250
  method = "grid"
  buffer = NULL
  return = FALSE

  # Expect equal
  expect_equal(
    nrow(bin_spatial(occdf = tetrapods, method = "grid", dist = 250)),
      nrow(tetrapods))

  # Expect true
  expect_true(
    is.list(
      bin_spatial(
        occdf = tetrapods, method = "grid", dist = 250, return = TRUE)))

  expect_true(
    is.list(
      bin_spatial(
        occdf = tetrapods[1:100,], method = "dist", dist = 250)))

  expect_true(
    is.list(
      bin_spatial(
        occdf = tetrapods[1:100,], method = "dist", dist = 250, buffer = 100)))

  # Error handling
  expect_error(bin_spatial(occdf = matrix(tetrapods)))
  expect_error(bin_spatial(occdf = tetrapods, method = "test"))
  expect_error(bin_spatial(occdf = tetrapods, lng = "long", lat = "latit"))
  expect_error(bin_spatial(occdf = tetrapods, method = 1))
  expect_error(bin_spatial(occdf = tetrapods, method = "grid", dist = "10"))
  expect_error(bin_spatial(occdf = tetrapods, method = "grid", buffer = "10"))
  expect_error(bin_spatial(occdf = tetrapods, return = "TRUE"))
  tetrapods$lat[1] <- 94
  expect_error(bin_spatial(occdf = tetrapods))
  tetrapods$lat[1] <- "94"
  expect_error(bin_spatial(occdf = tetrapods))
  tetrapods$lng[1] <- 184
  expect_error(bin_spatial(occdf = tetrapods))
})
