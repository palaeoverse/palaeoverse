test_that("arg spacing works", {
  expect_snapshot(space_bins(spacing = 1000))
  expect_snapshot(space_bins(spacing = 1000.2))
  expect_s3_class(space_bins(spacing = 1000), "palaeoverse_space_bins")

  expect_snapshot(space_bins(spacing = "10"), error = TRUE)
  expect_snapshot(space_bins(spacing = -1), error = TRUE)
  expect_snapshot(space_bins(spacing = 0), error = TRUE)
  expect_snapshot(space_bins(spacing = numeric(0)), error = TRUE)
  expect_snapshot(space_bins(spacing = NULL), error = TRUE)
  expect_snapshot(space_bins(spacing = NA), error = TRUE)
})

test_that("arg spacing works", {
  expect_snapshot(space_bins(resolution = 1))

  expect_snapshot(space_bins(resolution = 16), error = TRUE)
  expect_snapshot(space_bins(resolution = 15.1), error = TRUE)
  expect_snapshot(space_bins(resolution = "10"), error = TRUE)
  expect_snapshot(space_bins(resolution = -1), error = TRUE)
  expect_snapshot(space_bins(resolution = numeric(0)), error = TRUE)
  expect_snapshot(space_bins(resolution = NULL), error = TRUE)
  expect_snapshot(space_bins(resolution = NA), error = TRUE)
})

test_that("space_bins() must take one of spacing or resolution", {
  expect_snapshot(space_bins(), error = TRUE)
  expect_snapshot(space_bins(spacing = 1000, resolution = 1), error = TRUE)
})

test_that("space_bins() forbids unnamed args", {
  expect_snapshot(space_bins(1000), error = TRUE)
})

test_that("partial matching of argument names is forbidden", {
  expect_snapshot(space_bins(sp = 1000), error = TRUE)
})
