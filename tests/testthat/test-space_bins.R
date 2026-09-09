test_that("space_bins works", {
  expect_snapshot(space_bins(spacing = 1000))
  expect_snapshot(space_bins(spacing = 1000.2))
})

test_that("spacing has no default value", {
  expect_snapshot(space_bins(), error = TRUE)
})

test_that("space_bins() allows unnamed arg", {
  expect_equal(space_bins(spacing = 1000), space_bins(1000))
})

test_that("partial matching of argument names is forbidden", {
  expect_snapshot(space_bins(sp = 1000), error = TRUE)
})

test_that("space_bins errors with wrong inputs", {
  expect_snapshot(space_bins(spacing = "10"), error = TRUE)
  expect_snapshot(space_bins(spacing = -1), error = TRUE)
  expect_snapshot(space_bins(spacing = numeric(0)), error = TRUE)
  expect_snapshot(space_bins(spacing = NULL), error = TRUE)
  expect_snapshot(space_bins(spacing = NA), error = TRUE)
})
