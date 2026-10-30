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

test_that("arg resolution works", {
  expect_snapshot(space_bins(resolution = 1))

  expect_snapshot(space_bins(resolution = 16), error = TRUE)
  expect_snapshot(space_bins(resolution = 15.1), error = TRUE)
  expect_snapshot(space_bins(resolution = "10"), error = TRUE)
  expect_snapshot(space_bins(resolution = -1), error = TRUE)
  expect_snapshot(space_bins(resolution = numeric(0)), error = TRUE)
  expect_snapshot(space_bins(resolution = NULL), error = TRUE)
  expect_snapshot(space_bins(resolution = NA), error = TRUE)
})

test_that("arg area works", {
  expect_snapshot(space_bins(area = 100000))
  expect_equal(attr(space_bins(area = 100000), "h3_resolution"), 2)

  expect_snapshot(space_bins(area = "10"), error = TRUE)
  expect_snapshot(space_bins(area = -1), error = TRUE)
  expect_snapshot(space_bins(area = 0), error = TRUE)
  expect_snapshot(space_bins(area = NA), error = TRUE)
})

test_that("space_bins() must take one of spacing, resolution, or area", {
  expect_snapshot(space_bins(), error = TRUE)
  expect_snapshot(space_bins(spacing = 1000, resolution = 1), error = TRUE)
  expect_snapshot(space_bins(spacing = 1000, area = 100000), error = TRUE)
})

test_that("space_bins() forbids unnamed args", {
  expect_snapshot(space_bins(1000), error = TRUE)
})

test_that("partial matching of argument names is forbidden", {
  expect_snapshot(space_bins(sp = 1000), error = TRUE)
})

# This is used to detect possible changes in area and spacing coming from h3jsr
# (for instance if those values are recomputed with a slightly different Earth
# radius).
test_that("H3 resolution table used by space_bins() is unchanged", {
  tab <- h3jsr::h3_info_table
  expect_equal(tab$h3_resolution, 0:15)
  expect_equal(tab$total_unique_indexes, 2 + 120 * 7^(0:15))
  # fmt: skip
  expect_equal(
    tab$avg_area_sqkm,
    c(
      4357449.416, 609788.4418, 86801.7804, 12393.43466, 1770.347654,
      252.9038582, 36.12906216, 5.16129336, 0.7373275976, 0.1053325134,
      0.01504750191, 0.002149643129, 0.0003070918756, 4.387026795e-05,
      6.267181135e-06, 8.953115908e-07
    ),
    tolerance = 1e-8
  )
  # fmt: skip
  expect_equal(
    tab$avg_cendist_km,
    c(
      1918.614488, 725.1681134, 274.0877839, 103.5954448, 39.1553977,
      14.79934925, 5.593628243, 2.11419276, 0.799089753, 0.3020275166,
      0.1141556703, 0.0431463573, 0.01630816941, 0.006165915546,
      0.002335800418, 0.0008828488133
    ),
    tolerance = 1e-8
  )
})
