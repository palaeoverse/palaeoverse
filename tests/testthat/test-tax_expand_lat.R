test_that("tax_expand_lat works", {
  bins <- lat_bins_degrees()
  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c(60, 20, -10),
                      min_lat = c(20, -40, -60))

  result <- tax_expand_lat(taxdf = taxdf, bins = bins)

  # format
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 15)
  expect_equal(colnames(result), c(colnames(taxdf), colnames(bins)))

  # error handling
  expect_error(tax_expand_lat(taxdf = 5))
  expect_error(tax_expand_lat(taxdf))
  expect_error(tax_expand_lat(taxdf, bins = 1))
  expect_error(tax_expand_lat(taxdf, bins = bins, max_lat = "lat"))
  expect_error(tax_expand_lat(taxdf, bins = bins, min_lat = "lat"))

  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c(92, 20, -10),
                      min_lat = c(20, -40, -60))
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))

  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c(60, 20, -10),
                      min_lat = c(-92, -40, -60))
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))

  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c("60", "20", "-10"),
                      min_lat = c(-92, -40, -60))
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))

  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c(60, 20, -10),
                      min_lat = c("20", -40, -60))
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))

  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c(60, 20, -10),
                      min_lat = c(72, -40, -60))
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))

  taxdf <- data.frame(name = c("A", "A", "C"),
                      max_lat = c(60, 60, -10),
                      min_lat = c(20, 20, -60))
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))

  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_lat = c(60, 20, -10),
                      min_lat = c(20, -40, -60))
  bins <- bins[, -1]
  expect_error(tax_expand_lat(taxdf = taxdf, bins = bins))
})
