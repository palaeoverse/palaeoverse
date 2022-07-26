test_that("tax_range_geo() works", {
  # Grab internal data and set-up
  occdf <- tetrapods
  occdf$p_lng <- tetrapods$lng
  occdf$p_lat <- tetrapods$lat
  occdf$name <- occdf$accepted_name
  # n unique taxa
  unique_taxa <- length(unique(occdf$name))

  # Expect equal
  expect_equal(
    nrow(tax_range_geo(occdf = occdf, method = "lat", plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range_geo(occdf = occdf, method = "lat", plot = FALSE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range_geo(occdf = occdf, method = "occ", plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range_geo(occdf = occdf, method = "occ", plot = FALSE)),
    unique_taxa)

  # Expect greater than
  expect_gt(nrow(tax_range_geo(occdf = occdf, method = "gcd", plot = TRUE)),
                unique_taxa)
  expect_gt(nrow(tax_range_geo(occdf = occdf, method = "gcd", plot = FALSE)),
            unique_taxa)
  expect_gt(nrow(tax_range_geo(occdf = occdf, method = "con", plot = TRUE)),
            unique_taxa)
  expect_gt(nrow(tax_range_geo(occdf = occdf, method = "con", plot = FALSE)),
            unique_taxa)

  # Expect error
  expect_error(tax_range_geo(occdf = NA))
  expect_error(tax_range_geo(occdf = occdf, plot = "test"))
  expect_error(tax_range_geo(occdf = occdf, method = "test"))
  occdf$p_lat <- NA
  expect_error(tax_range_geo(occdf = occdf))
  occdf <- occdf[,-which(colnames(occdf) == "name")]
  expect_error(tax_range_geo(occdf = occdf))

})
