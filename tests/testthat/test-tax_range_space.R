test_that("tax_range_soace() works", {
  # Grab internal data and set-up
  occdf <- tetrapods
  # n unique taxa
  unique_taxa <- length(unique(occdf$accepted_name))

  # Expect equal
  expect_equal(
    nrow(tax_range_space(occdf = occdf, name = "accepted_name",
                         method = "lat")),
    unique_taxa)
  expect_equal(
    nrow(tax_range_space(occdf = occdf, name = "accepted_name",
                         method = "lat")),
    unique_taxa)
  expect_equal(
    nrow(tax_range_space(occdf = occdf, name = "accepted_name",
                         method = "occ")),
    unique_taxa)

  # Expect greater than
  expect_gt(nrow(tax_range_space(occdf = occdf,
                               name = "accepted_name",
                               method = "gcd")),
                unique_taxa)
  expect_gt(nrow(tax_range_space(occdf = occdf,
                               name = "accepted_name",
                               method = "gcd")),
            unique_taxa)
  expect_gt(nrow(tax_range_space(occdf = occdf,
                               name = "accepted_name",
                               method = "con")),
            unique_taxa)
  expect_gt(nrow(tax_range_space(occdf = occdf, name = "accepted_name",
                               method = "con")),
            unique_taxa)

  # Expect error
  expect_error(tax_range_space(occdf = NA))
  expect_error(tax_range_space(occdf = occdf))
  expect_error(tax_range_space(occdf = occdf, name = "accepted_name",
                             lat = "test", method = "con"))
  expect_error(tax_range_space(occdf = occdf,  name = "accepted_name",
                             method = "test"))
  expect_error(tax_range_space(occdf = occdf, name = "accepted_name",
                             method = 1))
  occdf$accepted_name[1] <- NA
  expect_error(tax_range_space(occdf = occdf, name = "accepted_name",
                             method = "con"))
  expect_error(tax_range_space(occdf = occdf))
  occdf$lat[1] <- "22"
  expect_error(tax_range_space(occdf = occdf, name = "accepted_name"))
  occdf$lat[1] <- NA
  expect_error(tax_range_space(occdf = occdf, name = "accepted_name"))
  occdf <- occdf[, -which(colnames(occdf) == "name")]
  expect_error(tax_range_space(occdf = occdf))

})
