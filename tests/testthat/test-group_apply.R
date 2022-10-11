test_that("group_apply works", {
  # Load example data
  occdf <- tetrapods
  # Remove NA data
  occdf <- subset(occdf, !is.na(genus))
  # Temporal range of data per time bin with default arguments
  expect_equal(nrow(
    group_apply(df = occdf, group = c("cc"), fun = tax_range_time)
    ), 1248)
  # Temporal range of data per time bin with updated arguments
  expect_equal(nrow(
    group_apply(df = occdf,
              group = c("cc"),
              fun = tax_range_time,
              name = "family")
    ), 498)
    # Run at family level (default: "genus")
  # Use multiple grouping variables
  expect_equal(nrow(group_apply(df = occdf,
              group = c("collection_no", "cc"),
              fun = tax_range_time)), 4017)

  # Errors
  expect_error(group_apply(df = 1,
                           group = c("cc"),
                           fun = tax_range_time))
  expect_error(group_apply(df = occdf,
                           group = c("test"),
                           fun = tax_range_time))
  expect_error(group_apply(df = occdf,
                           fun = tax_range_time))
  expect_error(group_apply(df = occdf,
                           group = c("cc"),
                           fun = tax_range))
})
