test_that("group_apply works", {
  # Load example data
  occdf <- tetrapods
  # Remove NA data
  occdf <- subset(occdf, !is.na(genus))
  # Number of occurrences from each country
  expect_equal(nrow(
    group_apply(occdf = occdf, group = c("cc"), fun = nrow)
  ), 37)
  # Temporal range of data per time bin with default arguments
  expect_equal(nrow(
    group_apply(occdf = occdf, group = c("cc"), fun = tax_range_time)
    ), 1248)
  # Temporal range of data per time bin with extra arguments
  expect_equal(nrow(
    group_apply(occdf = occdf,
              group = c("cc"),
              fun = tax_range_time,
              name = "family")
    ), 498)
    # Run at family level (default: "genus")
  # Use multiple grouping variables
  expect_equal(nrow(group_apply(occdf = occdf,
              group = c("collection_no", "cc"),
              fun = tax_range_time)), 4017)

  expect_equal(nrow(group_apply(occdf = occdf,
                                group = c("collection_no"),
                                fun = tax_check,
                                verbose = FALSE)), NULL)

  # Errors
  expect_error(group_apply(occdf = 1,
                           group = c("cc"),
                           fun = tax_range_time))
  expect_error(group_apply(occdf = occdf,
                           group = c("cc"),
                           fun = "tax_range_time"))
  expect_error(group_apply(occdf = occdf,
                           group = c("test"),
                           fun = tax_range_time))
  expect_error(group_apply(occdf = occdf,
                           fun = tax_range_time))
  expect_error(group_apply(occdf = occdf,
                           group = c("cc"),
                           fun = tax_range))
  expect_error(group_apply(occdf = occdf,
                           group = c("cc"),
                           fun = tax_range_time,
                           not_an_argument = "test"))
  expect_error(group_apply(occdf = occdf,
                           group = c("cc"),
                           fun = tax_range_time,
                           not_an_argument1 = "test",
                           not_an_argument2 = "test"))
})
