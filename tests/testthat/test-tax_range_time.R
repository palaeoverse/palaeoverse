test_that("tax_range_time() works", {

  occdf <- tetrapods

  occdf <- subset(occdf, !is.na(genus))

  unique_taxa <- length(unique(occdf$genus))

  # Expect true
  expect_true(is.data.frame(tax_range_time(occdf = occdf)))
  expect_true(is.data.frame(tax_range_time(occdf = occdf, by = "LAD")))
  expect_true(is.data.frame(tax_range_time(occdf = occdf, by = "name")))
  expect_true(is.data.frame(tax_range_time(occdf = occdf, group = "class")))

  # Expect equal
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = FALSE)),
    unique_taxa)

  # Expect error
  expect_error(tax_range_time(occdf = NA))
  expect_error(tax_range_time(occdf = occdf, max_ma = "test"))
  expect_error(tax_range_time(occdf = occdf, min_ma = "test"))
  expect_error(tax_range_time(occdf = occdf, by = "test"))
  expect_error(tax_range_time(occdf = occdf, group = "test"))
  expect_error(tax_range_time(occdf = occdf, plot = "test"))
  expect_error(tax_range_time(occdf = occdf, name = "test"))
  expect_error(tax_range_time(occdf = occdf, plot_args = "test"))
  occdf$genus[1] <- NA
  expect_error(tax_range_time(occdf = occdf))
  occdf$max_ma[1] <- "test"
  expect_error(tax_range_time(occdf = occdf))

})
