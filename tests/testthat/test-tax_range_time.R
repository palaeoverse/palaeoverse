test_that("tax_range_time() works", {
  occdf <- tetrapods

  occdf <- subset(occdf, !is.na(genus))

  unique_taxa <- length(unique(occdf$genus))

  # Expect true
  expect_true(is.data.frame(tax_range_time(occdf = occdf)))
  expect_true(is.data.frame(tax_range_time(occdf = occdf, by = "LAD")))
  expect_true(is.data.frame(tax_range_time(occdf = occdf, by = "name")))
  expect_true(is.data.frame(tax_range_time(
    occdf = occdf,
    group = "class",
    plot = TRUE
  )))

  # Expect equal
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = TRUE)),
    unique_taxa
  )
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = FALSE)),
    unique_taxa
  )

  # Expect error
  expect_snapshot(tax_range_time(occdf = NA), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, max_ma = "test"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, min_ma = "test"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, by = "test"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, group = "test"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, plot = "test"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, name = "test"), error = TRUE)
  expect_snapshot(
    tax_range_time(occdf = occdf, plot_args = "test"),
    error = TRUE
  )
  occdf$genus[1] <- NA
  expect_snapshot(tax_range_time(occdf = occdf), error = TRUE)
  occdf$max_ma[1] <- "test"
  expect_snapshot(tax_range_time(occdf = occdf), error = TRUE)
})
test_that("tax_range_time() resets row names", {
  # Remove NAs
  tetrapods <- subset(tetrapods, !is.na(order) & order != "NO_ORDER_SPECIFIED")
  # Temporal range
  result <- tax_range_time(occdf = tetrapods, name = "order")
  expect_equal(row.names(result), as.character(seq_len(nrow(result))))
})
