test_that("tax_range() works", {
  # Set-up
  occdf <- tetrapods
  # Add name column
  occdf$name <- occdf$accepted_name
  # Add latitudinal col
  occdf$p_lat <- occdf$lat
  # Add longitudinal col
  occdf$p_lng <- occdf$lng
  # n unique taxa
  unique_taxa <- length(unique(occdf$name))

  # Expect equal
  expect_equal(
    nrow(tax_range(occdf = occdf, method = "temporal", plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range(occdf = occdf, method = "temporal", plot = FALSE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range(occdf = occdf, method = "lat", plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range(occdf = occdf, method = "lat", plot = FALSE)),
    unique_taxa)
  expect_equal(
    max(tax_range(occdf = occdf, method = "geo", plot = FALSE)$taxa_id),
    unique_taxa)
  expect_equal(
    max(tax_range(occdf = occdf, method = "geo", plot = TRUE)$taxa_id),
    unique_taxa)

  # Expect true
  expect_true(
    is.data.frame(
      tax_range(occdf = occdf, method = "temporal", plot = FALSE)))
  expect_true(
    is.data.frame(
      tax_range(occdf = occdf, method = "lat", plot = FALSE)))
  expect_true(
    is.data.frame(
      tax_range(occdf = occdf, method = "geo", plot = FALSE)))

  # Expect error
  expect_error(tax_range(occdf = NA))
  expect_error(tax_range(occdf = occdf, method = "test"))
  expect_error(tax_range(occdf = occdf, plot = "test"))

  occdf <- occdf[, -which(colnames(occdf) == "max_ma")]
  expect_error(tax_range(occdf = occdf, method = "temporal"))

  occdf$p_lat[20] <- NA
  expect_error(tax_range(occdf = occdf, method = "lat"))
  expect_error(tax_range(occdf = occdf, method = "geo"))

  occdf$p_lat <- TRUE
  expect_error(tax_range(occdf = occdf, method = "lat"))
  expect_error(tax_range(occdf = occdf, method = "geo"))

  occdf <- occdf[, -which(colnames(occdf) == "p_lat")]
  expect_error(tax_range(occdf = occdf, method = "lat"))
  expect_error(tax_range(occdf = occdf, method = "geo"))




})
