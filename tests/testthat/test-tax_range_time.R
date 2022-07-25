test_that("tax_range_time() works", {
  occdf <- data.frame(name = c(c("Bulbasaur"), c("Charmander"), c("Squirtle")),
                      max_ma = c("Hettangian", "Albian", "Maastrichtian"),
                      min_ma = c("Tortonian", "Cenomanian", "Bartonian"))

  tax_range_time(occdf = occdf, plot = TRUE)

  # Set-up
  occdf <- tetrapods
  # Add name column
  occdf$name <- occdf$accepted_name
  # n unique taxa
  unique_taxa <- length(unique(occdf$name))

  # Expect equal
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = FALSE)),
    unique_taxa)

  # Expect true
  expect_true(
    is.data.frame(
      tax_range_time(occdf = occdf, plot = FALSE)))

  # Expect error
  expect_error(tax_range_time(occdf = NA))
  expect_error(tax_range_time(occdf = occdf, plot = "test"))
  occdf$max_ma <- occdf$early_interval
  expect_error(tax_range_time(occdf = occdf))
  occdf$min_ma <- occdf$late_interval
  expect_error(tax_range_time(occdf = occdf))
})
