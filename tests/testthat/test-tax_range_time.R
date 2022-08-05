test_that("tax_range_time() works", {

  occdf <- data.frame(name = c(c("Bulbasaur"), c("Charmander"), c("Squirtle")),
                      max_ma = c("Hettangian", "Albian", "Maastrichtian"),
                      min_ma = c("Tortonian", "Cenomanian", "Bartonia"))

  expect_error(tax_range_time(occdf = occdf, plot = TRUE))

  expect_true(is.vector(
    tax_range_time(occdf = occdf, plot = TRUE, return_error = TRUE)))

  occdf <- data.frame(name = c(c("Bulbasaur"), c("Charmander"), c("Squirtle")),
                      max_ma = c("Hettangian", "Albian", "Maastrichtian"),
                      min_ma = c("Tortonian", "Cenomanian", "Bartonian"))

  unique_taxa <- length(unique(occdf$name))

  # Expect true
  expect_true(
    is.data.frame(
      tax_range_time(occdf = occdf,
                     plot = FALSE)))

  # Expect equal
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = TRUE)),
    unique_taxa)
  expect_equal(
    nrow(tax_range_time(occdf = occdf, plot = FALSE)),
    unique_taxa)

  # Expect error
  expect_error(tax_range_time(occdf = NA))
  expect_error(tax_range_time(occdf = occdf, plot = "test"))
  expect_error(tax_range_time(occdf = occdf, name = "test"))
  occdf$name[1] <- NA
  expect_error(tax_range_time(occdf = occdf))
  occdf$name[1] <- "Bulbasaur"
  occdf$max_ma[1] <- NA
  expect_error(tax_range_time(occdf = occdf))
  occdf$max_ma <- 5
  expect_error(tax_range_time(occdf = occdf))

})
