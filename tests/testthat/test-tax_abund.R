test_that("tax_abund() works", {

  #error handling

  expect_error(tax_abund(occdf = c(50, 20, 10)))

  occdf <- tetrapods[1:100, ]
  expect_error(tax_abund(occdf = occdf))

  occdf <- tetrapods[!is.na(tetrapods$genus),]
  expect_error(tax_abund(occdf = occdf, plot = true))
  expect_error(tax_abund(occdf, plot_args = c(log = TRUE, col = "pink",
                                              brks = 50)))
  expect_error(tax_abund(occdf, plot_args = list(log = TRUE, col = pink)))
  expect_error(tax_abund(occdf = occdf, name = "Genus", plot = TRUE))
  expect_error(tax_abund(occdf = occdf, abund_vals = c(1, 20, 1002)))
  expect_error(tax_abund(occdf = occdf, abund_vals = "abundance"))

  #tests

  expect_true(is.data.frame(tax_abund(occdf)))

  occdf <- subset(tetrapods, !is.na(family))
  expect_equal(nrow(tax_abund(occdf = occdf, name = "family")),
               length(unique(occdf$family)))

})
