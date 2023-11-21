test_that("plot_section() works", {

  occdf <- data.frame(taxon = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                "bivalve", "shrimp", "anemone", "worm"),
                      bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                      certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

  # Expect error
  expect_error(plot_section(occdf = NA))
  expect_error(plot_section(occdf = occdf, name = "test"))
  expect_error(plot_section(occdf = occdf, level = "test"))
  expect_error(plot_section(occdf = occdf, level = "taxon"))
  expect_error(plot_section(occdf = occdf, certainty = "test"))
  expect_error(plot_section(occdf = occdf, by = "test"))

  occdf[1,1] <- NA;
  expect_error(plot_section(occdf = occdf))
  occdf[1,1] <- "shrimp"; occdf[1,2] <- NA
  expect_error(plot_section(occdf = occdf))
  occdf[1,2] <- 1; occdf[1,3] <- NA
  expect_error(plot_section(occdf = occdf, certainty = "certainty"))
})
