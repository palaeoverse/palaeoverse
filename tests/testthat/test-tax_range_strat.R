test_that("tax_range_strat() error handling", {

  occdf <- data.frame(genus = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                "bivalve", "shrimp", "anemone", "worm"),
                      group = c("A", "B", "B", "A", "A", "A", "A", "B", "B"),
                      bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                      certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

  # Expect error
  expect_error(tax_range_strat(occdf = NA))
  expect_error(tax_range_strat(occdf = occdf, group = "test"))
  expect_error(tax_range_strat(occdf = occdf, name = "test"))
  expect_error(tax_range_strat(occdf = occdf, level = "test"))
  expect_error(tax_range_strat(occdf = occdf, level = "genus"))
  expect_error(tax_range_strat(occdf = occdf, certainty = 0))
  expect_error(tax_range_strat(occdf = occdf, certainty = "test"))
  expect_error(tax_range_strat(occdf = occdf, by = "test"))

  occdf[1,1] <- NA;
  expect_error(tax_range_strat(occdf = occdf))
  occdf[1,1] <- "shrimp"; occdf[1,3] <- NA
  expect_error(tax_range_strat(occdf = occdf))
  occdf[1,3] <- 1; occdf[1,4] <- NA
  expect_error(tax_range_strat(occdf = occdf, certainty = "certainty"))
})

test_that("tax_range_strat() plots", {
  expect_doppelganger("tax_range_strat() plots", function() {

  occdf <- data.frame(genus = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                "bivalve", "shrimp", "anemone", "worm"),
                      bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                      certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

  tax_range_strat(occdf)
  })
})

test_that("tax_range_strat() does uncertainty", {
  expect_doppelganger("tax_range_strat() does uncertainty", function() {

    occdf <- data.frame(genus = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                  "bivalve", "shrimp", "anemone", "worm"),
                        bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                        certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

    tax_range_strat(occdf, certainty = "certainty")
  })
})

test_that("tax_range_strat() sorts", {
  expect_doppelganger("tax_range_strat() sorts", function() {

    occdf <- data.frame(genus = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                  "bivalve", "shrimp", "anemone", "worm"),
                        bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                        certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

    tax_range_strat(occdf, by = "LAD")
  })
})

test_that("tax_range_strat() labels", {
  expect_doppelganger("tax_range_strat() labels", function() {

    occdf <- data.frame(genus = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                  "bivalve", "shrimp", "anemone", "worm"),
                        bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                        certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

    tax_range_strat(occdf, plot_args = list(ylab = "Height (m)"))
  })
})

test_that("tax_range_strat() stops some plot_args", {
  expect_doppelganger("tax_range_strat() labels", function() {

    occdf <- data.frame(genus = c("shrimp", "worm", "worm", "shrimp", "bivalve",
                                  "bivalve", "shrimp", "anemone", "worm"),
                        bed = c(1, 1, 2, 2, 2, 3, 3, 4, 4),
                        certainty = c(1, 1, 0, 1, 0, 1, 1, 1, 0))

    tax_range_strat(occdf,
                    plot_args = list(type = "line", ylab = "Height (m)"),
                    x_args = list(side = 1), y_args = list(side = 2))
  })
})
