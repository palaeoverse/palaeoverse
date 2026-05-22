test_that("tax_expand_time() works", {
  taxdf <- data.frame(
    name = c("A", "B", "C"),
    max_ma = c(150, 60, 30),
    min_ma = c(110, 20, 0)
  )

  result1 <- tax_expand_time(taxdf)
  result2 <- tax_expand_time(taxdf, ext_orig = FALSE)
  result3 <- tax_expand_time(taxdf, scale = "GTS2012")
  result4 <- tax_expand_time(taxdf, bins = time_bins(scale = "GTS2020"))

  # format
  expect_true(is.data.frame(result1))
  expect_equal(nrow(result1), 34)
  expect_equal(
    colnames(result1),
    c(
      colnames(taxdf),
      "ext",
      "orig",
      colnames(time_bins(scale = "GTS2020", rank = "stage"))
    )
  )
  expect_equal(
    colnames(result2),
    c(colnames(taxdf), colnames(time_bins(scale = "GTS2020", rank = "stage")))
  )
  expect_equal(nrow(result3), 32)
  expect_equal(result1, result4)

  # error handling
  expect_snapshot(tax_expand_time(taxdf, scale = "GTS2067"), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, rank = "stages"), error = TRUE)
  expect_snapshot(
    tax_expand_time(taxdf, rank = c("stage", "period")),
    error = TRUE
  )
  expect_snapshot(tax_expand_time(taxdf, bins = "stages"), error = TRUE)
  bins <- time_bins()
  colnames(bins)[colnames(bins) == "max_ma"] <- "max_age"
  expect_snapshot(tax_expand_time(taxdf, bins = bins), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, scale = NULL), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, scale = NULL), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, ext_orig = "ext"), error = TRUE)
  expect_snapshot(tax_expand_time(c("A", "B")), error = TRUE)

  taxdf2 <- data.frame(
    name = c("A", "B", "C"),
    max_ma = c("A", "B", "C"),
    min_ma = c(110, 20, 0)
  )
  expect_snapshot(tax_expand_time(taxdf2), error = TRUE)

  taxdf3 <- data.frame(
    name = c("A", "B", "C"),
    max_ma = c(150, 60, 30),
    min_ma = c("A", "B", "C")
  )
  expect_snapshot(tax_expand_time(taxdf3), error = TRUE)

  taxdf4 <- data.frame(
    name = c("A", "A", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, 0)
  )
  expect_snapshot(tax_expand_time(taxdf4), error = TRUE)

  taxdf5 <- data.frame(
    name = c("A", "A", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, -20)
  )
  expect_snapshot(tax_expand_time(taxdf5), error = TRUE)

  taxdf6 <- data.frame(
    name = c("A", "A", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, 40)
  )
  expect_snapshot(tax_expand_time(taxdf6), error = TRUE)

  taxdf7 <- data.frame(
    name = c("A", "B", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, 40)
  )
  expect_snapshot(tax_expand_time(taxdf7, max_ma = "test"), error = TRUE)
})
