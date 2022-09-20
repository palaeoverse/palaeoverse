test_that("tax_time_expand() works", {
  taxdf <- data.frame(name = c("A", "B", "C"),
                      max_ma = c(150, 60, 30),
                      min_ma = c(110, 20, 0))

  result1 <- tax_time_expand(taxdf)
  result2 <- tax_time_expand(taxdf, ext_orig = FALSE)

  # format
  expect_true(is.data.frame(result1))
  expect_equal(nrow(result1), 34)
  expect_equal(colnames(result1), c(colnames(taxdf), "ext", "orig", colnames(GTS2020)))
  expect_equal(colnames(result2), c(colnames(taxdf), colnames(GTS2020)))

  #error handling
  expect_error(tax_time_expand(result1, scale = "GTS2067"))
  expect_error(tax_time_expand(result1, rank = "stages"))
  expect_error(tax_time_expand(result1, rank = c("stage", "period")))
  expect_error(tax_time_expand(result1, ext_orig = "ext"))
  expect_error(tax_time_expand(c("A", "B")))

  taxdf2 <- data.frame(name = c("A", "B", "C"),
                       max_ma = c("A", "B", "C"),
                       min_ma = c(110, 20, 0))
  expect_error(tax_time_expand(taxdf2))

  taxdf3 <- data.frame(name = c("A", "B", "C"),
                       max_ma = c(150, 60, 30),
                       min_ma = c("A", "B", "C"))
  expect_error(tax_time_expand(taxdf3))

  taxdf4 <- data.frame(name = c("A", "A", "C"),
                       max_ma = c(150, 150, 30),
                       min_ma = c(110, 110, 0))
  expect_error(tax_time_expand(taxdf4))
})
