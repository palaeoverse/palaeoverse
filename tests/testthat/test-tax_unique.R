test_that("tax_unique() works", {

  #expect equal
  #expect_equal()

  #expect true
  #expect_true()

  #expect error
  expect_error(tax_unique(paleobioDB = 100))
  expect_error(tax_unique(species = "yes", genus = "no"))

})
