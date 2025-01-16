library(testthat)

data(tetrapods)
tetrapods_sp <- tetrapods[which(tetrapods$identified_rank == "species"),]
tetrapods_sp <- tetrapods_sp[1:15,]

tetrapods_fam <- tetrapods[which(tetrapods$identified_rank == "family"),]
tetrapods_fam <- tetrapods_fam[1:15,]


test_that("Check certainty", {

  expect_error(tax_certainty(vector()))
  expect_error(tax_certainty(NULL))
  expect_error(tax_certainty(taxa_df = tetrapods_sp$identified_name,
               certainty = c("certain", "uncertain"), rank = "species",
               add_df = FALSE))

  expect_equal(tax_certainty(taxa_df = tetrapods_sp, name = "identified_name",
                             certainty = c("1", "0"), rank = "species",
                             add_df = FALSE), c(0, 0, 0, 0, 0, 0, 0, 0,0,
                                                0, 0, 0, 1, 1, 1))
  expect_equal(tax_certainty(taxa_df = tetrapods_sp, name = "identified_name",
                             certainty = c("TRUE", "FALSE"), rank = "species",
                             add_df = FALSE), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(tax_certainty(taxa_df = tetrapods_fam, name = "identified_name",
                                           certainty = c("certain", "uncertain"), rank = "family",
                                           add_df = FALSE), c("certain", "certain", "certain", "certain",
                                         "certain", "certain","certain", "certain","certain",
                                         "certain","certain", "certain", "certain", "certain",
                                         "certain"))

  ## test if it's correctly adding a column to dataframe
  res <- tax_certainty(taxa_df = tetrapods_sp, name = "identified_name",
                             certainty = c("1", "0"), rank = "species",
                             add_df = TRUE)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), nrow(tetrapods_sp))
  expect_equal(ncol(res), ncol(tetrapods_sp) + 1)
  expect_true("certainty" %in% colnames(res))
  expect_equal(res$certainty, c(0, 0, 0, 0, 0, 0, 0, 0,0,
                                   0, 0, 0, 1, 1, 1))



})


