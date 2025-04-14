test_that("tax_certainty() works", {
  data("tetrapods")
  # Error handling
  expect_error(tax_certainty(vector()))
  expect_error(tax_certainty(NULL))
  expect_error(tax_certainty(taxdf = tetrapods$identified_name,
               certainty = c("certain", "uncertain"), rank = "species",
               add_df = FALSE))
  expect_error(tax_certainty(taxdf = tetrapods,
                             name = "accepted_name",
                             rank = "subspecies",
                             add_df = FALSE))
  expect_error(tax_certainty(taxdf = tetrapods,
                             name = "subspecies",
                             rank = "species",
                             add_df = FALSE))
  # Expect true
  ## Vector returned
  expect_true(is.vector(tax_certainty(taxdf = tetrapods,
                                      name = "accepted_name",
                                      rank = "species",
                                      add_df = FALSE)))
  ## Dataframe returned
  expect_true(is.data.frame(tax_certainty(taxdf = tetrapods,
                                          name = "accepted_name",
                                          rank = "species",
                                          add_df = TRUE)))
  ## Custom certainty
  expect_true(is.logical(tax_certainty(taxdf = tetrapods,
                                       name = "accepted_name",
                                       rank = "species",
                                       certainty = c(TRUE, FALSE),
                                       add_df = FALSE)))
  ## Certainty appended correctly
  expect_true("certainty" %in% colnames(tax_certainty(taxdf = tetrapods,
                                                      name = "accepted_name",
                                                      rank = "species",
                                                      add_df = TRUE)))
  ## Genus level working?
  expect_true(is.vector(tax_certainty(taxdf = tetrapods,
                                      name = "genus",
                                      rank = "genus",
                                      add_df = FALSE)))
})


