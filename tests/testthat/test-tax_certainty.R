test_that("tax_certainty() works", {
  data("tetrapods")
  # Error handling
  expect_error(tax_certainty(vector()))
  expect_error(tax_certainty(NULL))
  expect_error(tax_certainty(taxdf = tetrapods$identified_name,
               certainty = c("certain", "uncertain"), rank = "species",
               append = FALSE))
  expect_error(tax_certainty(taxdf = tetrapods,
                             name = "accepted_name",
                             rank = "subspecies",
                             append = FALSE))
  expect_error(tax_certainty(taxdf = tetrapods,
                             name = "subspecies",
                             rank = "species",
                             append = FALSE))
  # Expect true
  ## Vector returned
  expect_true(is.vector(tax_certainty(taxdf = tetrapods,
                                      name = "accepted_name",
                                      rank = "species",
                                      append = FALSE)))
  ## Dataframe returned
  expect_true(is.data.frame(tax_certainty(taxdf = tetrapods,
                                          name = "accepted_name",
                                          rank = "species",
                                          append = TRUE)))
  ## Custom certainty
  expect_true(is.logical(tax_certainty(taxdf = tetrapods,
                                       name = "accepted_name",
                                       rank = "species",
                                       certainty = c(TRUE, FALSE),
                                       append = FALSE)))
  ## Certainty appended correctly
  expect_true("certainty" %in% colnames(tax_certainty(taxdf = tetrapods,
                                                      name = "accepted_name",
                                                      rank = "species",
                                                      append = TRUE)))
  ## Genus level working?
  expect_true(is.vector(tax_certainty(taxdf = tetrapods,
                                      name = "genus",
                                      rank = "genus",
                                      append = FALSE)))
})


