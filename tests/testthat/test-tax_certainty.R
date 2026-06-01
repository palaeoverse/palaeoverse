test_that("tax_certainty() works", {
  data("tetrapods")
  # Error handling
  expect_error(tax_certainty(vector()))
  expect_error(tax_certainty(NULL))
  expect_error(tax_certainty(
    taxdf = tetrapods$identified_name,
    certainty = c("certain", "uncertain"),
    append = FALSE
  ))
  expect_error(tax_certainty(
    taxdf = tetrapods,
    name = "accepted_name",
    terms = "subspecies",
    append = FALSE
  ))
  expect_error(tax_certainty(taxdf = tetrapods, name = "test", append = FALSE))
  expect_error(tax_certainty(taxdf = tetrapods, name = 1, append = FALSE))
  expect_error(tax_certainty(
    taxdf = tetrapods,
    name = "accepted_name",
    append = 99
  ))
  # Expect true
  ## Vector returned
  expect_true(is.data.frame(tax_certainty(
    taxdf = tetrapods,
    name = "accepted_name",
    append = FALSE
  )))
  ## Dataframe returned
  expect_true(is.data.frame(tax_certainty(
    taxdf = tetrapods,
    name = "accepted_name",
    append = TRUE
  )))
  ## Custom certainty
  expect_true(is.logical(
    tax_certainty(
      taxdf = tetrapods,
      name = "accepted_name",
      certainty = c(TRUE, FALSE),
      append = FALSE
    )$certainty
  ))
  ## Certainty appended correctly
  expect_true(
    "certainty" %in%
      colnames(tax_certainty(
        taxdf = tetrapods,
        name = "accepted_name",
        append = TRUE
      ))
  )
  ## Custom terms
  expect_true(is.data.frame(tax_certainty(
    taxdf = tetrapods,
    name = "genus",
    terms = list(custom = "test"),
    append = FALSE
  )))

  ## Correct number of certain/uncertainty values returned
  expect_equal(
    4222,
    sum(
      tax_certainty(
        taxdf = tetrapods,
        name = "family",
        append = FALSE
      )$certainty
    )
  )
})
