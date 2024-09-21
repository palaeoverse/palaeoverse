species <- c("Prohyrax indet.", "Pliohyrax rossignoli", "Pliohyrax_graecus",
             "Pliohyrax kruppii", "Pliohyrax graecus", "Pliohyrax", "Pliohyrax_indet.",
             "Bunohyrax indet.", "Pachyhyrax_sp.", "Heterohyrax_aff.", "Afrohyrax cf.",
             "Bunohyrax aff.", "Afrohyrax_cf.")

species_na <-  c("Prohyrax indet.", "Pliohyrax rossignoli", "Pliohyrax_graecus",
                 "Pliohyrax kruppii", NA, "Pliohyrax", "Pliohyrax_indet.",
                 "Bunohyrax indet.", "Pachyhyrax_sp.", "Heterohyrax_aff.", "Afrohyrax cf.",
                 "Bunohyrax aff.", NA)

test_that("Check species name uncertainty", {

  expect_error(tax_certainty(vector()))
  expect_error(tax_certainty(NULL))
  expect_error(tax_certainty(species_na))

  expect_equal(tax_certainty(species), c(0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0))
})
