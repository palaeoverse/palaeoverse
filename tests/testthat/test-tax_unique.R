test_that("tax_unique() works", {
  data(tetrapods)
  dinosaurs <- data.frame(c("rex", "aegyptiacus", NA, NA, "rex"),
                          c("Tyrannosaurus", "Spinosaurus", NA, NA,
                          "Tyrannosaurus"), c("Tyrannosaurus rex",
                          "Spinosaurus aegyptiacus", NA, NA,
                          "Tyrannosaurus rex"), c("Tyrannosauridae",
                          "Spinosauridae", "Diplodocidae", NA,
                          "Tyrannosauridae"), c("Coelurosauria", "Orionides",
                          NA, NA, "Coelurosauria"), c("Tetanurae", "Tetanurae",
                          NA, "Neosauropoda", "Tetanurae"))
  colnames(dinosaurs) <- c("species", "genus", "binomial", "family", "order",
                           "class")

  #expect equal
  expect_equal(ncol(tax_unique(occdf = tetrapods, genus = "genus", family =
                                 "family", order = "order", class = "class",
                               name = "accepted_name")), 6)
  expect_equal(ncol(tax_unique(occdf = dinosaurs, species = "species", genus =
                                 "genus", family = "family", order = "order",
                               class = "class")), 6)
  expect_equal(ncol(tax_unique(occdf = dinosaurs, binomial = "binomial",
                               family = "family", order = "order",
                               class = "class")), 6)
  expect_equal(ncol(tax_unique(occdf = dinosaurs, binomial = "binomial",
                               family = "family", order = "order",
                               class = "class", resolution = "genus")), 5)
  expect_equal(nrow(tax_unique(occdf = dinosaurs, species = "species", genus =
                                 "genus", family = "family", order = "order",
                               class = "class")), 4)
  expect_equal(nrow(tax_unique(occdf = dinosaurs, binomial = "binomial",
                               family = "family", order = "order",
                               class = "class")), 4)

  #expect true
  expect_true(is.data.frame(tax_unique(occdf = tetrapods, genus = "genus",
                                       family = "family", order = "order",
                                       class = "class", name =
                                         "accepted_name")))
  expect_true(is.data.frame(tax_unique(occdf = dinosaurs, species = "species",
                                       genus = "genus", family = "family",
                                       order = "order", class = "class")))

  #expect error
  expect_error(tax_unique(species = "species", genus = "genus"))
  expect_error(tax_unique(occdf = 100))
  expect_error(tax_unique(occdf = tetrapods, binomial = "test"))
  expect_error(tax_unique(occdf = tetrapods, genus = "test"))
  expect_error(tax_unique(occdf = dinosaurs, species = "species", genus =
                            "genus"))
  expect_error(tax_unique(occdf = dinosaurs, species = "species", genus =
                            "genus", family = "test"))
  expect_error(tax_unique(occdf = dinosaurs, species = "species", genus =
                            "genus", family = "family", order = "test"))
  expect_error(tax_unique(occdf = dinosaurs, species = "species", genus =
                            "genus", family = "family", order = "order",
                            class = "test"))
  expect_error(tax_unique(occdf = dinosaurs, genus = "genus", family = "family",
                          order = "order", class = "class", name = "test"))
  expect_error(tax_unique(occdf = dinosaurs, genus = "genus", family = "family"
                          ))
  expect_error(tax_unique(occdf = dinosaurs, species = "species", family =
                            "family", resolution = "genus"))
  expect_error(tax_unique(occdf = tetrapods, genus = "genus", family = "family",
                          name = "identified_name"))
  expect_error(tax_unique(occdf = dinosaurs, species = "species", genus =
                            "genus", family = "family", resolution = "test"))
})
