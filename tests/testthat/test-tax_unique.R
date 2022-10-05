test_that("tax_unique() works", {
  data(tetrapods)

  #expect equal
  expect_equal(ncol(tax_unique(paleobioDB = tetrapods)), 6)
  expect_equal(ncol(tax_unique(species = c("rex", "aegyptiacus"),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = c("Tyrannosauridae", "Spinosauridae"))), 5)
  expect_equal(ncol(tax_unique(species = c("rex", "aegyptiacus"),
                               genus = c("Tyrannosaurus", "Spinosaurus"),
                               family = c("Tyrannosauridae", "Spinosauridae"))),
                               order = c("Coelurosauria", "Orionides"),
                               class = c("Tetanurae", "Tetanurae"), 5)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", NA),
                          genus = c("Tyrannosaurus", "Spinosaurus", NA),
                          family = c("Tyrannosauridae", "Spinosauridae",
                                     "Diplodocidae"))), 3)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", NA),
                               genus = c("Tyrannosaurus", "Spinosaurus", NA),
                               family = c("Tyrannosauridae", "Spinosauridae",
                                          NA),
                               order = c("Coelurosauria", "Orionides",
                                          "Diplodocimorpha"))), 3)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", NA),
                               genus = c("Tyrannosaurus", "Spinosaurus", NA),
                               family = c("Tyrannosauridae", "Spinosauridae",
                                          NA),
                               order = c("Coelurosauria", "Orionides", NA),
                               class = c("Tetanurae", "Tetanurae",
                                         "Neosauropoda"))), 3)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", "imperator"),
                               genus = c("Tyrannosaurus", "Spinosaurus",
                                         "Tyrannosaurus"),
                               family = c("Tyrannosauridae", "Spinosauridae",
                                          "Tyrannosauridae"),
                              resolution = "genera")), 2)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", NA),
                               genus = c("Tyrannosaurus", "Spinosaurus", NA),
                               family = c("Tyrannosauridae", "Spinosauridae",
                                          "Tyrannosauridae"))), 2)

  #expect true
  expect_true(is.data.frame(tax_unique(paleobioDB = tetrapods)))
  expect_true(is.data.frame(tax_unique(species = c("rex", "aegyptiacus"),
                              genus = c("Tyrannosaurus", "Spinosaurus"),
                              family = c("Tyrannosauridae", "Spinosauridae"))))

  #expect error
  expect_error(tax_unique(paleobioDB = 100))
  expect_error(tax_unique(paleobioDB = 100, species = 100))
  expect_error(tax_unique(paleobioDB = subset(tetrapods, select = -genus)))
  expect_error(tax_unique(paleobioDB = tetrapods, group = "test"))
  expect_error(tax_unique(species = c("rex", "aegyptiacus", NA),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = tetrapods))
  expect_error(tax_unique(species = c("rex", "aegyptiacus", NA),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = c("Tyrannosauridae", "Spinosauridae")))
  expect_error(tax_unique(species = c("? rex", "aegyptiacus"),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = c("Tyrannosauridae", "Spinosauridae")))
  expect_error(tax_unique(species = c("rex", "aegyptiacus"),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = c("Tyrannosauridae", "Spinosauridae"),
                          resolution = TRUE))
  expect_error(tax_unique(species = c("rex", "aegyptiacus"),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = c("Tyrannosauridae", "Spinosauridae"),
                          group = TRUE))

})
