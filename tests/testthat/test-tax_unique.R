test_that("tax_unique() works", {

  #expect equal
  expect_equal(ncol(tax_unique(species = c("rex", "aegyptiacus"),
                          genus = c("Tyrannosaurus", "Spinosaurus"),
                          family = c("Tyrannosauridae", "Spinosauridae"))), 5)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", NA),
                          genus = c("Tyrannosaurus", "Spinosaurus", NA),
                          family = c("Tyrannosauridae", "Spinosauridae",
                                     "Diplodocidae"))), 3)
  expect_equal(nrow(tax_unique(species = c("rex", "aegyptiacus", NA),
                               genus = c("Tyrannosaurus", "Spinosaurus", NA),
                               family = c("Tyrannosauridae", "Spinosauridae",
                                          "Tyrannosauridae"))), 2)

  #expect true
  expect_true(is.data.frame(tax_unique(species = c("rex", "aegyptiacus"),
                              genus = c("Tyrannosaurus", "Spinosaurus"),
                              family = c("Tyrannosauridae", "Spinosauridae"))))

  #expect error
  expect_error(tax_unique())
  expect_error(tax_unique(paleobioDB = 100))
  expect_error(tax_unique(paleobioDB = 100, species = 100))
  expect_error(tax_unique(species = "yes", genus = "no"))
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
                          by = TRUE))

})
