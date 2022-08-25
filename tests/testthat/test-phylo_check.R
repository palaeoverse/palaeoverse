test_that("phylo_check() works", {
  library(paleotree)
  data(RaiaCopesRule)
  tree <- ceratopsianTreeRaia
  list <- c("Nasutoceratops_titusi", "Diabloceratops_eatoni",
            "Zuniceratops_christopheri", "Psittacosaurus_major",
            "Psittacosaurus_sinensis", "Avaceratops_lammersi",
            "Xenoceratops_foremostensis", "Leptoceratops_gracilis",
            "Triceratops_horridus", "Triceratops_prorsus")

  #expect equal
  expect_equal(nrow(phylo_check(tree, list)), 3)
  expect_equal(nrow(phylo_check(tree, list, out = "table")), 40)
  expect_equal(nrow(phylo_check(tree, list = "PsiTTacOsaurUs sinEnsIs",
                                out = "table")), 37)

  #expect true
  expect_true(is.data.frame(phylo_check(tree, list)))
  expect_true(is.data.frame(phylo_check(tree, list, out = "table")))
  expect_true(inherits(phylo_check(tree, list, out = "tree"), "phylo"))

  #expect error
  expect_error(phylo_check(tree))
  expect_error(phylo_check(list))
  expect_error(phylo_check(tree = tree, list = tree))
  expect_error(phylo_check(tree, list, out = "test"))
  expect_error(phylo_check(tree, list = "."))
})
