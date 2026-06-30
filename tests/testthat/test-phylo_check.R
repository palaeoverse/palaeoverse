test_that("basic behavior works", {
  skip_if_not_installed("paleotree")
  library(paleotree)
  data(RaiaCopesRule)
  tree <- ceratopsianTreeRaia
  list <- c(
    "Nasutoceratops_titusi",
    "Diabloceratops_eatoni",
    "Zuniceratops_christopheri",
    "Psittacosaurus_major",
    "Psittacosaurus_sinensis",
    "Avaceratops_lammersi",
    "Xenoceratops_foremostensis",
    "Leptoceratops_gracilis",
    "Triceratops_horridus",
    "Triceratops_prorsus"
  )

  out <- phylo_check(tree, list)
  expect_equal(nrow(out), 40)
  expect_equal(ncol(out), 3)
  expect_true(unique(out[out$taxon_name %in% list, "present_in_list"]))

  # input checks
  expect_snapshot(phylo_check(), error = TRUE)
  expect_snapshot(phylo_check(1), error = TRUE)
  expect_snapshot(phylo_check(data.frame()), error = TRUE)
  expect_snapshot(phylo_check(NA), error = TRUE)
})

test_that("arg 'list' works", {
  skip_if_not_installed("paleotree")
  library(paleotree)
  data(RaiaCopesRule)
  tree <- ceratopsianTreeRaia

  out <- phylo_check(tree, c("abc", "def"))
  # TODO: do we expect the change from "abc" -> "Abc"?
  expect_equal(out$taxon_name, c("Abc", "Def", tree$tip.label))

  # input checks
  expect_snapshot(phylo_check(tree), error = TRUE)

  # TODO: all those cases should error, docs say it should be a character vector
  # expect_snapshot(phylo_check(tree, list = 1), error = TRUE)
  # expect_snapshot(phylo_check(tree, list = list()), error = TRUE)
  # expect_snapshot(phylo_check(tree, list = NA), error = TRUE)

  # TODO: what should be done with NAs?
  # expect_snapshot(phylo_check(tree, list = c("a", NA)), error = TRUE)
})

test_that("arg 'out' works", {
  skip_if_not_installed("paleotree")
  library(paleotree)
  data(RaiaCopesRule)
  tree <- ceratopsianTreeRaia
  list <- c(
    "Nasutoceratops_titusi",
    "Diabloceratops_eatoni",
    "Zuniceratops_christopheri",
    "Psittacosaurus_major",
    "Psittacosaurus_sinensis",
    "Avaceratops_lammersi",
    "Xenoceratops_foremostensis",
    "Leptoceratops_gracilis",
    "Triceratops_horridus",
    "Triceratops_prorsus"
  )

  # diff_table -> only keep taxons present in tree but not in list, OR present in list but
  #               not in tree
  #            -> i.e. cannot have rows where both columns are true or both are false
  out <- phylo_check(tree, list, out = "diff_table")
  expect_equal(nrow(out), 33)
  expect_equal(ncol(out), 3)
  expect_all_false(out$present_in_tree & out$present_in_list)
  expect_all_false(!out$present_in_tree & !out$present_in_list)

  # counts -> summary table containing the number of taxa in the list but not the tree,
  #           in the tree but not the list, and in both
  out <- phylo_check(tree, list, out = "counts")
  expect_equal(
    out,
    data.frame(
      category = c("tree_and_list", "only_in_tree", "only_in_list"),
      number_of_taxa = c(7, 30, 3)
    )
  )

  # tree -> a phylo object consisting of the input phylogeny trimmed to only include
  #         the tips present in the list
  out <- phylo_check(tree, list, out = "tree")
  expect_s3_class(out, "phylo")
  # fmt: skip
  expect_equal(
    out,
    list(
      edge = matrix(
        c(
          8L, 9L, 10L, 11L, 11L, 10L, 12L, 12L, 9L, 8L, 13L, 13L, 9L, 10L, 11L, 
          1L, 2L, 12L, 3L, 4L, 5L, 13L, 6L, 7L
        ),
        nrow = 12L,
        ncol = 2L
      ),
      Nnode = 6L,
      tip.label = c(
        "Avaceratops_lammersi",
        "Diabloceratops_eatoni",
        "Triceratops_horridus",
        "Triceratops_prorsus",
        "Leptoceratops_gracilis",
        "Psittacosaurus_major",
        "Psittacosaurus_sinensis"
      ),
      edge.length = c(37.5, 18.9, 1.1, 6, 3, 15.5, 2, 2, 36.1, 2.5, 7.5, 26.5),
      root.edge = 3,
      root.time = 163
    ) |>
      structure(class = "phylo", order = "cladewise")
  )

  # input checks
  expect_snapshot(phylo_check(tree, list, out = "foo"), error = TRUE)
  expect_snapshot(phylo_check(tree, list, out = 1), error = TRUE)
  expect_snapshot(phylo_check(tree, list, out = NA), error = TRUE)
  expect_snapshot(phylo_check(tree, list, out = NULL), error = TRUE)
})

test_that("arg 'sort' works", {
  skip_if_not_installed("paleotree")
  library(paleotree)
  data(RaiaCopesRule)
  tree <- ceratopsianTreeRaia
  list <- c(
    "Nasutoceratops_titusi",
    "Diabloceratops_eatoni",
    "Zuniceratops_christopheri",
    "Psittacosaurus_major",
    "Psittacosaurus_sinensis",
    "Avaceratops_lammersi",
    "Xenoceratops_foremostensis",
    "Leptoceratops_gracilis",
    "Triceratops_horridus",
    "Triceratops_prorsus"
  )

  # arg "sorted" is equivalent to sorting the default output by hand
  sorted <- phylo_check(tree, list, sort = "az")
  unsorted <- phylo_check(tree, list)
  expect_equal(sorted, unsorted[order(unsorted$taxon_name), ])

  sorted <- phylo_check(tree, list, out = "diff_table", sort = "az")
  unsorted <- phylo_check(tree, list, out = "diff_table")
  expect_equal(sorted, unsorted[order(unsorted$taxon_name), ])

  # input checks
  expect_snapshot(phylo_check(tree, list, sort = "foo"), error = TRUE)
  expect_snapshot(phylo_check(tree, list, sort = 1), error = TRUE)
  expect_snapshot(phylo_check(tree, list, sort = NA), error = TRUE)
  expect_snapshot(phylo_check(tree, list, sort = NULL), error = TRUE)

  # TODO: should it error since "sort" is useless when out = "tree" or "counts"
  # expect_snapshot(
  #   phylo_check(tree, list, out = "tree", sort = "az"),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   phylo_check(tree, list, out = "counts", sort = "az"),
  #   error = TRUE
  # )
})
