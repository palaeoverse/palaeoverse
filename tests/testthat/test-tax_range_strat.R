test_that("basic behavior works", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )
  expect_equal(
    tax_range_strat(occdf),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      tmp_group = "1"
    ),
    ignore_attr = TRUE
  )

  # input checks
  expect_snapshot(tax_range_strat(data.frame()), error = TRUE)
  expect_snapshot(tax_range_strat(NULL), error = TRUE)
  expect_snapshot(tax_range_strat(NA), error = TRUE)
  expect_snapshot(tax_range_strat("a"), error = TRUE)
})

test_that("piping and not piping the first argument give the same result", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )
  expect_equal(
    occdf |> tax_range_strat(name = "genus"),
    tax_range_strat(occdf, name = "genus")
  )
})

test_that("tax_range_strat errors with unnamed args", {
  occdf <- data.frame(
    genus = c("Anconastes", "Procolophon", "Procolophon"),
    bed = c(1, 1, 2)
  )
  expect_snapshot(tax_range_strat(occdf, "genus"), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, "genus"), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, "genus", "bed"), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, "genus", level = "bed"), error = TRUE)
})

test_that("argument 'name' works", {
  # fmt: skip
  occdf <- data.frame(
    species = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  expect_equal(
    tax_range_strat(occdf, name = "species"),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      tmp_group = "1"
    ),
    ignore_attr = TRUE
  )

  # input checks
  # Those give a warning instead of an error on R < 4.3
  skip_if(getRversion() < "4.3")
  expect_snapshot(tax_range_strat(occdf, name = "test"), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, name = character(0)), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, name = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, name = 1), error = TRUE)
  nadf <- occdf
  nadf$genus[1] <- NA
  expect_snapshot(tax_range_strat(nadf), error = TRUE)
})

test_that("argument 'level' works", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    height = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  expect_equal(
    tax_range_strat(occdf, level = "height"),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      tmp_group = "1"
    ),
    ignore_attr = TRUE
  )

  # input checks
  expect_snapshot(tax_range_strat(occdf, level = "test"), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, level = character(0)), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, level = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, level = 1), error = TRUE)
  nadf <- occdf
  nadf$bed[1] <- NA
  expect_snapshot(tax_range_strat(nadf), error = TRUE)
})

test_that("argument 'group' works", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1),
    class = c(
      "Osteichthyes", "Reptilia", "Saurischia", "Osteichthyes", "Reptilia", "Saurischia",
      "Osteichthyes", "Reptilia"
    )
  )

  # fmt: skip
  expect_equal(
    tax_range_strat(occdf, group = "class"),
    data.frame(
      ID = 1:6,
      taxon = c(
        "Anconastes", "Edaphosaurus", "Procolophon", "Araeoscelis", "Procolophon",
        "Araeoscelis"
      ),
      min_bin = c(1, 3, 1, 2, 2, 3),
      max_bin = c(2, 3, 4, 2, 2, 3),
      class = rep(c("Osteichthyes", "Reptilia", "Saurischia"), each = 2L)
    ),
    ignore_attr = TRUE
  )

  # input checks
  expect_snapshot(
    tax_range_strat(occdf, group = c("class", "genus")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf, group = "test"), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, group = character(0)), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, group = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, group = 1), error = TRUE)
})

test_that("argument 'certainty' works", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  # A certainty column adds columns for the range of certain identifications
  expect_equal(
    tax_range_strat(occdf, certainty = "certainty"),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      min_bin_certain = rep(c(1, 3), each = 2L),
      max_bin_certain = c(2, 4, 3, 3),
      tmp_group = "1"
    ),
    ignore_attr = TRUE
  )

  # input checks
  expect_snapshot(
    tax_range_strat(occdf, certainty = c("class", "genus")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf, certainty = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf, certainty = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf, certainty = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, certainty = 1), error = TRUE)
})

test_that("argument 'by' works", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  expect_equal(
    tax_range_strat(occdf, by = "FAD")$taxon,
    c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus")
  )
  expect_equal(
    tax_range_strat(occdf, by = "LAD")$taxon,
    c("Anconastes", "Araeoscelis", "Edaphosaurus", "Procolophon")
  )
  # "name" sorts alphabetically by taxon name
  expect_equal(
    tax_range_strat(occdf, by = "name")$taxon,
    c("Anconastes", "Araeoscelis", "Edaphosaurus", "Procolophon")
  )
  # input checks
  expect_snapshot(
    tax_range_strat(occdf, by = c("FAD", "LAD")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf, by = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf, by = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf, by = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, by = 1), error = TRUE)
})

test_that("plotting works", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1),
    class = c(
      "Osteichthyes", "Reptilia", "Saurischia", "Osteichthyes", "Reptilia", "Saurischia",
      "Osteichthyes", "Reptilia"
    )
  )

  expect_doppelganger("tax_range_strat() plots", function() {
    plot(tax_range_strat(occdf))
  })

  expect_doppelganger("tax_range_strat() plots groups", function() {
    plot(tax_range_strat(occdf, group = "class"))
  })

  expect_doppelganger("tax_range_strat() plots uncertainty", function() {
    plot(tax_range_strat(occdf, certainty = "certainty"))
  })
  expect_doppelganger("tax_range_strat() plots sort", function() {
    plot(tax_range_strat(occdf, by = "LAD"))
  })
})

test_that("plotting works with extra args", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  # Arguments are passed to the underlying plot (e.g. the y-axis label)
  expect_doppelganger("tax_range_strat() labels", function() {
    plot(tax_range_strat(occdf), ylab = "Height (m)")
  })
})
