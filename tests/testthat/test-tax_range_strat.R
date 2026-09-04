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
    tax_range_strat(occdf = occdf),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      tmp_group = "1"
    )
  )

  # It produces the expected plot
  expect_doppelganger("tax_range_strat() plots", function() {
    tax_range_strat(occdf = occdf)
  })

  # input checks
  expect_snapshot(tax_range_strat(occdf = data.frame()), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = NULL), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = "a"), error = TRUE)
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
  expect_snapshot(tax_range_strat(occdf = occdf, "genus"), error = TRUE)
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
    tax_range_strat(occdf = occdf, name = "species"),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      tmp_group = "1"
    )
  )

  # input checks
  # Those give a warning instead of an error on R < 4.3
  skip_if(getRversion() < "4.3")
  expect_snapshot(tax_range_strat(occdf = occdf, name = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf = occdf, name = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, name = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = occdf, name = 1), error = TRUE)
  nadf <- occdf
  nadf$genus[1] <- NA
  expect_snapshot(tax_range_strat(occdf = nadf), error = TRUE)
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
    tax_range_strat(occdf = occdf, level = "height"),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      tmp_group = "1"
    )
  )

  # input checks
  expect_snapshot(tax_range_strat(occdf = occdf, level = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf = occdf, level = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, level = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = occdf, level = 1), error = TRUE)
  nadf <- occdf
  nadf$bed[1] <- NA
  expect_snapshot(tax_range_strat(occdf = nadf), error = TRUE)
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
    tax_range_strat(occdf = occdf, group = "class"),
    data.frame(
      ID = 1:6,
      taxon = c(
        "Anconastes", "Edaphosaurus", "Procolophon", "Araeoscelis", "Procolophon",
        "Araeoscelis"
      ),
      min_bin = c(1, 3, 1, 2, 2, 3),
      max_bin = c(2, 3, 4, 2, 2, 3),
      class = rep(c("Osteichthyes", "Reptilia", "Saurischia"), each = 2L)
    )
  )

  # It produces the expected plot
  expect_doppelganger("tax_range_strat() plots groups", function() {
    tax_range_strat(occdf = occdf, group = "class")
  })

  # input checks
  expect_snapshot(
    tax_range_strat(occdf = occdf, group = c("class", "genus")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, group = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf = occdf, group = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, group = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = occdf, group = 1), error = TRUE)
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
    tax_range_strat(occdf = occdf, certainty = "certainty"),
    data.frame(
      ID = 1:4,
      taxon = c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus"),
      group = NA,
      min_bin = c(1, 1, 2, 3),
      max_bin = c(2, 4, 3, 3),
      min_bin_certain = rep(c(1, 3), each = 2L),
      max_bin_certain = c(2, 4, 3, 3),
      tmp_group = "1"
    )
  )

  # It produces the expected plot
  expect_doppelganger("tax_range_strat() does uncertainty", function() {
    tax_range_strat(occdf = occdf, certainty = "certainty")
  })

  # input checks
  expect_snapshot(
    tax_range_strat(occdf = occdf, certainty = c("class", "genus")),
    error = TRUE
  )
  expect_snapshot(
    tax_range_strat(occdf = occdf, certainty = "test"),
    error = TRUE
  )
  expect_snapshot(
    tax_range_strat(occdf = occdf, certainty = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, certainty = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = occdf, certainty = 1), error = TRUE)
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
    tax_range_strat(occdf = occdf, by = "FAD")$taxon,
    c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus")
  )
  expect_equal(
    tax_range_strat(occdf = occdf, by = "LAD")$taxon,
    c("Anconastes", "Araeoscelis", "Edaphosaurus", "Procolophon")
  )
  # "name" sorts alphabetically by taxon name
  expect_equal(
    tax_range_strat(occdf = occdf, by = "name")$taxon,
    c("Anconastes", "Araeoscelis", "Edaphosaurus", "Procolophon")
  )

  # It produces the expected plot
  expect_doppelganger("tax_range_strat() sorts", function() {
    tax_range_strat(occdf = occdf, by = "LAD")
  })

  # input checks
  expect_snapshot(
    tax_range_strat(occdf = occdf, by = c("FAD", "LAD")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, by = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf = occdf, by = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(occdf = occdf, by = NA), error = TRUE)
  expect_snapshot(tax_range_strat(occdf = occdf, by = 1), error = TRUE)
})

test_that("argument 'plot_args' works", {
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
    tax_range_strat(occdf = occdf, plot_args = list(ylab = "Height (m)"))
  })

  # Unsupported arguments ("type") are overridden rather than passed through
  expect_doppelganger("tax_range_strat() stops some plot_args", function() {
    tax_range_strat(
      occdf = occdf,
      plot_args = list(type = "line", ylab = "Height (m)"),
      x_args = list(side = 1),
      y_args = list(side = 2)
    )
  })
})
