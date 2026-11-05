test_that("basic behavior works", {
  # fmt: skip
  data <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )
  expect_equal(
    tax_range_strat(data),
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
  expect_s3_class(tax_range_strat(data), "palaeoverse_tax_range_strat")

  # input checks
  expect_snapshot(tax_range_strat(data.frame()), error = TRUE)
  expect_snapshot(tax_range_strat(NULL), error = TRUE)
  expect_snapshot(tax_range_strat(NA), error = TRUE)
  expect_snapshot(tax_range_strat("a"), error = TRUE)
})

test_that("piping and not piping the first argument give the same result", {
  # fmt: skip
  data <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )
  expect_equal(
    data |> tax_range_strat(name = "genus"),
    tax_range_strat(data, name = "genus")
  )
})

test_that("tax_range_strat errors with unnamed args", {
  data <- data.frame(
    genus = c("Anconastes", "Procolophon", "Procolophon"),
    bed = c(1, 1, 2)
  )
  expect_snapshot(tax_range_strat(data, "genus"), error = TRUE)
  expect_snapshot(tax_range_strat(data, "genus"), error = TRUE)
  expect_snapshot(tax_range_strat(data, "genus", "bed"), error = TRUE)
  expect_snapshot(tax_range_strat(data, "genus", level = "bed"), error = TRUE)
})

test_that("argument 'name' works", {
  # fmt: skip
  data <- data.frame(
    species = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  expect_equal(
    tax_range_strat(data, name = "species"),
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
  expect_snapshot(tax_range_strat(data, name = "test"), error = TRUE)
  expect_snapshot(tax_range_strat(data, name = character(0)), error = TRUE)
  expect_snapshot(tax_range_strat(data, name = NA), error = TRUE)
  expect_snapshot(tax_range_strat(data, name = 1), error = TRUE)
  nadf <- data
  nadf$genus[1] <- NA
  expect_snapshot(tax_range_strat(nadf), error = TRUE)
})

test_that("argument 'level' works", {
  # fmt: skip
  data <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    height = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  expect_equal(
    tax_range_strat(data, level = "height"),
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
  expect_snapshot(tax_range_strat(data, level = "test"), error = TRUE)
  expect_snapshot(tax_range_strat(data, level = character(0)), error = TRUE)
  expect_snapshot(tax_range_strat(data, level = NA), error = TRUE)
  expect_snapshot(tax_range_strat(data, level = 1), error = TRUE)
  nadf <- data
  nadf$bed[1] <- NA
  expect_snapshot(tax_range_strat(nadf), error = TRUE)
})

test_that("argument 'group' works", {
  # fmt: skip
  data <- data.frame(
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
    tax_range_strat(data, group = "class"),
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
    tax_range_strat(data, group = c("class", "genus")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(data, group = "test"), error = TRUE)
  expect_snapshot(tax_range_strat(data, group = character(0)), error = TRUE)
  expect_snapshot(tax_range_strat(data, group = NA), error = TRUE)
  expect_snapshot(tax_range_strat(data, group = 1), error = TRUE)
})

test_that("argument 'certainty' works", {
  # fmt: skip
  data <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  # A certainty column adds columns for the range of certain identifications
  expect_equal(
    tax_range_strat(data, certainty = "certainty"),
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
    tax_range_strat(data, certainty = c("class", "genus")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(data, certainty = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(data, certainty = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(data, certainty = NA), error = TRUE)
  expect_snapshot(tax_range_strat(data, certainty = 1), error = TRUE)
})

test_that("argument 'by' works", {
  # fmt: skip
  data <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1)
  )

  expect_equal(
    tax_range_strat(data, by = "FAD")$taxon,
    c("Anconastes", "Procolophon", "Araeoscelis", "Edaphosaurus")
  )
  expect_equal(
    tax_range_strat(data, by = "LAD")$taxon,
    c("Anconastes", "Araeoscelis", "Edaphosaurus", "Procolophon")
  )
  # "name" sorts alphabetically by taxon name
  expect_equal(
    tax_range_strat(data, by = "name")$taxon,
    c("Anconastes", "Araeoscelis", "Edaphosaurus", "Procolophon")
  )
  # input checks
  expect_snapshot(
    tax_range_strat(data, by = c("FAD", "LAD")),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(data, by = "test"), error = TRUE)
  expect_snapshot(
    tax_range_strat(data, by = character(0)),
    error = TRUE
  )
  expect_snapshot(tax_range_strat(data, by = NA), error = TRUE)
  expect_snapshot(tax_range_strat(data, by = 1), error = TRUE)
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
  expect_doppelganger("tax_range_strat() sorts", function() {
    plot(tax_range_strat(occdf, by = "LAD"))
  })
  expect_doppelganger("tax_range_strat() plots groups", function() {
    plot(tax_range_strat(occdf, group = "class"))
  })
  expect_doppelganger("tax_range_strat() plots uncertainty", function() {
    plot(tax_range_strat(occdf, certainty = "certainty"))
  })
})

test_that("tax_range_strat plotting with extra args works", {
  # fmt: skip
  data <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis",
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4),
    certainty = c(1, 1, 0, 1, 0, 1, 1, 1),
    class = c("A", "A", "B", "A", "C", "C", "C", "B")
  )

  # Arguments are passed to the underlying plot (e.g. the y-axis label)
  expect_doppelganger("tax_range_strat() labels", function() {
    plot(tax_range_strat(data), ylab = "Height (m)")
  })

  expect_doppelganger("tax_range_strat-many-args", function() {
    plot(
      tax_range_strat(data, certainty = "certainty"),
      main = "Section A",
      xlab = "x axis",
      ylab = "y axis",
      col = c("red", "blue"),
      bg = "grey",
      lty = 3,
      lwd = c(5, 2),
      cex = 2,
      pch = 21,
      ylim = c(0, 6)
    )
  })

  # `col.group` must be of length 1 or the same length as the number of groups
  expect_doppelganger("tax_range_strat-col-group", function() {
    plot(
      tax_range_strat(data, group = "class"),
      col.group = c("red", "blue", "green")
    )
  })
  expect_doppelganger("tax_range_strat-col-group-single", function() {
    plot(
      tax_range_strat(data, group = "class"),
      col.group = "grey90"
    )
  })
  expect_doppelganger("tax_range_strat-col-group-null", function() {
    plot(
      tax_range_strat(data, group = "class"),
      col.group = NULL
    )
  })
  expect_snapshot(
    plot(tax_range_strat(data, group = "class"), col.group = c("red", "blue")),
    error = TRUE
  )

  # forbidden args
  expect_snapshot(
    plot(tax_range_strat(data), y = 1, axes = TRUE, type = "l", xaxs = "r"),
    error = TRUE
  )
})

test_that("plot resets par and returns the settings to add elements", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    bed = c(1, 2, 2, 4, 5)
  )
  expect_doppelganger("tax_range_strat-add-elements", function() {
    old_mar <- par("mar")
    out <- plot(tax_range_strat(data))
    expect_identical(par("mar"), old_mar)
    expect_named(out, c("mar", "usr"))
    op <- par(out)
    abline(h = 3, col = "red", lwd = 3)
    points(x = 2, y = 4, pch = 4, cex = 3)
    par(op)
    expect_identical(par("mar"), old_mar)
  })
})

test_that("plotting with custom axis call works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    bed = c(1, 2, 2, 4, 5)
  )
  expect_doppelganger("tax_range_strat-custom-axis", function() {
    plot(
      tax_range_strat(data),
      axis = axis(2, lty = "dashed", lwd = 5, col = "red")
    )
  })
})

test_that("plot argument 'axis' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    bed = c(1, 2, 2, 4, 5)
  )

  # input checks
  expect_snapshot(
    plot(tax_range_strat(occdf), axis = "periods"),
    error = TRUE
  )
  expect_snapshot(
    plot(tax_range_strat(occdf), axis = axis(side = 5)),
    error = TRUE
  )
})

test_that("arguments 'plot_args', 'x_args' and 'y_args' are hard deprecated", {
  # fmt: skip
  occdf <- data.frame(
    genus = c(
      "Anconastes", "Procolophon", "Procolophon", "Anconastes", "Araeoscelis", "Araeoscelis", 
      "Edaphosaurus", "Procolophon"
    ),
    bed = c(1, 1, 2, 2, 2, 3, 3, 4)
  )
  expect_snapshot(tax_range_strat(occdf, x_args = list(las = 1)), error = TRUE)
  expect_snapshot(tax_range_strat(occdf, y_args = list(las = 1)), error = TRUE)
  expect_snapshot(
    tax_range_strat(occdf, plot_args = list(ylab = "Height (m)")),
    error = TRUE
  )
})
