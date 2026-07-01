test_that("basic behavior works", {
  taxdf <- data.frame(
    name = c("A", "B"),
    max_ma = c(150, 30),
    min_ma = c(110, 0)
  )

  out <- tax_expand_time(taxdf)
  # fmt: skip
  expect_equal(
    out[, c("name", "interval_name")],
    data.frame(
      name = rep(c("A", "B"), c(7, 17)),
      interval_name = c(
        "Tithonian", "Berriasian", "Valanginian", "Hauterivian", "Barremian",
        "Aptian", "Albian", "Rupelian", "Chattian", "Aquitanian", "Burdigalian",
        "Langhian", "Serravallian", "Tortonian", "Messinian", "Zanclean",
        "Piacenzian", "Gelasian", "Calabrian", "Chibanian", "Upper Pleistocene",
        "Greenlandian", "Northgrippian", "Meghalayan"
      )
    )
  )
  # fmt: skip
  expect_named(
    out,
    c(
      "name", "max_ma", "min_ma", "ext", "orig", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # input checks
  expect_snapshot(tax_expand_time(), error = TRUE)
  expect_snapshot(tax_expand_time(data.frame()), error = TRUE)
  expect_snapshot(tax_expand_time(1), error = TRUE)
  expect_snapshot(tax_expand_time(NULL), error = TRUE)
})

test_that("rows must be unique", {
  taxdf <- data.frame(
    name = c("A", "A", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, 0)
  )
  expect_snapshot(tax_expand_time(taxdf), error = TRUE)
})

test_that("ages must be positive", {
  taxdf <- data.frame(
    name = c("A", "A", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, -20)
  )
  expect_snapshot(tax_expand_time(taxdf), error = TRUE)
})

test_that("max ages must be larger than or equal to min ages", {
  taxdf <- data.frame(
    name = c("A", "B", "C"),
    max_ma = c(150, 150, 30),
    min_ma = c(110, 110, 40)
  )
  expect_snapshot(tax_expand_time(taxdf), error = TRUE)
})

test_that("arg 'bins' works", {
  bins <- data.frame(
    bin = 1:4,
    max_ma = c(160, 120, 80, 40),
    min_ma = c(120, 80, 40, 0)
  )

  taxdf <- data.frame(
    name = c("A", "B"),
    max_ma = c(150, 30),
    min_ma = c(110, 0)
  )

  # TODO: this behavior should be fixed
  # https://github.com/palaeoverse/palaeoverse/issues/245

  # "A" spans 110-150 Ma, so it falls in bins 1 (120-160) and 2 (80-120);
  # "B" spans 20-60 Ma, so it falls in bins 3 (40-80) and 4 (0-40);
  # "C" spans 0-30 Ma, so it falls in bin 4 (0-40) only.
  # `ext` flags the bin in which a taxon goes extinct (its LAD), and `orig`
  # the bin in which it originates (its FAD). Note that `bins` also carries
  # "max_ma"/"min_ma" columns, which are appended after those of `taxdf`.
  expect_equal(
    tax_expand_time(taxdf, bins = bins),
    # jarl-ignore duplicated_arguments: this will be fixed later
    data.frame(
      name = c("A", "A", "B"),
      max_ma = c(150, 150, 30),
      min_ma = c(110, 110, 0),
      ext = c(FALSE, TRUE, FALSE),
      orig = c(TRUE, FALSE, TRUE),
      bin = c(1L, 2L, 4L),
      max_ma = c(160, 120, 40),
      min_ma = c(120, 80, 0),
      check.names = FALSE
    )
  )

  # input checks
  expect_snapshot(tax_expand_time(taxdf, bins = data.frame()), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, bins = 1), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, bins = NA), error = TRUE)
})

test_that("args 'max_ma' and 'min_ma' work", {
  bins <- data.frame(
    bin = 1:4,
    max_ma = c(160, 120, 80, 40),
    min_ma = c(120, 80, 40, 0)
  )

  taxdf <- data.frame(
    name = c("A", "B"),
    fad = c(150, 30),
    lad = c(110, 0)
  )

  # pointing to the renamed columns reproduces the basic behavior
  expect_equal(
    tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = "lad"),
    data.frame(
      name = c("A", "A", "B"),
      fad = c(150, 150, 30),
      lad = c(110, 110, 0),
      ext = c(FALSE, TRUE, FALSE),
      orig = c(TRUE, FALSE, TRUE),
      bin = c(1L, 2L, 4L),
      max_ma = c(160, 120, 40),
      min_ma = c(120, 80, 0)
    )
  )

  # the default "max_ma"/"min_ma" column names are absent
  expect_snapshot(tax_expand_time(taxdf, bins = bins), error = TRUE)

  # input checks on `max_ma`
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = "nonexistent", min_ma = "lad"),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = NULL, min_ma = "lad"),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(
      taxdf,
      bins = bins,
      max_ma = character(0),
      min_ma = "lad"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = NA, min_ma = "lad"),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = c("a", "b"), min_ma = "lad"),
    error = TRUE
  )

  # input checks on `min_ma`
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = "nonexistent"),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = NULL),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(
      taxdf,
      bins = bins,
      max_ma = "fad",
      min_ma = character(0)
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = NA),
    error = TRUE
  )
  expect_snapshot(
    tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = c("a", "b")),
    error = TRUE
  )
})

test_that("arg 'scale' works", {
  bins <- data.frame(
    bin = 1:4,
    max_ma = c(160, 120, 80, 40),
    min_ma = c(120, 80, 40, 0)
  )

  taxdf <- data.frame(
    name = c("A", "B"),
    max_ma = c(150, 30),
    min_ma = c(110, 0)
  )

  out <- tax_expand_time(taxdf, scale = "GTS2012")

  # fmt: skip
  expect_equal(
    out[, c("name", "interval_name")],
    data.frame(
      name = rep(c("A", "B"), c(7L, 15L)),
      interval_name = c(
        "Tithonian", "Berriasian", "Valanginian", "Hauterivian", "Barremian",
        "Aptian", "Albian", "Rupelian", "Chattian", "Aquitanian", "Burdigalian",
        "Langhian", "Serravallian", "Tortonian", "Messinian", "Zanclean",
        "Piacenzian", "Gelasian", "Calabrian", "Middle Pleistocene",
        "Upper Pleistocene", "Holocene"
      )
    )
  )
  # fmt: skip
  expect_named(
    out,
    c(
      "name", "max_ma", "min_ma", "ext", "orig", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # input checks
  expect_snapshot(tax_expand_time(taxdf, scale = "foo"), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, scale = character(0)), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, scale = NULL), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, scale = 1), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, scale = NA), error = TRUE)
})

test_that("arg 'rank' works", {
  bins <- data.frame(
    bin = 1:4,
    max_ma = c(160, 120, 80, 40),
    min_ma = c(120, 80, 40, 0)
  )

  taxdf <- data.frame(
    name = c("A", "B"),
    max_ma = c(150, 30),
    min_ma = c(110, 0)
  )

  # epoch -------------------------------
  epoch <- tax_expand_time(taxdf, rank = "epoch")
  # fmt: skip
  expect_equal(
    epoch[, c("name", "interval_name", "rank")],
    data.frame(
      name = rep(c("A", "B"), c(2L, 5L)),
      interval_name = c(
        "Upper Jurassic", "Lower Cretaceous", "Oligocene", "Miocene", "Pliocene",
        "Pleistocene", "Holocene"
      ),
      rank = "epoch"
    )
  )
  # fmt: skip
  expect_named(
    epoch,
    c(
      "name", "max_ma", "min_ma", "ext", "orig", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # period -------------------------------
  period <- tax_expand_time(taxdf, rank = "period")
  # fmt: skip
  expect_equal(
    period[, c("name", "interval_name", "rank")],
    data.frame(
      name = c("A", "A", "B", "B", "B"),
      interval_name = c("Jurassic", "Cretaceous", "Paleogene", "Neogene", "Quaternary"),
      rank = "period"
    )
  )
  # fmt: skip
  expect_named(
    period,
    c(
      "name", "max_ma", "min_ma", "ext", "orig", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # era -------------------------------
  era <- tax_expand_time(taxdf, rank = "era")
  expect_equal(
    era[, c("name", "interval_name", "rank")],
    data.frame(
      name = c("A", "B"),
      interval_name = c("Mesozoic", "Cenozoic"),
      rank = "era"
    )
  )
  # fmt: skip
  expect_named(
    era,
    c(
      "name", "max_ma", "min_ma", "ext", "orig", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # eon -------------------------------
  eon <- tax_expand_time(taxdf, rank = "eon")
  expect_equal(
    eon[, c("name", "interval_name", "rank")],
    data.frame(name = c("A", "B"), interval_name = "Phanerozoic", rank = "eon")
  )
  # fmt: skip
  expect_named(
    eon,
    c(
      "name", "max_ma", "min_ma", "ext", "orig", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # input checks
  expect_snapshot(
    tax_expand_time(taxdf, rank = c("eon", "period")),
    error = TRUE
  )
  expect_snapshot(tax_expand_time(taxdf, rank = "foo"), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, rank = character(0)), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, rank = NULL), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, rank = 1), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, rank = NA), error = TRUE)
})

test_that("arg 'ext_orig' works", {
  bins <- data.frame(
    bin = 1:4,
    max_ma = c(160, 120, 80, 40),
    min_ma = c(120, 80, 40, 0)
  )

  taxdf <- data.frame(
    name = c("A", "B"),
    max_ma = c(150, 30),
    min_ma = c(110, 0)
  )

  out <- tax_expand_time(taxdf, ext_orig = FALSE)

  # fmt: skip
  expect_named(
    out,
    c(
      "name", "max_ma", "min_ma", "bin", "interval_name", "rank",
      "max_ma", "mid_ma", "min_ma", "duration_myr", "abbr", "colour", "font"
    )
  )

  # input checks
  expect_snapshot(tax_expand_time(taxdf, ext_orig = "foo"), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, ext_orig = logical(0)), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, ext_orig = NULL), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, ext_orig = 1), error = TRUE)
  expect_snapshot(tax_expand_time(taxdf, ext_orig = NA), error = TRUE)
})
