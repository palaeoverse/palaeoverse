test_that("basic behaviour works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(occdf = occdf),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    )
  )

  # input checks
  expect_snapshot(tax_range_time(occdf = data.frame()), error = TRUE)
  expect_snapshot(tax_range_time(occdf = NULL), error = TRUE)
  expect_snapshot(tax_range_time(occdf = NA), error = TRUE)
  expect_snapshot(tax_range_time(occdf = "a"), error = TRUE)
})

test_that("tax_range_time errors with unnamed args", {
  occdf <- data.frame(
    genus = c("A", "A", "B"),
    max_ma = c(10, 8, 6),
    min_ma = c(9, 7, 5)
  )
  expect_snapshot(tax_range_time(occdf, "genus"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, "genus"), error = TRUE)
  expect_snapshot(tax_range_time(occdf, "genus", "min_ma"), error = TRUE)
  expect_snapshot(
    tax_range_time(occdf, "genus", min_ma = "min_ma"),
    error = TRUE
  )
})

test_that("argument 'name' works", {
  occdf <- data.frame(
    species = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(occdf = occdf, name = "species"),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    )
  )

  # the "name" column must not contain NA values
  nadf <- occdf
  nadf$species[1] <- NA
  expect_snapshot(
    tax_range_time(occdf = nadf, name = "species"),
    error = TRUE
  )

  # input checks
  expect_snapshot(
    tax_range_time(occdf = occdf, name = c("Species", "max_ma")),
    error = TRUE
  )
  expect_snapshot(
    tax_range_time(occdf = occdf, name = "nonexistent"),
    error = TRUE
  )
  expect_snapshot(tax_range_time(occdf = occdf, name = 1), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, name = NA), error = TRUE)

  # Snapshot is slightly different with R < 4.3
  skip_if(getRversion() < "4.3.0")
  expect_snapshot(tax_range_time(occdf = occdf, name = NULL), error = TRUE)
})

test_that("argument 'max_ma' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    p_max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(occdf = occdf, max_ma = "p_max_ma"),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_time(occdf = occdf, max_ma = c("Species", "max_ma")),
    error = TRUE
  )
  expect_snapshot(
    tax_range_time(occdf = occdf, max_ma = "nonexistent"),
    error = TRUE
  )
  expect_snapshot(tax_range_time(occdf = occdf, max_ma = 1), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, max_ma = NA), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, max_ma = NULL), error = TRUE)

  # the "max_ma" column must be numeric
  chardf <- data.frame(genus = "a", max_ma = "10", min_ma = 5)
  expect_snapshot(
    tax_range_time(occdf = chardf),
    error = TRUE
  )
  # the "max_ma" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), max_ma = c(10, NA), min_ma = 5)
  expect_snapshot(
    tax_range_time(occdf = nadf),
    error = TRUE
  )
})

test_that("argument 'min_ma' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    p_min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(occdf = occdf, min_ma = "p_min_ma"),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_time(occdf = occdf, min_ma = c("Species", "min_ma")),
    error = TRUE
  )
  expect_snapshot(
    tax_range_time(occdf = occdf, min_ma = "nonexistent"),
    error = TRUE
  )
  expect_snapshot(tax_range_time(occdf = occdf, min_ma = 1), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, min_ma = NA), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, min_ma = NULL), error = TRUE)

  # the "min_ma" column must be numeric
  chardf <- data.frame(genus = "a", max_ma = 10, min_ma = "5")
  expect_snapshot(
    tax_range_time(occdf = chardf),
    error = TRUE
  )
  # the "min_ma" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), max_ma = 10, min_ma = c(5, NA))
  expect_snapshot(
    tax_range_time(occdf = nadf),
    error = TRUE
  )
})

test_that("argument 'group' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2),
    family = c("F1", "F1", "F1", "F2", "F2")
  )

  expect_equal(
    tax_range_time(occdf = occdf, group = "family"),
    data.frame(
      taxon = c("B", "A", "C", "B"),
      taxon_id = 1:4,
      max_ma = c(6, 10, 3, 5),
      min_ma = c(5, 7, 2, 4),
      range_myr = c(1, 3, 1, 1),
      n_occ = c(1L, 2L, 1L, 1L),
      family = c("F1", "F1", "F2", "F2")
    )
  )

  # input checks
  expect_snapshot(
    tax_range_time(occdf = occdf, group = c("genus", "min_ma")),
    error = TRUE
  )
  expect_snapshot(
    tax_range_time(occdf = occdf, group = "nonexistent"),
    error = TRUE
  )
  expect_snapshot(tax_range_time(occdf = occdf, group = 1), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, group = NA), error = TRUE)
})

test_that("argument 'by' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(occdf = occdf),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    )
  )
  expect_equal(
    tax_range_time(occdf = occdf, by = "LAD"),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    )
  )
  expect_equal(
    tax_range_time(occdf = occdf, by = "name"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_ma = c(10, 6, 3),
      min_ma = c(7, 4, 2),
      range_myr = c(3, 2, 1),
      n_occ = c(2L, 2L, 1L)
    )
  )

  # input checks
  expect_snapshot(
    tax_range_time(occdf = occdf, by = c("genus", "min_ma")),
    error = TRUE
  )
  expect_snapshot(
    tax_range_time(occdf = occdf, by = "nonexistent"),
    error = TRUE
  )
  expect_snapshot(tax_range_time(occdf = occdf, by = 1), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, by = NA), error = TRUE)
})

test_that("argument 'plot' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # The returned data.frame is identical whether or not a plot is produced
  expect_equal(
    tax_range_time(occdf = occdf, plot = TRUE),
    tax_range_time(occdf = occdf, plot = FALSE)
  )

  expect_doppelganger("tax_range_time() works", function() {
    tax_range_time(occdf = occdf)
  })
  expect_doppelganger("tax_range_time() works with LAD sorting", function() {
    tax_range_time(occdf = occdf, by = "LAD")
  })
  expect_doppelganger("tax_range_time() works with name sorting", function() {
    tax_range_time(occdf = occdf, by = "name")
  })

  # input checks
  expect_snapshot(tax_range_time(occdf = occdf, plot = "test"), error = TRUE)
  expect_snapshot(tax_range_time(occdf = occdf, plot = NA), error = TRUE)
})

test_that("argument 'plot_args' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # Passing plot_args does not change the returned data.frame
  expect_equal(
    tax_range_time(occdf = occdf, plot = TRUE, plot_args = list(ylab = "Taxa")),
    tax_range_time(occdf = occdf, plot = FALSE)
  )

  expect_doppelganger("tax_range_time() works with plot args", function() {
    tax_range_time(occdf = occdf, plot_args = list(ylab = "Taxa"))
  })

  # input checks
  expect_snapshot(
    tax_range_time(occdf = occdf, plot_args = "test"),
    error = TRUE
  )
  expect_snapshot(tax_range_time(occdf = occdf, plot_args = NA), error = TRUE)
})

test_that("argument 'intervals' works", {
  occdf <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # Passing intervals does not change the returned data.frame
  expect_equal(
    tax_range_time(occdf = occdf, plot = TRUE, intervals = "epochs"),
    tax_range_time(occdf = occdf, plot = FALSE)
  )

  # input checks

  # TODO: currently these do not work because they produce the plot and then throw the error.
  # The fact that they still create a plot doesn't play nicely with expect_snapshot().
  # The validation of "intervals" should come earlier in the function, before creating the plot.

  # expect_snapshot(
  #   tax_range_time(occdf = occdf, plot = TRUE, intervals = c("genus", "min_ma")),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(occdf = occdf, plot = TRUE, intervals = "nonexistent"),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(occdf = occdf, plot = TRUE, intervals = 1),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(occdf = occdf, plot = TRUE, intervals = NA),
  #   error = TRUE
  # )

  # TODO: should these error if plot = FALSE since intervals would be irrelevant in this case?
  # expect_snapshot(
  #   tax_range_time(occdf = occdf, intervals = c("genus", "min_ma")),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(occdf = occdf, intervals = "nonexistent"),
  #   error = TRUE
  # )
  # expect_snapshot(tax_range_time(occdf = occdf, intervals = 1), error = TRUE)
  # expect_snapshot(tax_range_time(occdf = occdf, intervals = NA), error = TRUE)
})
