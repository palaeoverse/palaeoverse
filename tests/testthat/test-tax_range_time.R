test_that("basic behaviour works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(data),
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
  expect_snapshot(tax_range_time(data = data.frame()), error = TRUE)
  expect_snapshot(tax_range_time(data = NULL), error = TRUE)
  expect_snapshot(tax_range_time(data = NA), error = TRUE)
  expect_snapshot(tax_range_time(data = "a"), error = TRUE)
})

test_that("piping and not piping the first argument give the same result", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    data |> tax_range_time(name = "genus", plot = FALSE),
    tax_range_time(data, name = "genus", plot = FALSE)
  )
})

test_that("tax_range_time errors with unnamed args", {
  data <- data.frame(
    genus = c("A", "A", "B"),
    max_ma = c(10, 8, 6),
    min_ma = c(9, 7, 5)
  )
  expect_snapshot(tax_range_time(data, "genus"), error = TRUE)
  expect_snapshot(tax_range_time(data, "genus", "min_ma"), error = TRUE)
  expect_snapshot(
    tax_range_time(data, "genus", min_ma = "min_ma"),
    error = TRUE
  )
})

test_that("argument 'name' works", {
  data <- data.frame(
    species = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(data, name = "species"),
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
  nadf <- data
  nadf$species[1] <- NA
  expect_snapshot(
    tax_range_time(nadf, name = "species"),
    error = TRUE
  )

  # input checks
  expect_snapshot(
    tax_range_time(data, name = c("Species", "max_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, name = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, name = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, name = NA), error = TRUE)

  # Snapshot is slightly different with R < 4.3
  skip_if(getRversion() < "4.3.0")
  expect_snapshot(tax_range_time(data, name = NULL), error = TRUE)
})

test_that("argument 'max_ma' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    p_max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(data, max_ma = "p_max_ma"),
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
    tax_range_time(data, max_ma = c("Species", "max_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, max_ma = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, max_ma = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, max_ma = NA), error = TRUE)
  expect_snapshot(tax_range_time(data, max_ma = NULL), error = TRUE)

  # the "max_ma" column must be numeric
  chardf <- data.frame(genus = "a", max_ma = "10", min_ma = 5)
  expect_snapshot(
    tax_range_time(chardf),
    error = TRUE
  )
  # the "max_ma" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), max_ma = c(10, NA), min_ma = 5)
  expect_snapshot(
    tax_range_time(nadf),
    error = TRUE
  )
})

test_that("argument 'min_ma' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    p_min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(data, min_ma = "p_min_ma"),
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
    tax_range_time(data, min_ma = c("Species", "min_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, min_ma = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, min_ma = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, min_ma = NA), error = TRUE)
  expect_snapshot(tax_range_time(data, min_ma = NULL), error = TRUE)

  # the "min_ma" column must be numeric
  chardf <- data.frame(genus = "a", max_ma = 10, min_ma = "5")
  expect_snapshot(
    tax_range_time(chardf),
    error = TRUE
  )
  # the "min_ma" column must not contain NA values
  nadf <- data.frame(genus = c("a", "b"), max_ma = 10, min_ma = c(5, NA))
  expect_snapshot(
    tax_range_time(nadf),
    error = TRUE
  )
})

test_that("max ages must be larger than or equal to min ages", {
  data <- data.frame(
    genus = c("A", "B", "C"),
    max_ma = c(150, 100, 30),
    min_ma = c(110, 110, 40)
  )
  expect_snapshot(tax_range_time(data), error = TRUE)
})

test_that("argument 'group' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2),
    family = c("F1", "F1", "F1", "F2", "F2")
  )

  expect_equal(
    tax_range_time(data, group = "family"),
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
    tax_range_time(data, group = c("genus", "min_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, group = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, group = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, group = NA), error = TRUE)
})

test_that("argument 'by' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_equal(
    tax_range_time(data),
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
    tax_range_time(data, by = "LAD"),
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
    tax_range_time(data, by = "name"),
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
    tax_range_time(data, by = c("genus", "min_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, by = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, by = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, by = NA), error = TRUE)
})

test_that("argument 'plot' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # The returned data.frame is identical whether or not a plot is produced
  expect_equal(
    tax_range_time(data, plot = TRUE),
    tax_range_time(data, plot = FALSE)
  )

  expect_doppelganger("tax_range_time() works", function() {
    tax_range_time(data)
  })
  expect_doppelganger("tax_range_time() works with LAD sorting", function() {
    tax_range_time(data, by = "LAD")
  })
  expect_doppelganger("tax_range_time() works with name sorting", function() {
    tax_range_time(data, by = "name")
  })

  # input checks
  expect_snapshot(tax_range_time(data, plot = "test"), error = TRUE)
  expect_snapshot(tax_range_time(data, plot = NA), error = TRUE)
})

test_that("argument 'plot_args' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # Passing plot_args does not change the returned data.frame
  expect_equal(
    tax_range_time(data, plot = TRUE, plot_args = list(ylab = "Taxa")),
    tax_range_time(data, plot = FALSE)
  )

  expect_doppelganger("tax_range_time() works with plot args", function() {
    tax_range_time(data, plot_args = list(ylab = "Taxa"))
  })

  # input checks
  expect_snapshot(tax_range_time(data, plot_args = "test"), error = TRUE)
  expect_snapshot(tax_range_time(data, plot_args = NA), error = TRUE)
})

test_that("argument 'intervals' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # Passing intervals does not change the returned data.frame
  expect_equal(
    tax_range_time(data, plot = TRUE, intervals = "epochs"),
    tax_range_time(data, plot = FALSE)
  )

  # input checks

  # TODO: currently these do not work because they produce the plot and then throw the error.
  # The fact that they still create a plot doesn't play nicely with expect_snapshot().
  # The validation of "intervals" should come earlier in the function, before creating the plot.

  # expect_snapshot(
  #   tax_range_time(data, plot = TRUE, intervals = c("genus", "min_ma")),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(data, plot = TRUE, intervals = "nonexistent"),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(data, plot = TRUE, intervals = 1),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(data, plot = TRUE, intervals = NA),
  #   error = TRUE
  # )

  # TODO: should these error if plot = FALSE since intervals would be irrelevant in this case?
  # expect_snapshot(
  #   tax_range_time(data, intervals = c("genus", "min_ma")),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_range_time(data, intervals = "nonexistent"),
  #   error = TRUE
  # )
  # expect_snapshot(tax_range_time(data, intervals = 1), error = TRUE)
  # expect_snapshot(tax_range_time(data, intervals = NA), error = TRUE)
})
