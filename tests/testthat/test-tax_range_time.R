test_that("basic behaviour works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  out <- tax_range_time(data)
  expect_equal(
    out,
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    ),
    ignore_attr = TRUE
  )
  expect_s3_class(out, "palaeoverse_tax_range_time")

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
    data |> tax_range_time(name = "genus"),
    tax_range_time(data, name = "genus")
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
    ),
    ignore_attr = TRUE
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
    ),
    ignore_attr = TRUE
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
    ),
    ignore_attr = TRUE
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
    ),
    ignore_attr = TRUE
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

test_that("argument 'sort' works", {
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
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    tax_range_time(data, sort = "LAD"),
    data.frame(
      taxon = c("C", "B", "A"),
      taxon_id = 1:3,
      max_ma = c(3, 6, 10),
      min_ma = c(2, 4, 7),
      range_myr = c(1, 2, 3),
      n_occ = c(1L, 2L, 2L)
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    tax_range_time(data, sort = "name"),
    data.frame(
      taxon = c("A", "B", "C"),
      taxon_id = 1:3,
      max_ma = c(10, 6, 3),
      min_ma = c(7, 4, 2),
      range_myr = c(3, 2, 1),
      n_occ = c(2L, 2L, 1L)
    ),
    ignore_attr = TRUE
  )

  # input checks
  expect_snapshot(
    tax_range_time(data, sort = c("genus", "min_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, sort = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, sort = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, sort = NA), error = TRUE)
})

test_that("argument 'by' is deprecated but still works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_warning(
    expect_equal(
      tax_range_time(data, by = "LAD"),
      tax_range_time(data, sort = "LAD")
    ),
    "deprecated as of palaeoverse 2.0.0"
  )

  # can't have both sort and by
  expect_snapshot(
    tax_range_time(data, by = "LAD", sort = "LAD"),
    error = TRUE
  )

  # still doing input checks
  expect_snapshot(
    tax_range_time(data, by = c("genus", "min_ma")),
    error = TRUE
  )
  expect_snapshot(tax_range_time(data, by = "nonexistent"), error = TRUE)
  expect_snapshot(tax_range_time(data, by = 1), error = TRUE)
  expect_snapshot(tax_range_time(data, by = NA), error = TRUE)
})

test_that("plotting works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2),
    family = c("F1", "F1", "F1", "F2", "F2")
  )

  expect_doppelganger("tax_range_time() works", function() {
    plot(tax_range_time(data))
  })
  expect_doppelganger("tax_range_time() works with LAD sorting", function() {
    plot(tax_range_time(data, by = "LAD"))
  })
  expect_doppelganger("tax_range_time() works with name sorting", function() {
    plot(tax_range_time(data, by = "name"))
  })
  expect_doppelganger("tax_range_time() works with group", function() {
    plot(tax_range_time(data, group = "family"))
  })
})

test_that("tax_range_time plotting with extra args works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2),
    family = c("F1", "F1", "F1", "F2", "F3")
  )
  expect_doppelganger("tax_range_time-many-args", function() {
    plot(
      tax_range_time(data),
      xlab = "x axis",
      ylab = "y axis",
      col = "red",
      bg = "grey",
      lty = 3,
      lwd = 5,
      cex = 6,
      pch = 21,
      axes = FALSE,
      yaxs = "r"
    )
  })

  # `cex.main` is passed through `...`
  expect_doppelganger("tax_range_time-dots", function() {
    plot(tax_range_time(data), cex.main = 4)
  })

  # `col.group` must be of the same length as the number of groups
  expect_doppelganger("tax_range_time-col-group", function() {
    plot(
      tax_range_time(data, group = "family"),
      col.group = c("red", "blue", "green")
    )
  })
  expect_snapshot(
    plot(tax_range_time(data, group = "family"), col.group = c("red", "blue")),
    error = TRUE
  )

  # forbidden args
  expect_snapshot(
    plot(
      tax_range_time(data),
      xlim = 1,
      ylim = 1,
      xaxt = 1,
      yaxt = 1,
      yaxs = 1
    ),
    error = TRUE
  )
})

test_that("plot resets par and returns the settings to add elements", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )
  expect_doppelganger("tax_range_time-add-elements", function() {
    old_mar <- par("mar")
    out <- plot(tax_range_time(data))
    expect_identical(par("mar"), old_mar)
    expect_named(out, c("mar", "usr"))
    op <- par(out)
    abline(v = 5, col = "red", lwd = 3)
    points(x = 7, y = 2, pch = 4, cex = 3)
    par(op)
    expect_identical(par("mar"), old_mar)
  })
})

test_that("plotting with custom axis_geo() call works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )
  expect_doppelganger("tax_range_time-custom-axis-geo", function() {
    plot(
      tax_range_time(data),
      axis = axis_geo(intervals = "epoch", fill = "red", abbr = FALSE)
    )
  })
  expect_doppelganger("tax_range_time-custom-axis", function() {
    plot(
      tax_range_time(data),
      axis = axis(1, lty = "dashed", lwd = 5, col = "red")
    )
  })
})

test_that("plot is deprecated but still works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2),
    family = c("F1", "F1", "F1", "F2", "F2")
  )
  expect_doppelganger("tax_range_time-plot-deprecated", function() {
    expect_warning(
      tax_range_time(data, plot = TRUE),
      "is deprecated as of palaeoverse 2.0.0",
      fixed = TRUE
    )
  })
  expect_doppelganger("tax_range_time()-plot-deprecated-group", function() {
    expect_warning(
      tax_range_time(data, group = "family", plot = TRUE),
      "is deprecated as of palaeoverse 2.0.0",
      fixed = TRUE
    )
  })

  # still input checking
  expect_snapshot(tax_range_time(data, plot = "6"), error = TRUE)
})

test_that("plot_args is deprecated but still works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )
  expect_doppelganger("tax_range_time_plot-args-deprecated", function() {
    expect_warning(
      expect_warning(
        tax_range_time(
          data,
          plot = TRUE,
          plot_args = list(xlab = "x axis", ylab = "y axis")
        ),
        "is deprecated as of palaeoverse 2.0.0",
        fixed = TRUE
      ),
      "is deprecated as of palaeoverse 2.0.0",
      fixed = TRUE
    )
  })

  # still input checking
  expect_snapshot(tax_range_time(data, plot_args = 1), error = TRUE)
})

test_that("argument 'intervals' is hard deprecated", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  expect_snapshot(
    tax_range_time(data, intervals = c("genus", "min_ma")),
    error = TRUE
  )
})

test_that("plot argument 'axis' works", {
  data <- data.frame(
    genus = c("A", "A", "B", "B", "C"),
    max_ma = c(10, 8, 6, 5, 3),
    min_ma = c(9, 7, 5, 4, 2)
  )

  # input checks
  expect_snapshot(
    plot(tax_range_time(data), axis = "periods"),
    error = TRUE
  )
  expect_snapshot(
    plot(
      tax_range_time(data),
      axis = axis_geo(intervals = "nonexistent")
    ),
    error = TRUE
  )
  expect_snapshot(
    plot(tax_range_time(data), axis = axis_geo(intervals = 1)),
    error = TRUE
  )
  expect_snapshot(
    plot(tax_range_time(data), axis = axis_geo(intervals = NA)),
    error = TRUE
  )
})
