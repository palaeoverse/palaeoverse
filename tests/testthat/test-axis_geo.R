test_that("axis_geo() works", {
  expect_doppelganger("axis_geo()", function() {
    plot(x = reef_df$interval_mid_ma, y = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         xlim = c(542, 0), xlab = NA, ylab = "Paleolatitude")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = "periods")
    title(xlab = "Time (Ma)", line = 4)
  })
})

test_that("axis_geo() works with title", {
  expect_doppelganger("axis_geo() with title", function() {
    plot(x = reef_df$interval_mid_ma, y = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         xlim = c(542, 0), xlab = NA, ylab = "Paleolatitude")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = "periods", title = "Time (Ma)")
  })
})

test_that("axis_geo() works with negative axis", {
  expect_doppelganger("axis_geo() with negative axis", function() {
    plot(x = -reef_df$interval_mid_ma, y = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         xlim = c(-542, 0), xlab = NA, ylab = "Paleolatitude")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = "periods", neg = TRUE, title = "Time (Ma)")
  })
})

test_that("axis_geo() works with autofit", {
  expect_doppelganger("axis_geo() with autofit", function() {
    plot(x = reef_df$interval_mid_ma, y = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         xlim = c(542, 0), xlab = NA, ylab = "Paleolatitude")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = "periods", lab_size = 4, autofit = TRUE)
    title(xlab = "Time (Ma)", line = 4)
  })
  expect_doppelganger("axis_geo() with autofit2", function() {
    par(mar = c(5.1, 6.1, 4.1, 2.1))
    plot(y = reef_df$interval_mid_ma, x = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         ylim = c(542, 0), ylab = NA, xlab = "Paleolatitude")
    box()

    axis(side = 1)
    axis_geo(side = 2, intervals = "periods", lab_size = 4, autofit = TRUE)
    title(ylab = "Time (Ma)", line = 4.5)
  })
})

test_that("axis_geo() works with time_bins()", {
  expect_doppelganger("axis_geo() with time_bins() scale", function() {
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    plot(x = reef_df$interval_mid_ma, y = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         xlim = c(542, 0), xlab = NA, ylab = "Paleolatitude")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = time_bins(rank = "period",
                                             scale = "GTS2020"))
    title(xlab = "Time (Ma)", line = 4)
  })
})

test_that("axis_geo() works with multiple scales", {
  expect_doppelganger("axis_geo() with multiple scales", function() {
    par(mar = c(7.6, 4.1, 4.1, 2.1))
    plot(x = reef_df$interval_mid_ma, y = reef_df$lat,
         axes = FALSE, type = "p", pch = 20,
         xlim = c(542, 0), xlab = NA, ylab = "Paleolatitude")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = list("stages", "periods"),
             tick_at = seq(0, 500, 50), lab = list(FALSE, TRUE),
             abbr = FALSE)
    title(xlab = "Time (Ma)", line = 6)
  })
})

test_that("axis_geo() can be used on multiple sides", {
  periods_sub <- subset(periods, select = -c(font, colour))
  expect_doppelganger("axis_geo() on multiple sides", function() {
    par(mar = c(7, 7, 7, 7))
    plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(95, 0),
         xlab = NA, ylab = NA)
    box()

    axis_geo(side = 1, intervals = list("epochs", "periods"),
             height = list(.05, .03), tick_at = seq(0, 100, 25),
             title = "Time (Ma)")
    axis_geo(side = 2, height = list(.03, .05),
             intervals = list("epoch", "period"), bord_col = "purple",
             center_end_labels = list(FALSE, TRUE), exact = TRUE,
             title = "Time (Ma)")
    axis_geo(side = 3, height = list(.03, .05),
             intervals = list(epochs, periods_sub), abbr = FALSE,
             skip = c("Paleogene", "Holocene", "Pleistocene", "Pliocene",
                      "Quaternary"), lab_col = list("blue", NULL),
             title = "Time (Ma)")
    axis_geo(side = 4, height = list(.04, .03),
             intervals = list("epoch", "North American land mammal ages"),
             fill = list("lightblue", "yellow"),
             lty = list("solid", "dashed"), exact = TRUE, round = 1,
             title = "Time (Ma)")
  })
})

test_that("axis_geo() works with phylogenies", {
  skip_if_not_installed("phytools")
  library(phytools)
  data(mammal.tree)
  expect_doppelganger("axis_geo() with ultrametric tree", function() {
    plot(mammal.tree)
    axis_geo(intervals = "epoch", phylo = TRUE)
  })
  expect_doppelganger("axis_geo() with backwards ultrametric tree", function() {
    plot(mammal.tree, direction = "l")
    axis_geo_phylo(intervals = "epoch", exact = TRUE)
  })
  skip_if_not_installed("paleotree")
  library(paleotree)
  data(RaiaCopesRule)
  expect_doppelganger("axis_geo() with fossil tree", function() {
    plot(ceratopsianTreeRaia)
    axis_geo_phylo(intervals = "epoch", tick_at = seq(80, 200, 10))
  })
  expect_doppelganger("axis_geo() with downwards fossil tree", function() {
    plot(ceratopsianTreeRaia, direction = "d")
    axis_geo(side = 2, intervals = "epoch", phylo = TRUE,
             tick_at = seq(80, 200, 10), tick_labels = seq(-80, -200, -10))
  })
})

test_that("axis_geo() error handling", {
  expect_error(axis_geo(height = c(0.5, 0.5)))
  expect_error(axis_geo(fill = 5))
  expect_error(axis_geo(lab = "true"))
  expect_error(axis_geo(lab_color = 42))
  expect_error(axis_geo(lab_size = "big"))
  expect_error(axis_geo(rot = NULL))
  expect_error(axis_geo(abbr = c("true", 1)))
  expect_error(axis_geo(skip = c(1, 2, 3)))
  expect_error(axis_geo(center_end_labels = c(FALSE, TRUE)))
  expect_error(axis_geo(autofit = c(FALSE, TRUE)))
  expect_error(axis_geo(bord_color = TRUE))
  expect_error(axis_geo(lty = 7))
  expect_error(axis_geo(lwd = "thin"))
  expect_error(axis_geo(side = 5))
})
