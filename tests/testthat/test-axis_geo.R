test_that("axis_geo() works", {
  expect_doppelganger("axis_geo()", function() {
    plot(x = coral_div$stage_age, y = coral_div$n, axes = FALSE, xlim = c(250, 0),
         xlab = NA, ylab = "Diversity")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = "periods")
  })
})

test_that("axis_geo() works with multiple scales", {
  expect_doppelganger("axis_geo() with multiple scales", function() {
    par(mar = c(6.6, 4.1, 4.1, 2.1))
    plot(x = coral_div$stage_age, y = coral_div$n, axes = FALSE, xlim = c(250, 0),
         xlab = NA, ylab = "Diversity")
    box()

    axis(side = 2)
    axis_geo(side = 1, intervals = list("stages", "periods"),
             at = seq(0, 250, 25), lab = list(FALSE, TRUE),
             abbrv = FALSE)
  })
})

test_that("axis_geo() can be used on multiple sides", {
  expect_doppelganger("axis_geo() on multiple sides", function() {
    par(mar = c(7, 7, 7, 7))
    plot(0:100, axes = FALSE, xlim = c(100, 0), ylim = c(95, 0), xlab = NA, ylab = NA)
    box()

    axis_geo(side = 1, intervals = list("epochs", "periods"), height = list(.05,.03), at = seq(0, 100, 25))
    axis_geo(side = 2, height = list(.03, .05), intervals = list("epochs", "periods"), bord_color = "purple",
             center_end_labels = list(FALSE, TRUE), exact = TRUE)
    axis_geo(side = 3, height = list(.03, .05), intervals = list("epochs", "periods"),
             abbrv = FALSE, skip = c("Paleogene", "Holocene", "Pleistocene", "Pliocene", "Quaternary"))
    axis_geo(side = 4, height = list(.04, .03), intervals = list("epochs", "periods"),
             lty = list("solid", "dashed"), exact = TRUE, round = 1)
  })
})
