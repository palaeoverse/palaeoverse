testdf <- data.frame(family = c("Examplidae", "Examplidae", "Taxonidae",
                                NA, "Taxonidae", "Examplidae", "Examplidae",
                                "Examplidae", "Examplidae", "Taxonidae",
                                "Taxonidae"),
                     genus = c("Joebloggsia", "Joebloggia", "Facsimilus",
                               "Imperfectella", "Automaton", NA, "Joebloggsia",
                               "Shangrilaria", "Shangirlaria", "Kunlungoides",
                               "Kunlungoides.sp"),
                     age = c(1:11))

# test errors from incorrectly supplied arguments, and warnings
test_that("tax_check() accepts taxon names, no groups", {
  # not data.frame or zero length data.frame supplied
  expect_error(tax_check())
  expect_error(tax_check(1))
  expect_error(tax_check(data.frame()))
  # name column not/incorrectly specified
  expect_error(tax_check(testdf, name = 1))
  expect_error(tax_check(testdf, name = c("one", "name")))
  expect_error(tax_check(testdf, name = "age"))
  # groups column incorrectly specified
  expect_error(tax_check(testdf, "genus", group = TRUE))
  expect_error(tax_check(testdf, "genus", group = c("a", "group")))
  expect_error(tax_check(testdf, "genus", group = "agroup"))
  expect_error(tax_check(testdf, "genus", group = "age"))
  # Jaro distance out of range 0-1
  expect_error(tax_check(testdf, "genus", dis = "max"))
  expect_error(tax_check(testdf, "genus", dis = 0))
  expect_error(tax_check(testdf, "genus", dis = FALSE))
   # start/end letter matches incorrectly supplied
  expect_error(tax_check(testdf, "genus", start = "1"))
  expect_error(tax_check(testdf, "genus", start = c(1, 3)))
  expect_error(tax_check(testdf, "genus", start = TRUE))
  # verbosity not logical
  expect_error(tax_check(testdf, "genus", verbose = "TRUE"))
  # non alpha character warning
  expect_warning(tax_check(testdf, "genus", verbose = TRUE))
})
