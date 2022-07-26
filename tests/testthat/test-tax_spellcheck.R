
# test dataset
testdf <- data.frame(family = c("Examplidae", "Examplidae", "Taxonidae",
                                NA, "Taxonidae", "Examplidae",
                                "Examplidae", "Examplidae", "Taxonidae",
                                "Taxonidae"),
                     genus = c("Joebloggsia", "Joebloggia", "Facsimilus",
                               "Imperfectella", "Automaton", NA,
                               "Shangrilaria", "Shangirlaria", "Kunlungoides",
                               "Kunlungoides.sp"),
                     age = c(1:10))

# result
res1 <- data.frame(greater = c("Joebloggsia", "Kunlungoides", "Shangrilaria"),
                   lesser = c("Joebloggia", "Kunlungoides.sp", "Shangirlaria"),
                   group = c("J", "K", "S"))
res2 <- data.frame(greater = c("Joebloggsia", "Shangrilaria", "Kunlungoides"),
                   lesser = c("Joebloggia", "Shangirlaria", "Kunlungoides.sp"),
                   group = c("Examplidae", "Examplidae", "Taxonidae"))

# test errors from incorrectly supplied arguments, and warnings
test_that("tax_spellcheck() accepts taxon names, no groups", {
  # not data.frame or zero length data.frame supplied
  expect_error(tax_spellcheck(1))
  expect_error(tax_spellcheck(data.frame()))
  # name column not/incorrectly specified
  expect_error(tax_spellcheck(testdf))
  expect_error(tax_spellcheck(testdf, name = 1))
  expect_error(tax_spellcheck(testdf, name = c("one", "name")))
  expect_error(tax_spellcheck(testdf, name = "onename"))
  expect_error(tax_spellcheck(testdf, name = "age"))
  # groups column incorrectly specified
  expect_error(tax_spellcheck(testdf, "genus", group = TRUE))
  expect_error(tax_spellcheck(testdf, "genus", group = c("a", "group")))
  expect_error(tax_spellcheck(testdf, "genus", group = "agroup"))
  expect_error(tax_spellcheck(testdf, "genus", group = "age"))
  # Jaro distance out of range 0-1
  expect_error(tax_spellcheck(testdf, "genus", dissim = "max"))
  expect_error(tax_spellcheck(testdf, "genus", dissim = 0))
  expect_error(tax_spellcheck(testdf, "genus", dissim = 1))
  # start/end letter matches incorrectly supplied
  expect_error(tax_spellcheck(testdf, "genus", srt = "1"))
  expect_error(tax_spellcheck(testdf, "genus", srt = c(1, 3)))
  expect_error(tax_spellcheck(testdf, "genus", end = TRUE))
  expect_error(tax_spellcheck(testdf, "genus", end = c(1, 3)))
  # prefixes/suffixes not supplied as a character vector
  expect_error(tax_spellcheck(testdf, "genus", pref = c(TRUE, FALSE)))
  expect_error(tax_spellcheck(testdf, "genus", suff = c(1, 2)))
  expect_error(tax_spellcheck(testdf, "genus", suff = data.frame(1)))
  # verbosity not logical
  expect_error(tax_spellcheck(testdf, "genus", verbose = "TRUE"))
  # non alpha character warning
  expect_warning(tax_spellcheck(testdf, "genus", verbose = TRUE))
})

# scan with and without higher taxonomy, with prefixes/suffixes, with srt/end
test_that("tax_spellcheck() works with argument variations", {
  # without higher taxonomy
  expect_identical(tax_spellcheck(testdf, "genus"), res1)
  # with higher taxonomy
  expect_identical(tax_spellcheck(testdf, "genus", "family"), res2)
  # with suffixes (suff and pref get concatenated internally)
  expect_identical(tax_spellcheck(testdf, "genus", suff = "ia"), res1)
  # with starting character matches
  expect_identical(tax_spellcheck(testdf, "genus", srt = 3), res1)
  # return NULL due to all matches thresholded away
  expect_identical(tax_spellcheck(testdf, "genus", dissim = 0.01), NULL)

})













