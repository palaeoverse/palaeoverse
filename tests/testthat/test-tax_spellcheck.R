brachios <- read.csv(paste0("https://paleobiodb.org/data1.2/occs/list.csv",
                            "?base_name=Brachiopoda",
                            "&interval=Cambrian,Silurian",
                            "&show=class"))

# test errors from incorrectly supplied arguments
test_that("tax_spellcheck() accepts taxon names, no groups", {
  # no data.frame or zero length data.frame supplied
  expect_error(tax_spellcheck())
  expect_error(tax_spellcheck(data.frame()))
  # name column not/incorrectly specified
  expect_error(tax_spellcheck(brachios))
  expect_error(tax_spellcheck(brachios, name = 1))
  expect_error(tax_spellcheck(brachios, name = c("one", "name")))
  # groups column incorrectly specified
  expect_error(tax_spellcheck(brachios, "genus", group = TRUE))
  expect_error(tax_spellcheck(brachios, "genus", group = c("a", "group")))
  # Jaro distance out of range 0-1
  expect_error(tax_spellcheck(brachios, "genus", sim = 0))
  expect_error(tax_spellcheck(brachios, "genus", sim = 1))
  # start/end letter matches incorrectly supplied
  expect_error(tax_spellcheck(brachios, "genus", srt = "1"))
  expect_error(tax_spellcheck(brachios, "genus", end = TRUE))
  # prefixes/suffixes not supplied as a character vector
  expect_error(tax_spellcheck(brachios, "genus", pref = c(TRUE, FALSE)))
  expect_error(tax_spellcheck(brachios, "genus", end = c(1, 2)))
  # verbosity not logical
  expect_error(tax_spellcheck(brachios, "genus", verbose = "TRUE"))
})
