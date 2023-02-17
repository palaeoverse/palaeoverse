test_that("get_timescale_data works", {
  expect_named(get_timescale_data(rank = "stage"),
               c("interval_number", "interval_name", "rank", "max_ma", "mid_ma",
                 "min_ma", "duration_myr", "font", "colour"))
  expect_equal(get_timescale_data(rank = "stage"),
               subset(GTS2020, rank =="stage"))
  expect_equal(get_timescale_data(name = "GTS2012", rank = "stage"),
               subset(GTS2012, rank =="stage"))
  expect_equal(nrow(get_timescale_data(rank = "eon")), 4)
  expect_equal(get_timescale_data(rank = "stage"),
               get_timescale_data(rank = "s"))
  expect_named(
    get_timescale_data("North American Land Mammal Ages"),
    c("interval_name", "max_ma", "min_ma", "abbr", "colour", "mid_ma",
      "duration_myr", "font")
  )

  expect_error(get_timescale_data(name = "GTS"))
  expect_error(get_timescale_data(rank = "pickles"))
  expect_error(get_timescale_data(rank = "e"))
  expect_error(get_timescale_data("international house of pancakes"))
})
