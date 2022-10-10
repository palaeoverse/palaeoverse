test_that("look_up() works", {

  # load tetrapod data
  occdf <- tetrapods

  # check correct format and output
  expect_true(is.data.frame(look_up(occdf[1:10, ])))
  expect_true(is.character((look_up(occdf[1:10, ]))$early_stage))
  expect_true(is.numeric((look_up(occdf[1:10, ]))$interval_min_ma))

  # check correct amount of data returned
  expect_equal(nrow(look_up(occdf[1:10, ])), 10)
  expect_equal(ncol(look_up(occdf[1:10, ])), 37)
  expect_equal(nrow(look_up(occdf[1:10, ], assign_with_GTS = "GTS2012")), 10)
  expect_equal(ncol(look_up(occdf[1:10, ], assign_with_GTS = "GTS2012")), 37)

  # check whether unassigned intervals are returned, if required
  expect_equal(look_up(occdf[1:100, ], int_key = palaeoverse::interval_key,
                       return_unassigned = TRUE), "Early Triassic")

  # turn off "GTS": stage ages are not returned as not given in interval_key
  expect_equal(ncol(look_up(occdf[1:10, ], int_key = palaeoverse::interval_key,
                            assign_with_GTS = FALSE)), 34)

  # test with own interval key - reset occdf
  occdf <- tetrapods[1:10, ]
  # define own interval key
  custom_key <- data.frame(
    interval_name = c("Missourian", "Gzhelian", "Capitanian"),
    early_stage = c("Kasimovian", "Gzhelian", "Capitanian"),
    late_stage = c("Sakmarian", "Gzhelian", "Capitanian"),
    max_ma = c(400, 360, 300),
    min_ma = c(370, 350, 290))

  expect_equal(ncol(look_up(occdf[1:10, ], int_key = custom_key,
                            assign_with_GTS = FALSE)), 37)

  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE))$interval_max_ma[1], 400)
  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE))$interval_mid_ma[2], 295)
  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE))$late_stage[1], "Gzhelian")

  # check assignment just based on early_interval
  occdf <- tetrapods[1:10, ]
  occdf <- occdf[, -which(colnames(occdf) == "late_interval")]
  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE, late_interval =
                          "early_interval"))$late_stage[1], "Sakmarian")

  # check whether different early_interval name works
  colnames(occdf)[which(colnames(occdf) == "early_interval")] <- "some_interval"
  expect_equal((look_up(
    occdf[1:10, ], int_key = custom_key, assign_with_GTS = FALSE,
    early_interval = "some_interval", late_interval = "some_interval")
    )$late_stage[1], "Sakmarian")

  # check whether different late_interval name works
  occdf <- tetrapods[1:10, ]
  colnames(occdf)[which(colnames(occdf) == "late_interval")] <- "other_interval"
  expect_equal((look_up(
    occdf[1:10, ], int_key = custom_key, assign_with_GTS = FALSE,
    late_interval = "other_interval"))$late_stage[1], "Gzhelian")

  # check behaviour without int_key
  occdf <- tetrapods[1:10, ]
  expect_equal((look_up(occdf[1:10, ], int_key = FALSE,
                        assign_with_GTS = "GTS2012"))$early_stage[1:2],
               c(NA, "Capitanian"))

  # check error handling
  occdf <- tetrapods[1:10, ]
  expect_error(look_up("Sakmarian"))
  expect_error(look_up(occdf[names(occdf) != "early_interval"]))
  expect_error(look_up(occdf, early_interval = "test"))
  expect_error(look_up(occdf, late_interval = "test"))
  expect_error(look_up(occdf, int_key = "Sakmarian"))
  expect_error(look_up(occdf, int_key = custom_key[names(custom_key) !=
                                                     "interval_name"]))
  expect_error(look_up(occdf, int_key = FALSE, assign_with_GTS = FALSE))

  interval_key <- palaeoverse::interval_key

  interval_key$max_ma <- rep(1, nrow(interval_key))
  expect_warning(look_up(occdf[1:10, ], int_key = interval_key))

  interval_key$interval_name <- rep(1, nrow(interval_key))
  expect_error(look_up(occdf[1:10, ], int_key = interval_key))

  interval_key$min_ma <- rep("test", nrow(interval_key))
  expect_error(look_up(occdf[1:10, ], int_key = interval_key))

  interval_key$max_ma <- rep("test", nrow(interval_key))
  expect_error(look_up(occdf[1:10, ], int_key = interval_key))

  expect_error(look_up(occdf[1:10, ], early_interval = 5))

  expect_error(look_up(occdf[1:10, ], late_interval = 5))
})
