test_that("look_up() works", {

  occdf <- tetrapods
  vec <- c(NA, " ", "")
  occdf <- occdf[which(!occdf$late_interval %in% vec), ]
  vec <- look_up(occdf = occdf, return_unassigned = TRUE)
  vec <- append(vec, c(NA, " ", ""))

  # load tetrapod data
  occdf <- occdf[which(!occdf$late_interval %in% vec), ]
  occdf <- occdf[which(!occdf$early_interval %in% vec), ]
  occdf$late_interval <- occdf$early_interval

  # check correct format and output
  expect_true(is.data.frame(look_up(occdf[1:10, ])))
  expect_true(is.character((look_up(occdf[1:10, ]))$early_stage))
  expect_true(is.numeric((look_up(occdf[1:10, ]))$interval_min_ma))

  # check correct amount of data returned
  expect_equal(nrow(look_up(occdf[1:10, ])), 10)
  expect_equal(ncol(look_up(occdf[1:10, ])), 37)
  expect_equal(nrow(look_up(occdf[1:10, ], assign_with_GTS = "GTS2012")), 10)
  expect_equal(ncol(look_up(occdf[1:10, ], assign_with_GTS = "GTS2012")), 37)

  # check behaviour without int_key
  expect_equal((look_up(occdf[1:10, ], int_key = FALSE,
                        assign_with_GTS = "GTS2012"))$early_stage[1:2],
               c("Induan", "Asselian"))

  # check that no error is produced when pre-Phanerozoic intervals are included
  occdf <- reefs
  occdf$interval[1] <- "Meso-archean"
  expect_warning(expect_equal(look_up(occdf[which(occdf$interval == "Ediacaran" |
                                      occdf$interval == "Meso-archean"),],
                        early_interval = "interval",
                        late_interval = "interval", assign_with_GTS = "GTS2012",
                        int_key = FALSE)$early_stage, rep(NA_character_,3)),
                 regexp = "Ediacaran")

  # check whether unassigned intervals are returned, if required
  occdf <- tetrapods
  vec <- c(NA, " ", "")
  occdf <- occdf[which(!occdf$late_interval %in% vec), ]
  expect_equal(look_up(occdf[1:100, ], int_key = palaeoverse::interval_key,
                       return_unassigned = TRUE), "Early Triassic")

  # turn off "GTS": stage ages are not returned as not given in interval_key
  occdf <- tetrapods
  occdf$late_interval <- occdf$early_interval
  occdf <- occdf[which(!occdf$early_interval %in% GTS2020$interval_name), ]
  expect_equal(ncol(look_up(occdf[10:20, ], int_key = palaeoverse::interval_key,
                            assign_with_GTS = FALSE)), 34)

  # test with own interval key - reset occdf
  # define own interval key
  custom_key <- data.frame(
    interval_name = c("Missourian", "Gzhelian", "Capitanian"),
    early_stage = c("Kasimovian", "Gzhelian", "Capitanian"),
    late_stage = c("Sakmarian", "Gzhelian", "Capitanian"),
    max_ma = c(400, 360, 300),
    min_ma = c(370, 350, 290))

  occdf <- occdf[which(occdf$early_interval %in% custom_key$interval_name), ]

  expect_equal(ncol(look_up(occdf[1:10, ], int_key = custom_key,
                            assign_with_GTS = FALSE)), 37)

  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE))$interval_max_ma[1], 400)
  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE))$interval_mid_ma[2], 385)
  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE))$late_stage[1], "Sakmarian")


  # check assignment just based on early_interval
  expect_equal((look_up(occdf[1:10, ], int_key = custom_key,
                        assign_with_GTS = FALSE, late_interval =
                          "early_interval"))$late_stage[1], "Sakmarian")

  # check whether different late_interval name works
  colnames(occdf)[which(colnames(occdf) == "late_interval")] <- "other_interval"
  expect_equal((look_up(
    occdf[1:10, ], int_key = custom_key, assign_with_GTS = FALSE,
    late_interval = "other_interval"))$late_stage[1], "Sakmarian")

  occdf <- tetrapods
  # check error handling
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

  interval_key$min_ma <- rep("test", nrow(interval_key))
  expect_error(look_up(occdf[1:10, ], int_key = interval_key))

  interval_key$max_ma <- rep("test", nrow(interval_key))
  expect_error(look_up(occdf[1:10, ], int_key = interval_key))

  interval_key$interval_name <- rep(1, nrow(interval_key))
  expect_error(look_up(occdf[1:10, ], int_key = interval_key))

  expect_error(look_up(occdf[1:10, ], early_interval = 5))

  expect_error(look_up(occdf[1:10, ], late_interval = 5))
})
