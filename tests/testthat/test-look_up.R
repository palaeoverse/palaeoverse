test_that("basic behavior works", {
  # drop missings
  occdf <- tetrapods[which(!tetrapods$late_interval %in% c(NA, " ", "")), ]
  occdf <- occdf[2:6, ]

  out <- look_up(occdf = occdf)
  expect_named(
    out,
    c(
      names(occdf),
      c(
        "early_stage",
        "late_stage",
        "interval_max_ma",
        "interval_mid_ma",
        "interval_min_ma"
      )
    )
  )
  expect_equal(nrow(out), nrow(occdf))
  expect_snapshot(
    out[, c(
      "early_interval",
      "late_interval",
      "early_stage",
      "late_stage",
      "interval_max_ma",
      "interval_mid_ma",
      "interval_min_ma"
    )]
  )
})

test_that("wrong input for argument 'occdf'", {
  expect_snapshot(look_up(1), error = TRUE)
  expect_snapshot(look_up(NA), error = TRUE)
  expect_snapshot(look_up(NULL), error = TRUE)
})

test_that("look_up() warns if some intervals couldn't be matched", {
  # drop missings
  occdf <- tetrapods[which(!tetrapods$late_interval %in% c(NA, " ", "")), ]
  occdf <- occdf[1:2, ]
  expect_warning(
    out <- look_up(occdf = occdf),
    "The following intervals could not be matched with intervals from int_key"
  )
  expect_snapshot(
    out[, c(
      "early_interval",
      "late_interval",
      "early_stage",
      "late_stage",
      "interval_max_ma",
      "interval_mid_ma",
      "interval_min_ma"
    )]
  )
})

test_that("arguments 'early_interval' and 'late_interval' work", {
  # drop missings and rename interval columns
  occdf <- tetrapods[which(!tetrapods$late_interval %in% c(NA, " ", "")), ]
  occdf <- occdf[2:3, c("early_interval", "late_interval")]
  names(occdf) <- c("early", "late")

  # either one or both interval colums are not found in the data
  expect_snapshot(look_up(occdf = occdf), error = TRUE)
  expect_snapshot(
    look_up(occdf = occdf, early_interval = "early"),
    error = TRUE
  )
  expect_snapshot(look_up(occdf = occdf, late_interval = "late"), error = TRUE)

  # wrong input type
  expect_snapshot(look_up(occdf = occdf, early_interval = 1), error = TRUE)
  expect_snapshot(look_up(occdf = occdf, early_interval = NA), error = TRUE)
  expect_snapshot(
    look_up(occdf = occdf, early_interval = c("a", "b")),
    error = TRUE
  )
  expect_snapshot(look_up(occdf = occdf, late_interval = 1), error = TRUE)
  expect_snapshot(look_up(occdf = occdf, late_interval = NA), error = TRUE)
  expect_snapshot(
    look_up(occdf = occdf, late_interval = c("a", "b")),
    error = TRUE
  )

  expect_equal(
    look_up(
      occdf = occdf,
      early_interval = "early",
      late_interval = "late"
    ),
    data.frame(
      early = c("Induan", "Asselian"),
      late = c("Olenekian", "Sakmarian"),
      early_stage = c("Induan", "Asselian"),
      late_stage = c("Olenekian", "Sakmarian"),
      interval_max_ma = c(251.902, 298.9),
      interval_mid_ma = c(249.551, 294.5),
      interval_min_ma = c(247.2, 290.1),
      row.names = c(3L, 19L)
    )
  )
})

test_that("argument 'int_key' works", {
  # drop missings
  occdf <- tetrapods[which(!tetrapods$late_interval %in% c(NA, " ", "")), ]
  occdf <- occdf[2:6, c("early_interval", "late_interval")]

  my_interval <- data.frame(
    interval_name = c("Induan", "Asselian"),
    early_stage = c("foo1", "foo2"),
    late_stage = c("bar1", "bar2")
  )

  # TODO: I really don't know what to expect here
  expect_snapshot(look_up(occdf, int_key = my_interval))

  # wrong format for int_key
  expect_snapshot(
    look_up(
      occdf,
      int_key = data.frame(
        interval_name = c("Induan", "Asselian"),
        early_stage = c("foo1", "foo2")
      )
    ),
    error = TRUE
  )
  expect_snapshot(
    look_up(
      occdf,
      int_key = data.frame(
        interval_name = c("Induan", "Asselian"),
        late_stage = c("foo1", "foo2")
      )
    ),
    error = TRUE
  )
  expect_snapshot(
    look_up(occdf, int_key = 1),
    error = TRUE
  )
  expect_snapshot(
    look_up(occdf, int_key = c("a", "b")),
    error = TRUE
  )
})

test_that("argument 'assign_with_GTS' works", {
  # drop missings
  occdf <- tetrapods[which(!tetrapods$late_interval %in% c(NA, " ", "")), ]
  occdf <- occdf[2:6, c("early_interval", "late_interval")]

  expect_snapshot(look_up(occdf, assign_with_GTS = "GTS2012"))

  occdf <- tetrapods
  occdf$late_interval <- occdf$early_interval
  occdf <- occdf[which(!occdf$early_interval %in% GTS2020$interval_name), ]
  occdf <- occdf[10:20, ]
  out <- look_up(occdf, int_key = interval_key, assign_with_GTS = FALSE)
  expect_named(
    out,
    c(names(occdf), c("early_stage", "late_stage"))
  )
  expect_equal(nrow(out), nrow(occdf))
  expect_snapshot(
    out[, c("early_interval", "late_interval", "early_stage", "late_stage")]
  )

  # TODO: update docs to mention that we can't have assign_with_GTS = FALSE
  # and int_key = FALSE at the same time
  expect_snapshot(look_up(occdf, assign_with_GTS = FALSE), error = TRUE)

  # TODO: input type and value checks should come before checking whether int_key = FALSE
  # (for both snapshots below)
  expect_snapshot(look_up(occdf, assign_with_GTS = 1), error = TRUE)
  expect_snapshot(look_up(occdf, assign_with_GTS = "foo"), error = TRUE)
})

test_that("argument 'return_unassigned' works", {
  # drop missings
  occdf <- tetrapods[which(!tetrapods$late_interval %in% c(NA, " ", "")), ]
  occdf <- occdf[1:5, c("early_interval", "late_interval")]
  expect_equal(look_up(occdf, return_unassigned = TRUE), "Missourian")

  expect_message(
    expect_null(look_up(occdf[2:3, ], return_unassigned = TRUE)),
    "All intervals have been assigned."
  )
})

test_that("no error is produced when pre-Phanerozoic intervals are included", {
  occdf <- reefs
  occdf$interval[1] <- "Meso-archean"
  occdf <- occdf[which(occdf$interval %in% c("Ediacaran", "Meso-archean")), ]
  expect_warning(
    out <- look_up(
      occdf,
      early_interval = "interval",
      late_interval = "interval",
      assign_with_GTS = "GTS2012",
      int_key = FALSE
    ),
    regexp = "Ediacaran"
  )
  expect_snapshot(
    out[, c(
      "interval",
      "early_stage",
      "late_stage",
      "interval_max_ma",
      "interval_mid_ma",
      "interval_min_ma"
    )]
  )
})

test_that("", {
  # turn off "GTS": stage ages are not returned as not given in interval_key
  occdf <- tetrapods
  occdf$late_interval <- occdf$early_interval
  occdf <- occdf[which(!occdf$early_interval %in% GTS2020$interval_name), ]
  expect_equal(
    ncol(look_up(
      occdf[10:20, ],
      int_key = palaeoverse::interval_key,
      assign_with_GTS = FALSE
    )),
    34
  )

  # test with own interval key - reset occdf
  # define own interval key
  custom_key <- data.frame(
    interval_name = c("Missourian", "Gzhelian", "Capitanian"),
    early_stage = c("Kasimovian", "Gzhelian", "Capitanian"),
    late_stage = c("Sakmarian", "Gzhelian", "Capitanian"),
    max_ma = c(400, 360, 300),
    min_ma = c(370, 350, 290)
  )

  occdf <- occdf[which(occdf$early_interval %in% custom_key$interval_name), ]

  expect_equal(
    ncol(look_up(occdf[1:10, ], int_key = custom_key, assign_with_GTS = FALSE)),
    37
  )

  # check assignment just based on early_interval
  expect_equal(
    (look_up(
      occdf[1:10, ],
      int_key = custom_key,
      assign_with_GTS = FALSE,
      late_interval = "early_interval"
    ))$late_stage[1],
    "Sakmarian"
  )
})
