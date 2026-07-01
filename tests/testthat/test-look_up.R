### Small dataframe that covers many edge cases

test_look_up <- rbind(
  # Single GTS stage
  c("both_stages_equal", "Capitanian", "Capitanian"),
  # Spans across two GTS stages
  c("two_stages_range", "Induan", "Olenekian"),

  # Late filled from early, should warn
  c("late_na", "Asselian", NA),
  c("late_empty", "Asselian", ""),
  c("late_blank", "Asselian", " "),

  # Non-stage GTS interval (epoch), resolved via max/min_ma
  c("gts_epoch", "Cisuralian", "Cisuralian"),

  # early only in `interval_key` (unassigned by GTS), late is a GTS stage -> mixed row
  c("key_only", "Missourian", "Gzhelian"),

  # pre-Cambrian in GTS but has no stage in `interval_key`
  c("precambrian", "Ediacaran", "Ediacaran"),

  # in neither GTS nor `interval_key` -> always unassigned
  c("unmatchable", "Meso-archean", "Meso-archean")
) |>
  as.data.frame()

colnames(test_look_up) <- c("case", "early_interval", "late_interval")

test_that("basic behavior works", {
  expect_equal(
    look_up(
      occdf = test_look_up[
        test_look_up$case %in% c("both_stages_equal", "two_stages_range"),
      ]
    ),
    data.frame(
      case = c("both_stages_equal", "two_stages_range"),
      early_interval = c("Capitanian", "Induan"),
      late_interval = c("Capitanian", "Olenekian"),
      early_stage = c("Capitanian", "Induan"),
      late_stage = c("Capitanian", "Olenekian"),
      interval_max_ma = c(265.1, 251.902),
      interval_mid_ma = c(262.1, 249.551),
      interval_min_ma = c(259.1, 247.2)
    )
  )
})

test_that("wrong input for argument 'occdf'", {
  expect_snapshot(look_up(1), error = TRUE)
  expect_snapshot(look_up(NA), error = TRUE)
  expect_snapshot(look_up(NULL), error = TRUE)
})

test_that("look_up() warns if late interval is NA, empty, or blank", {
  expect_warning(
    expect_equal(
      look_up(occdf = test_look_up[test_look_up$case == "late_na", ]),
      data.frame(
        case = "late_na",
        early_interval = "Asselian",
        late_interval = NA_character_,
        early_stage = "Asselian",
        late_stage = "Asselian",
        interval_max_ma = 298.9,
        interval_mid_ma = 296.21,
        interval_min_ma = 293.52,
        row.names = 3L
      )
    ),
    "filled in with the corresponding"
  )
  expect_warning(
    expect_equal(
      look_up(occdf = test_look_up[test_look_up$case == "late_empty", ]),
      data.frame(
        case = "late_empty",
        early_interval = "Asselian",
        late_interval = "",
        early_stage = "Asselian",
        late_stage = "Asselian",
        interval_max_ma = 298.9,
        interval_mid_ma = 296.21,
        interval_min_ma = 293.52,
        row.names = 4L
      )
    ),
    "filled in with the corresponding"
  )
  expect_warning(
    expect_equal(
      look_up(occdf = test_look_up[test_look_up$case == "late_blank", ]),
      data.frame(
        case = "late_blank",
        early_interval = "Asselian",
        late_interval = " ",
        early_stage = "Asselian",
        late_stage = "Asselian",
        interval_max_ma = 298.9,
        interval_mid_ma = 296.21,
        interval_min_ma = 293.52,
        row.names = 5L
      )
    ),
    "filled in with the corresponding"
  )
})

test_that("look_up() warns if some intervals couldn't be matched", {
  expect_warning(
    expect_equal(
      look_up(occdf = test_look_up[test_look_up$case == "key_only", ]),
      data.frame(
        case = "key_only",
        early_interval = "Missourian",
        late_interval = "Gzhelian",
        early_stage = NA_character_,
        late_stage = "Gzhelian",
        interval_max_ma = NA_real_,
        interval_mid_ma = NA_real_,
        interval_min_ma = 298.9,
        row.names = 7L
      )
    ),
    "The following intervals could not be matched with intervals from int_key"
  )
})

test_that("arguments 'early_interval' and 'late_interval' work", {
  dat <- test_look_up
  colnames(dat) <- c("case", "early", "late")

  expect_equal(
    look_up(
      occdf = dat[dat$case %in% c("both_stages_equal", "two_stages_range"), ],
      early_interval = "early",
      late_interval = "late"
    ),
    data.frame(
      case = c("both_stages_equal", "two_stages_range"),
      early = c("Capitanian", "Induan"),
      late = c("Capitanian", "Olenekian"),
      early_stage = c("Capitanian", "Induan"),
      late_stage = c("Capitanian", "Olenekian"),
      interval_max_ma = c(265.1, 251.902),
      interval_mid_ma = c(262.1, 249.551),
      interval_min_ma = c(259.1, 247.2)
    )
  )

  # either one or both interval colums are not found in the data
  expect_snapshot(look_up(occdf = dat), error = TRUE)
  expect_snapshot(
    look_up(occdf = dat, early_interval = "early"),
    error = TRUE
  )
  expect_snapshot(look_up(occdf = dat, late_interval = "late"), error = TRUE)

  # wrong input type
  expect_snapshot(look_up(occdf = dat, early_interval = 1), error = TRUE)
  expect_snapshot(look_up(occdf = dat, early_interval = NA), error = TRUE)
  expect_snapshot(
    look_up(occdf = dat, early_interval = c("a", "b")),
    error = TRUE
  )
  expect_snapshot(look_up(occdf = dat, late_interval = 1), error = TRUE)
  expect_snapshot(
    look_up(occdf = dat, early_interval = "early", late_interval = NA),
    error = TRUE
  )
  expect_snapshot(
    look_up(occdf = dat, late_interval = c("a", "b")),
    error = TRUE
  )
})

test_that("argument 'int_key' works", {
  occdf <- test_look_up[test_look_up$case %in% c("key_only", "precambrian"), ]

  # A custom int_key assigns its own stage names. Intervals missing from the
  # key still fall back to GTS, e.g. the late Gzhelian stage of `key_only`,
  # which is why it keeps a numeric `interval_min_ma`.
  my_interval <- data.frame(
    interval_name = c("Missourian", "Ediacaran"),
    early_stage = c("foo1", "foo2"),
    late_stage = c("bar1", "bar2")
  )
  expect_equal(
    look_up(occdf, int_key = my_interval),
    data.frame(
      case = c("key_only", "precambrian"),
      early_interval = c("Missourian", "Ediacaran"),
      late_interval = c("Gzhelian", "Ediacaran"),
      early_stage = c("foo1", "foo2"),
      late_stage = c("Gzhelian", "bar2"),
      interval_max_ma = c(NA_real_, NA_real_),
      interval_mid_ma = c(NA_real_, NA_real_),
      interval_min_ma = c(298.9, NA_real_),
      row.names = 7:8
    )
  )

  # wrong input for int_key
  expect_snapshot(look_up(occdf, int_key = 1), error = TRUE)
  expect_snapshot(look_up(occdf, int_key = c("a", "b")), error = TRUE)

  # missing column(s) in int_key
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
  # wrong column type
  expect_snapshot(
    look_up(
      occdf,
      int_key = data.frame(
        interval_name = c("Induan", "Asselian"),
        early_stage = 1:2,
        late_stage = c("foo1", "foo2")
      )
    ),
    error = TRUE
  )
  # max_ma and min_ma must be numeric
  expect_snapshot(
    look_up(
      occdf,
      int_key = data.frame(
        interval_name = c("Induan", "Asselian"),
        early_stage = c("foo1", "foo2"),
        late_stage = c("foo1", "foo2"),
        max_ma = c("a", "b")
      )
    ),
    error = TRUE
  )
  expect_snapshot(
    look_up(
      occdf,
      int_key = data.frame(
        interval_name = c("Induan", "Asselian"),
        early_stage = c("foo1", "foo2"),
        late_stage = c("foo1", "foo2"),
        min_ma = c("a", "b")
      )
    ),
    error = TRUE
  )
})

test_that("int_key works with columns 'min_ma' and 'max_ma'", {
  occdf <- test_look_up[
    test_look_up$case %in% c("both_stages_equal", "key_only", "unmatchable"),
  ]

  custom_key <- data.frame(
    interval_name = c("Missourian", "Gzhelian", "Capitanian"),
    early_stage = c("Kasimovian", "Gzhelian", "Capitanian"),
    late_stage = c("Sakmarian", "Gzhelian", "Capitanian"),
    max_ma = c(400, 360, 300),
    min_ma = c(370, 350, 290)
  )

  # `unmatchable` is in neither the key nor GTS, so it stays unassigned and warns
  expect_warning(
    expect_equal(
      look_up(occdf, int_key = custom_key, assign_with_GTS = FALSE),
      data.frame(
        case = c("both_stages_equal", "key_only", "unmatchable"),
        early_interval = c("Capitanian", "Missourian", "Meso-archean"),
        late_interval = c("Capitanian", "Gzhelian", "Meso-archean"),
        early_stage = c("Capitanian", "Kasimovian", NA),
        late_stage = c("Capitanian", "Gzhelian", NA),
        interval_max_ma = c(300, 400, NA),
        interval_mid_ma = c(295, 375, NA),
        interval_min_ma = c(290, 350, NA),
        row.names = c(1L, 7L, 9L)
      )
    ),
    "The following intervals could not be matched with"
  )
})

test_that("argument 'assign_with_GTS' works", {
  occdf <- test_look_up[
    test_look_up$case %in%
      c("both_stages_equal", "two_stages_range", "gts_epoch"),
  ]
  expect_equal(
    look_up(occdf, assign_with_GTS = "GTS2020"),
    data.frame(
      case = c("both_stages_equal", "two_stages_range", "gts_epoch"),
      early_interval = c("Capitanian", "Induan", "Cisuralian"),
      late_interval = c("Capitanian", "Olenekian", "Cisuralian"),
      early_stage = c("Capitanian", "Induan", "Asselian"),
      late_stage = c("Capitanian", "Olenekian", "Kungurian"),
      interval_max_ma = c(265.1, 251.902, 298.9),
      interval_mid_ma = c(262.1, 249.551, 285.925),
      interval_min_ma = c(259.1, 247.2, 272.95),
      row.names = c(1L, 2L, 6L)
    )
  )
  expect_equal(
    look_up(occdf, assign_with_GTS = "GTS2012"),
    data.frame(
      case = c("both_stages_equal", "two_stages_range", "gts_epoch"),
      early_interval = c("Capitanian", "Induan", "Cisuralian"),
      late_interval = c("Capitanian", "Olenekian", "Cisuralian"),
      early_stage = c("Capitanian", "Induan", "Asselian"),
      late_stage = c("Capitanian", "Olenekian", "Kungurian"),
      interval_max_ma = c(265.1, 252.2, 298.9),
      interval_mid_ma = c(262.5, 249.7, 285.6),
      interval_min_ma = c(259.9, 247.2, 272.3),
      row.names = c(1L, 2L, 6L)
    )
  )

  # with assign_with_GTS = FALSE, only the int_key is used: no GTS fallback and
  # no age columns. `Gzhelian` is not an interval name in the key, so the late
  # stage of `key_only` stays unassigned and a warning is raised.
  occdf <- test_look_up[test_look_up$case %in% c("key_only", "precambrian"), ]
  expect_warning(
    expect_equal(
      look_up(occdf, int_key = interval_key, assign_with_GTS = FALSE),
      data.frame(
        case = c("key_only", "precambrian"),
        early_interval = c("Missourian", "Ediacaran"),
        late_interval = c("Gzhelian", "Ediacaran"),
        early_stage = c("Kasimovian", "Ediacaran"),
        late_stage = c(NA, "Ediacaran"),
        row.names = 7:8
      )
    ),
    "The following intervals could not be matched with"
  )

  # input check
  expect_snapshot(
    look_up(occdf, int_key = interval_key, assign_with_GTS = "foo"),
    error = TRUE
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
  expect_equal(
    look_up(
      test_look_up[
        !(test_look_up$case %in% c("late_na", "late_empty", "late_blank")),
      ],
      return_unassigned = TRUE
    ),
    c("Ediacaran", "Meso-archean", "Missourian")
  )

  expect_message(
    expect_null(look_up(
      test_look_up[
        !(test_look_up$case %in% c("late_na", "late_empty", "late_blank")) &
          !(test_look_up$early_interval %in%
            c("Ediacaran", "Meso-archean", "Missourian")),
      ],
      return_unassigned = TRUE
    )),
    "All intervals have been assigned."
  )
})

test_that("pre-Phanerozoic intervals are handled without error", {
  # With int_key = FALSE, assignment relies on GTS alone. `Ediacaran` exists in
  # GTS only as a pre-Cambrian period, with no stage beneath it (the oldest GTS
  # stage is Fortunian), so it must be dropped from the GTS assignment rather
  # than causing an error. `Meso-archean` is absent from GTS entirely. Both end
  # up unassigned (with a warning), but the function still returns a well-formed
  # dataframe.
  occdf <- test_look_up[
    test_look_up$case %in% c("precambrian", "unmatchable"),
  ]
  expect_warning(
    expect_equal(
      look_up(occdf, assign_with_GTS = "GTS2012", int_key = FALSE),
      data.frame(
        case = c("precambrian", "unmatchable"),
        early_interval = c("Ediacaran", "Meso-archean"),
        late_interval = c("Ediacaran", "Meso-archean"),
        early_stage = c(NA_character_, NA_character_),
        late_stage = c(NA_character_, NA_character_),
        interval_max_ma = c(NA_real_, NA_real_),
        interval_mid_ma = c(NA_real_, NA_real_),
        interval_min_ma = c(NA_real_, NA_real_),
        row.names = 8:9
      )
    ),
    "The following intervals could not be matched with"
  )
})

test_that("'early_interval' and 'late_interval' can point to the same column", {
  occdf <- test_look_up[
    test_look_up$case %in% c("both_stages_equal", "gts_epoch"),
    c("case", "early_interval")
  ]
  colnames(occdf) <- c("case", "interval")

  expect_equal(
    look_up(occdf, early_interval = "interval", late_interval = "interval"),
    data.frame(
      case = c("both_stages_equal", "gts_epoch"),
      interval = c("Capitanian", "Cisuralian"),
      early_stage = c("Capitanian", "Asselian"),
      late_stage = c("Capitanian", "Kungurian"),
      interval_max_ma = c(265.1, 298.9),
      interval_mid_ma = c(262.1, 285.925),
      interval_min_ma = c(259.1, 272.95),
      row.names = c(1L, 6L)
    )
  )

  # Ensure that we get the same results when early_interval and late_interval
  # are equal
  occdf <- data.frame(
    case = c("both_stages_equal", "gts_epoch"),
    early_interval = c("Capitanian", "Cisuralian"),
    late_interval = c("Capitanian", "Cisuralian")
  )
  expect_equal(
    look_up(occdf),
    data.frame(
      case = c("both_stages_equal", "gts_epoch"),
      early_interval = c("Capitanian", "Cisuralian"),
      late_interval = c("Capitanian", "Cisuralian"),
      early_stage = c("Capitanian", "Asselian"),
      late_stage = c("Capitanian", "Kungurian"),
      interval_max_ma = c(265.1, 298.9),
      interval_mid_ma = c(262.1, 285.925),
      interval_min_ma = c(259.1, 272.95),
      row.names = c(1L, 2L)
    )
  )
})
