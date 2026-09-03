test_that("time_bins() default behaviour", {
  res <- time_bins()

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 102)
  expect_named(
    res,
    c(
      "bin",
      "interval_name",
      "rank",
      "max_ma",
      "mid_ma",
      "min_ma",
      "duration_myr",
      "abbr",
      "colour",
      "font"
    )
  )
})

test_that("arg 'interval' works", {
  expect_equal(
    time_bins(interval = "Maastrichtian"),
    data.frame(
      bin = 1L,
      interval_name = "Maastrichtian",
      rank = "stage",
      max_ma = 72.1,
      mid_ma = 69.05,
      min_ma = 66,
      duration_myr = 6.1,
      abbr = "M",
      colour = "#F2FA8C",
      font = "black"
    )
  )

  expect_equal(
    time_bins(interval = 10),
    data.frame(
      bin = 1L,
      interval_name = "Tortonian",
      rank = "stage",
      max_ma = 11.63,
      mid_ma = 9.438,
      min_ma = 7.246,
      duration_myr = 4.384,
      abbr = "T",
      colour = "#FFFF66",
      font = "black"
    )
  )

  # numeric age range
  expect_equal(nrow(time_bins(interval = c(500, 0), scale = "GTS2012")), 94)

  # name range
  expect_equal(
    time_bins(interval = c("Fortunian", "Wuliuan")),
    data.frame(
      bin = 1:5,
      interval_name = c(
        "Fortunian",
        "Stage 2",
        "Stage 3",
        "Stage 4",
        "Wuliuan"
      ),
      rank = "stage",
      max_ma = c(541, 529, 521, 514, 509),
      mid_ma = c(535, 525, 517.5, 511.5, 506.75),
      min_ma = c(529, 521, 514, 509, 504.5),
      duration_myr = c(12, 8, 7, 5, 4.5),
      abbr = c("F", "S2", "S3", "S4", "W"),
      colour = c("#99B575", "#A6BA80", "#A6C583", "#B3CA8E", "#B3D492"),
      font = "black"
    )
  )

  # input checks

  # general
  expect_snapshot(time_bins(interval = NA), error = TRUE)
  expect_snapshot(time_bins(interval = NULL), error = TRUE)

  # name interval
  expect_snapshot(time_bins(interval = "foo"), error = TRUE)
  expect_snapshot(time_bins(interval = c("Mastrichtian", "foo")), error = TRUE)
  expect_snapshot(time_bins(interval = c("Mastrichtian", NA)), error = TRUE)
  expect_snapshot(time_bins(interval = letters[1:3]), error = TRUE)
  expect_snapshot(time_bins(interval = character(0)), error = TRUE)

  # numeric interval
  expect_snapshot(time_bins(interval = data.frame()), error = TRUE)
  expect_snapshot(time_bins(interval = 1:3), error = TRUE)
  expect_snapshot(time_bins(interval = -1, plot = TRUE), error = TRUE)
  expect_snapshot(time_bins(interval = 700), error = TRUE)
  expect_snapshot(time_bins(interval = numeric(0)), error = TRUE)
  expect_snapshot(time_bins(interval = c(10000, 100)), error = TRUE)
})

test_that("order of interval doesn't matter", {
  expect_equal(
    time_bins(interval = c("Fortunian", "Wuliuan")),
    time_bins(interval = c("Wuliuan", "Fortunian"))
  )
  expect_equal(
    time_bins(interval = c(500, 200)),
    time_bins(interval = c(200, 500))
  )
})

test_that("arg 'rank' works", {
  # name + period
  expect_equal(
    time_bins(interval = "Mesozoic", rank = "period"),
    data.frame(
      bin = 1:3,
      interval_name = c("Triassic", "Jurassic", "Cretaceous"),
      rank = "period",
      max_ma = c(251.902, 201.3, 145),
      mid_ma = c(226.601, 173.15, 105.5),
      min_ma = c(201.3, 145, 66),
      duration_myr = c(50.602, 56.3, 79),
      abbr = c("Tr", "J", "K"),
      colour = c("#812B92", "#34B2C9", "#7FC64E"),
      font = c("white", "black", "black")
    )
  )

  # name + eon
  expect_equal(
    time_bins(interval = "Mesozoic", rank = "eon"),
    data.frame(
      bin = 1L,
      interval_name = "Phanerozoic",
      rank = "eon",
      max_ma = 541,
      mid_ma = 270.5,
      min_ma = 0,
      duration_myr = 541,
      abbr = "P",
      colour = "#9AD9DD",
      font = "black"
    )
  )

  # name + era
  expect_equal(
    time_bins(interval = "Mesozoic", rank = "era"),
    data.frame(
      bin = 1L,
      interval_name = "Mesozoic",
      rank = "era",
      max_ma = 251.902,
      mid_ma = 158.951,
      min_ma = 66,
      duration_myr = 185.902,
      abbr = "M",
      colour = "#67C5CA",
      font = "black"
    )
  )

  # name + stage
  expect_equal(
    time_bins(interval = "Albian", rank = "stage"),
    data.frame(
      bin = 1L,
      interval_name = "Albian",
      rank = "stage",
      max_ma = 113,
      mid_ma = 106.75,
      min_ma = 100.5,
      duration_myr = 12.5,
      abbr = "A",
      colour = "#CCEA97",
      font = "black"
    )
  )

  # name + epoch
  expect_equal(
    time_bins(interval = c("Albian", "Danian"), rank = "epoch"),
    data.frame(
      bin = 1:3,
      interval_name = c("Lower Cretaceous", "Upper Cretaceous", "Paleocene"),
      rank = "epoch",
      max_ma = c(145, 100.5, 66),
      mid_ma = c(122.75, 83.25, 61),
      min_ma = c(100.5, 66, 56),
      duration_myr = c(44.5, 34.5, 10),
      abbr = c("LC", "UC", "P"),
      colour = c("#8CCD57", "#A6D84A", "#FDA75F"),
      font = "black"
    )
  )

  # numeric + period
  expect_equal(
    time_bins(interval = c(0, 200), rank = "period"),
    data.frame(
      bin = 1:5,
      interval_name = c(
        "Jurassic",
        "Cretaceous",
        "Paleogene",
        "Neogene",
        "Quaternary"
      ),
      rank = "period",
      max_ma = c(201.3, 145, 66, 23.03, 2.58),
      mid_ma = c(173.15, 105.5, 44.515, 12.805, 1.29),
      min_ma = c(145, 66, 23.03, 2.58, 0),
      duration_myr = c(56.3, 79, 42.97, 20.45, 2.58),
      abbr = c("J", "K", "Pg", "Ng", "Q"),
      colour = c("#34B2C9", "#7FC64E", "#FD9A52", "#FFE619", "#F9F97F"),
      font = "black"
    )
  )

  # input checks
  expect_snapshot(
    time_bins(interval = "Mesozoic", rank = "stages"),
    error = TRUE
  )
  expect_snapshot(
    time_bins(interval = "Mesozoic", rank = c("stage", "period")),
    error = TRUE
  )
  expect_snapshot(time_bins(interval = "Mesozoic", rank = NA), error = TRUE)
  expect_snapshot(
    time_bins(interval = "Mesozoic", rank = character(0)),
    error = TRUE
  )
})

test_that("arg 'size' works", {
  # correct amount of data returned
  # fmt: skip
  expect_message(
    expect_equal(
      time_bins(
        interval = c("Fortunian", "Meghalayan"),
        size = 200
      ),
      data.frame(
        bin = 1:3,
        max_ma = c(541, 358.9, 182.7),
        mid_ma = c(449.95, 270.8, 91.35),
        min_ma = c(358.9, 182.7, 0),
        duration_myr = c(182.1, 176.2, 182.7),
        grouping_rank = "stage",
        intervals = c(
          "Fortunian, Stage 2, Stage 3, Stage 4, Wuliuan, Drumian, Guzhangian, Paibian, Jiangshanian, Stage 10, Tremadocian, Floian, Dapingian, Darriwilian, Sandbian, Katian, Hirnantian, Rhuddanian, Aeronian, Telychian, Sheinwoodian, Homerian, Gorstian, Ludfordian, Pridoli, Lochkovian, Pragian, Emsian, Eifelian, Givetian, Frasnian, Famennian",
          "Tournaisian, Visean, Serpukhovian, Bashkirian, Moscovian, Kasimovian, Gzhelian, Asselian, Sakmarian, Artinskian, Kungurian, Roadian, Wordian, Capitanian, Wuchiapingian, Changhsingian, Induan, Olenekian, Anisian, Ladinian, Carnian, Norian, Rhaetian, Hettangian, Sinemurian, Pliensbachian",
          "Toarcian, Aalenian, Bajocian, Bathonian, Callovian, Oxfordian, Kimmeridgian, Tithonian, Berriasian, Valanginian, Hauterivian, Barremian, Aptian, Albian, Cenomanian, Turonian, Coniacian, Santonian, Campanian, Maastrichtian, Danian, Selandian, Thanetian, Ypresian, Lutetian, Bartonian, Priabonian, Rupelian, Chattian, Aquitanian, Burdigalian, Langhian, Serravallian, Tortonian, Messinian, Zanclean, Piacenzian, Gelasian, Calabrian, Chibanian, Upper Pleistocene, Greenlandian, Northgrippian, Meghalayan"
        )
      )
    ),
    "Target duration of equal length time bins was set to 200 Myr"
  )
  expect_message(
    expect_equal(
      nrow(time_bins(interval = c("Fortunian", "Meghalayan"), size = 6)),
      90
    ),
    "90 time bins were generated"
  )

  expect_snapshot(
    out <- time_bins(
      interval = c("Fortunian", "Meghalayan"),
      size = 200
    )
  )
  expect_snapshot(
    out <- time_bins(interval = c("Fortunian", "Meghalayan"), size = 6)
  )

  # Test edge effect resolve
  expect_message(
    expect_equal(
      nrow(time_bins(interval = "Phanerozoic", size = 25, rank = "stage")),
      22
    )
  )

  # Test single bin
  expect_message(
    expect_equal(
      nrow(time_bins(interval = "Phanerozoic", size = 1000, rank = "stage")),
      1
    )
  )

  # input checks
  expect_snapshot(time_bins(interval = "Mesozoic", size = "ten"), error = TRUE)
  expect_snapshot(
    time_bins(interval = "Mesozoic", size = numeric(0)),
    error = TRUE
  )
  expect_snapshot(time_bins(interval = "Mesozoic", size = NA), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", size = 1:2), error = TRUE)

  # TODO: should error
  # expect_snapshot(time_bins(interval = "Mesozoic", size = -1), error = TRUE)
})

test_that("arg 'n_bins' works", {
  expect_message(
    expect_equal(
      time_bins(interval = c(0, 200), rank = "period", n_bins = 4),
      data.frame(
        bin = 1:4,
        max_ma = c(201.3, 145, 66, 23.03),
        mid_ma = c(173.15, 105.5, 44.515, 11.515),
        min_ma = c(145, 66, 23.03, 0),
        duration_myr = c(56.3, 79, 42.97, 23.03),
        grouping_rank = "period",
        intervals = c(
          "Jurassic",
          "Cretaceous",
          "Paleogene",
          "Neogene, Quaternary"
        )
      )
    ),
    "Number of equal length time bins was set to 4"
  )

  # check that size- and n_bins-based bins are identical
  expect_message({
    bins_n <- time_bins(n_bins = 11)
  })
  expect_message({
    bins_size <- time_bins(size = 50)
  })
  expect_equal(bins_n, bins_size)

  # input checks
  expect_snapshot(
    time_bins(interval = "Mesozoic", n_bins = "ten"),
    error = TRUE
  )
  expect_snapshot(
    time_bins(interval = "Mesozoic", n_bins = numeric(0)),
    error = TRUE
  )
  expect_snapshot(time_bins(interval = "Mesozoic", n_bins = NA), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", n_bins = 1:2), error = TRUE)
  expect_snapshot(time_bins(n_bins = 200), error = TRUE)
})

test_that("arg 'assign' works", {
  expect_true(
    is.list(time_bins(
      interval = c("Fortunian", "Holocene"),
      scale = "GTS2012",
      assign = c(232, 167, 33)
    ))
  )
  expect_true(is.vector(
    time_bins(
      interval = c("Fortunian", "Meghalayan"),
      assign = c(232, 167, 33)
    )$Assignation
  ))

  # input checks
  expect_snapshot(time_bins(interval = "Mesozoic", assign = 40), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", assign = -40), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", assign = "30"), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", assign = NA), error = TRUE)
  # TODO: should error
  # expect_snapshot(
  #   time_bins(interval = "Mesozoic", assign = numeric(0)),
  #   error = TRUE
  # )
  expect_snapshot(time_bins(interval = "Mesozoic", assign = 1:2), error = TRUE)
})

test_that("arg 'scale' works", {
  # built-in scales
  expect_equal(
    nrow(time_bins(interval = c("Fortunian", "Meghalayan"), scale = "GTS2020")),
    102
  )
  expect_equal(
    nrow(time_bins(interval = c("Fortunian", "Holocene"), scale = "GTS2012")),
    100
  )
  expect_message(
    expect_equal(
      nrow(time_bins(
        interval = c("Fortunian", "Holocene"),
        scale = "GTS2012",
        size = 10
      )),
      54
    ),
    "Target duration of equal length time bins was set to 10 Myr"
  )

  # user-input data.frame
  scale <- data.frame(
    interval_name = 1:5,
    min_ma = c(0, 18, 32, 38, 45),
    max_ma = c(18, 32, 38, 45, 53)
  )
  expect_message(
    expect_equal(
      time_bins(scale = scale, size = 15),
      data.frame(
        bin = 1:4,
        max_ma = c(53, 45, 32, 18),
        mid_ma = c(49, 38.5, 25, 9),
        min_ma = c(45, 32, 18, 0),
        duration_myr = c(8, 13, 14, 18),
        grouping_rank = "user",
        intervals = c("5", "4, 3", "2", "1")
      )
    ),
    "Target duration of equal length time bins was set to 15 Myr"
  )

  # input checks
  expect_snapshot(time_bins(interval = "Mesozoic", scale = "foo"), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", scale = 1), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", scale = NA), error = TRUE)
  expect_snapshot(
    time_bins(interval = "Mesozoic", scale = character(0)),
    error = TRUE
  )
  scale <- data.frame(
    name = 1:5,
    min_ma = c(0, 18, 32, 38, 45),
    max_ma = c(18, 32, 38, 45, 53)
  )
  expect_snapshot(time_bins(scale = scale, size = 15), error = TRUE)
})

test_that("arg 'scale' works with macrostrat", {
  skip_if_offline(host = "macrostrat.org")

  expect_equal(nrow(time_bins(scale = "North american land mammal ages")), 19)
  expect_message(
    expect_equal(
      time_bins(size = 20, scale = "North american land mammal ages"),
      data.frame(
        bin = 1:3,
        max_ma = c(66, 39.9, 18.6),
        mid_ma = c(52.95, 29.25, 9.3057),
        min_ma = c(39.9, 18.6, 0.0114),
        duration_myr = c(26.1, 21.3, 18.5886),
        grouping_rank = "North american land mammal ages",
        intervals = c(
          "Puercan, Torrejonian, Tiffanian, Clarkforkian, Wasatchian, Bridgerian, Uintan",
          "Duchesnean, Chadronian, Orellan, Whitneyan, Arikareean",
          "Hemingfordian, Barstovian, Clarendonian, Hemphillian, Blancan, Irvingtonian, Rancholabrean"
        )
      )
    ),
    "Target duration of equal length time bins was set to 20 Myr"
  )
})

test_that("arg 'plot' works", {
  expect_doppelganger("time_bins() basic", function() {
    time_bins(interval = "Mesozoic", plot = TRUE)
  })

  # input checks
  expect_snapshot(time_bins(interval = "Mesozoic", plot = "TRUE"), error = TRUE)
  expect_snapshot(time_bins(interval = "Mesozoic", plot = NA), error = TRUE)
  expect_snapshot(
    time_bins(interval = "Mesozoic", plot = logical(0)),
    error = TRUE
  )
})
