test_that("group_apply() basic behavior", {
  occdf <- tetrapods[1:50, ]

  expect_equal(
    group_apply(occdf = occdf, group = "cc", fun = nrow),
    data.frame(
      nrow = c(1, 5, 6, 35, 3),
      cc = c("CA", "RU", "UK", "US", "ZA")
    ),
    # TODO: this is necessary because the first column is a named vector but
    # I guess this should be a simple vector instead.
    ignore_attr = TRUE
  )

  # several groups
  expect_equal(
    group_apply(occdf = occdf, group = c("collection_no", "cc"), fun = nrow),
    data.frame(
      nrow = c(1, 5, 4, 2, 1, 1, 1, 1, 1, 9, 6, 6, 3, 6, 1, 1, 1),
      collection_no = c(
        "13219",
        "22644",
        "22725",
        "22726",
        "12943",
        "13044",
        "13046",
        "13048",
        "13049",
        "13080",
        "13257",
        "13947",
        "22635",
        "22714",
        "13004",
        "13043",
        "13083"
      ),
      cc = c("CA", "RU", "UK", "UK", rep("US", 10), "ZA", "ZA", "ZA"),
      row.names = 1:17
    )
  )
})

test_that("error handling for argument 'occdf'", {
  occdf <- tetrapods[1:50, ]
  expect_snapshot(
    group_apply(group = "cc", fun = nrow),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = 1, group = "cc", fun = nrow),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = data.frame(), group = "cc", fun = nrow),
    error = TRUE
  )
})

test_that("group_apply() accepts functions that return less or more rows that in the input", {
  occdf <- tetrapods[1:100, ]
  occdf <- subset(occdf, !is.na(genus))
  expect_equal(
    nrow(group_apply(occdf = occdf, group = "cc", fun = tax_range_time)),
    48
  )
  expect_equal(
    nrow(
      group_apply(
        occdf = occdf,
        group = "cc",
        fun = tax_range_time,
        name = "family"
      )
    ),
    35
  )
  expect_equal(
    nrow(
      group_apply(
        occdf = occdf,
        group = c("collection_no", "cc"),
        fun = tax_range_time
      )
    ),
    80
  )

  # can return no rows at all
  expect_null(
    group_apply(
      occdf = occdf,
      group = "collection_no",
      fun = tax_check,
      verbose = FALSE
    )
  )
})

test_that("group_apply() puts groups last", {
  occdf <- tetrapods[1:100, ]
  occdf <- subset(occdf, !is.na(genus))

  # Single group
  expect_named(
    group_apply(occdf = occdf, group = "cc", fun = nrow),
    c("nrow", "cc")
  )

  # Several groups
  expect_named(
    group_apply(occdf = occdf, group = c("cc", "formation"), fun = nrow),
    c("nrow", "cc", "formation")
  )

  # Hits the "list" branch in group_apply()
  expect_named(
    group_apply(
      occdf = occdf,
      group = "collection_no",
      fun = tax_unique,
      genus = "genus",
      family = "family",
      order = "order",
      class = "class",
      resolution = "genus"
    ),
    c("class", "order", "family", "genus", "unique_name", "collection_no")
  )
  expect_named(
    group_apply(
      occdf = occdf,
      group = c("cc", "formation"),
      fun = tax_range_time
    ),
    c(
      "taxon",
      "taxon_id",
      "max_ma",
      "min_ma",
      "range_myr",
      "n_occ",
      "cc",
      "formation"
    )
  )
})

test_that("error handling for argument 'group'", {
  occdf <- tetrapods[1:50, ]
  expect_snapshot(group_apply(occdf = occdf, fun = nrow), error = TRUE)
  expect_snapshot(
    group_apply(occdf = occdf, group = NULL, fun = nrow),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = occdf, group = "foo", fun = nrow),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = occdf, group = 1, fun = nrow),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = occdf, group = c("cc", "foobar"), fun = nrow),
    error = TRUE
  )

  # using an external object in 'group' when 'group' has multiple values

  # TODO: this gives a different error message depending on whether I do
  # testthat::test_file("tests/testthat/test-group_apply.R") ("cannot xtfrm data frames")
  # or devtools::test() ("object foo not found")

  # foo <- mtcars
  # expect_snapshot(
  #   group_apply(occdf = occdf, group = c("cc", "foo"), fun = nrow),
  #   error = TRUE
  # )
})

test_that("error handling for argument 'fun'", {
  occdf <- tetrapods
  occdf <- subset(occdf, !is.na(genus))

  # quoted function name isn't accepted
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = "cc",
      fun = "tax_range_time"
    ),
    error = TRUE
  )
  # unknown function
  expect_snapshot(
    group_apply(occdf = occdf, group = "cc", fun = foobar),
    error = TRUE
  )
  # one unknown arg
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = "cc",
      fun = tax_range_time,
      not_an_argument = "test"
    ),
    error = TRUE
  )
  # multiple unknown args
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = "cc",
      fun = tax_range_time,
      not_an_argument1 = "test",
      not_an_argument2 = "test"
    ),
    error = TRUE
  )
})
