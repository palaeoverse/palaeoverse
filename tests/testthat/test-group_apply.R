test_that("group_apply works", {
  # Load example data
  occdf <- tetrapods
  # Remove NA data
  occdf <- subset(occdf, !is.na(genus))
  # Number of occurrences from each country
  expect_equal(
    nrow(
      group_apply(occdf = occdf, group = c("cc"), fun = nrow)
    ),
    37
  )
  # Temporal range of data per time bin with default arguments
  expect_equal(
    nrow(
      group_apply(occdf = occdf, group = c("cc"), fun = tax_range_time)
    ),
    1248
  )
  # Temporal range of data per time bin with extra arguments
  expect_equal(
    nrow(
      group_apply(
        occdf = occdf,
        group = c("cc"),
        fun = tax_range_time,
        name = "family"
      )
    ),
    498
  )
  # Run at family level (default: "genus")
  # Use multiple grouping variables
  expect_equal(
    nrow(group_apply(
      occdf = occdf,
      group = c("collection_no", "cc"),
      fun = tax_range_time
    )),
    4017
  )

  expect_null(nrow(group_apply(
    occdf = occdf,
    group = c("collection_no"),
    fun = tax_check,
    verbose = FALSE
  )))
})

test_that("group_apply() puts groups last", {
  # Load example data
  occdf <- tetrapods[1:100, ]
  # Remove NA data
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

test_that("group_apply() error handling", {
  # Load example data
  occdf <- tetrapods
  # Remove NA data
  occdf <- subset(occdf, !is.na(genus))

  expect_snapshot(
    group_apply(occdf = 1, group = c("cc"), fun = tax_range_time),
    error = TRUE
  )
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = c("cc"),
      fun = "tax_range_time"
    ),
    error = TRUE
  )
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = c("test"),
      fun = tax_range_time
    ),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = occdf, fun = tax_range_time),
    error = TRUE
  )
  expect_snapshot(
    group_apply(occdf = occdf, group = c("cc"), fun = tax_range),
    error = TRUE
  )
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = c("cc"),
      fun = tax_range_time,
      not_an_argument = "test"
    ),
    error = TRUE
  )
  expect_snapshot(
    group_apply(
      occdf = occdf,
      group = c("cc"),
      fun = tax_range_time,
      not_an_argument1 = "test",
      not_an_argument2 = "test"
    ),
    error = TRUE
  )
})
