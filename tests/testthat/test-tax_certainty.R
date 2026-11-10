test_that("throws error for missing required arguments", {
  expect_snapshot(tax_certainty(data = 1, name = "foo"), error = TRUE)
  skip_if(getRversion() < "4.3.0")
  expect_snapshot(tax_certainty(), error = TRUE)
  expect_snapshot(tax_certainty(data = tetrapods), error = TRUE)
})

test_that("tax_certainty errors with unnamed args", {
  data <- tetrapods[1:5, ]
  expect_snapshot(tax_certainty(data, "identified_name"), error = TRUE)
  expect_snapshot(tax_certainty(data = data, "identified_name"), error = TRUE)
  expect_snapshot(tax_certainty(data, "identified_name", NULL), error = TRUE)
  expect_snapshot(
    tax_certainty(data, "identified_name", terms = NULL),
    error = TRUE
  )
})

test_that("tax_certainty() basic behavior", {
  data("tetrapods")
  data <- tetrapods[1:5, ]
  expect_equal(
    tax_certainty(data = data, name = "identified_name"),
    cbind(data, data.frame(certainty = c(1, 0, 1, 1, 1)))
  )

  # input checks
  expect_snapshot(
    tax_certainty(data = data.frame(), name = "identified_name"),
    error = TRUE
  )
})

test_that("arg 'name' works", {
  data("tetrapods")
  data <- tetrapods[1:5, "identified_name", drop = FALSE]
  colnames(data) <- "new_name"

  expect_equal(
    tax_certainty(data = data, name = "new_name"),
    cbind(data, data.frame(certainty = c(1, 0, 1, 1, 1)))
  )

  # input checks
  expect_snapshot(
    tax_certainty(data = data, name = "foo"),
    error = TRUE
  )
  expect_snapshot(
    tax_certainty(data = data, name = NULL),
    error = TRUE
  )

  # TODO: this should error, docs say this should be a column name, not a column index
  # expect_snapshot(
  #   tax_certainty(data = data, name = 1),
  #   error = TRUE
  # )
})

test_that("arg 'terms' works", {
  data("tetrapods")
  data <- tetrapods[1:5, "identified_name", drop = FALSE]

  # "Broiliellus n. sp. arroyoensis" and "n. gen. Ophiodeirus n. sp. casei"
  # now become uncertain
  expect_equal(
    tax_certainty(
      data = data,
      name = "identified_name",
      terms = list(
        species = "Broiliellus n. sp. arroyoensis",
        genus = "Ophiodeirus"
      )
    ),
    cbind(data, data.frame(certainty = c(1, 0, 1, 0, 0)))
  )

  # input checks
  expect_snapshot(
    tax_certainty(data = data, name = "identified_name", terms = 1),
    error = TRUE
  )

  # TODO: should error
  # expect_snapshot(
  #   tax_certainty(data = data, name = "identified_name", terms = list(1)),
  #   error = TRUE
  # )

  # TODO: should error
  # expect_snapshot(
  #   tax_certainty(data = data, name = "identified_name", terms = list(a = 1)),
  #   error = TRUE
  # )
})

test_that("arg 'certainty' works", {
  data("tetrapods")
  data <- tetrapods[1:5, ]

  expect_equal(
    tax_certainty(
      data = data,
      name = "identified_name",
      certainty = c("A", "B")
    ),
    cbind(data, data.frame(certainty = c("A", "B", "A", "A", "A")))
  )

  # check mixing character and numeric
  expect_equal(
    tax_certainty(
      data = data,
      name = "identified_name",
      certainty = c("A", 0)
    ),
    cbind(data, data.frame(certainty = c("A", "0", "A", "A", "A")))
  )

  # input checks

  # TODO: both tests below should error - docs say "A vector of length two denoting how
  # certainty should be coded."
  #
  # expect_snapshot(
  #   tax_certainty(data = data, name = "identified_name", certainty = "A"),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   tax_certainty(data = data, name = "identified_name", certainty = c("A", "B", "C")),
  #   error = TRUE
  # )

  # TODO: should error, this doesn't return the column "certainty"
  # expect_snapshot(
  #   tax_certainty(data = data, name = "identified_name", certainty = NULL),
  #   error = TRUE
  # )

  # TODO: should error, this returns 1 everywhere
  # expect_snapshot(
  #   tax_certainty(data = data, name = "identified_name", certainty = c(1, 1)),
  #   error = TRUE
  # )
})

test_that("arg 'append' works", {
  data("tetrapods")
  data <- tetrapods[1:5, c("identified_name", "life_habit")]

  expect_equal(
    tax_certainty(
      data = data,
      name = "identified_name",
      append = FALSE
    ),
    cbind(
      data[, "identified_name", drop = FALSE],
      data.frame(certainty = c(1, 0, 1, 1, 1))
    )
  )

  # input checks

  expect_snapshot(
    tax_certainty(data = data, name = "identified_name", append = 1),
    error = TRUE
  )
  expect_snapshot(
    tax_certainty(data = data, name = "identified_name", append = NA),
    error = TRUE
  )
})
