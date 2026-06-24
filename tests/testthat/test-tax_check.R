test_that("basic behavior works", {
  dat <- data.frame(
    genus = c(
      "Automaton",
      "Joebloggsia",
      "Facsimilus",
      "Joebloggia",
      "Facsimilu",
      "Facsimilu",
      NA
    )
  )
  expect_equal(
    tax_check(dat),
    list(
      synonyms = data.frame(
        group = c("F", "J"),
        greater = c("Facsimilu", "Joebloggsia"),
        lesser = c("Facsimilus", "Joebloggia"),
        count_greater = c(2, 1),
        count_lesser = 1
      ),
      non_letter_name = NULL,
      non_letter_group = NULL
    )
  )

  # input checks
  expect_snapshot(tax_check(data.frame()), error = TRUE)
  expect_snapshot(tax_check(1), error = TRUE)
})

test_that("arg 'name' works", {
  dat <- data.frame(
    foo = c(
      "Automaton",
      "Joebloggsia",
      "Facsimilus",
      "Joebloggia",
      "Facsimilu",
      "Facsimilu",
      NA
    )
  )

  # default name is "genus"
  expect_snapshot(tax_check(dat), error = TRUE)

  expect_equal(
    tax_check(dat, name = "foo"),
    list(
      synonyms = data.frame(
        group = c("F", "J"),
        greater = c("Facsimilu", "Joebloggsia"),
        lesser = c("Facsimilus", "Joebloggia"),
        count_greater = c(2, 1),
        count_lesser = 1
      ),
      non_letter_name = NULL,
      non_letter_group = NULL
    )
  )

  # input checks
  expect_snapshot(tax_check(dat, name = "nonexistent"), error = TRUE)
  expect_snapshot(tax_check(dat, name = 1), error = TRUE)
  expect_snapshot(tax_check(dat, name = NULL), error = TRUE)
  expect_snapshot(tax_check(dat, name = character(0)), error = TRUE)
  expect_snapshot(tax_check(dat, name = ""), error = TRUE)
})

test_that("arg 'group' works", {
  dat <- data.frame(
    family = c(
      "Foo",
      "Examplidae",
      "Examplidae",
      "Examplidae",
      "Taxonidae"
    ),
    genus = c(
      "Joebloggsia",
      "Joebloggia",
      "Facsimilus",
      "Facsimilu",
      "Facsimilu"
    )
  )

  # Without "group", we would have two groups "F" and "J"
  expect_equal(
    tax_check(dat, group = "family"),
    list(
      synonyms = data.frame(
        group = "Examplidae",
        greater = "Facsimilu",
        lesser = "Facsimilus",
        count_greater = 2L,
        count_lesser = 1L
      ),
      non_letter_name = NULL,
      non_letter_group = NULL
    )
  )

  # input checks
  expect_snapshot(tax_check(dat, group = "nonexistent"), error = TRUE)
  expect_snapshot(tax_check(dat, group = 1), error = TRUE)
  expect_snapshot(tax_check(dat, group = character(0)), error = TRUE)
  expect_snapshot(tax_check(dat, group = ""), error = TRUE)
})

test_that("arg 'dis' works", {
  dat <- data.frame(
    genus = c(
      "Automaton",
      "Joebloggsia",
      "Facsimilus",
      "Joebloggia",
      "Facsimilu",
      "Facstlwe",
      NA
    )
  )

  expect_equal(
    tax_check(dat, dis = 0.5),
    list(
      synonyms = data.frame(
        group = c("F", "F", "F", "J"),
        greater = c("Facsimilu", "Facsimilus", "Facsimilus", "Joebloggsia"),
        lesser = c("Facstlwe", "Facsimilu", "Facstlwe", "Joebloggia"),
        count_greater = 1L,
        count_lesser = 1L
      ),
      non_letter_name = NULL,
      non_letter_group = NULL
    )
  )

  # input checks
  expect_snapshot(tax_check(dat, dis = 1), error = TRUE)
  expect_snapshot(tax_check(dat, dis = 0), error = TRUE)
  expect_snapshot(tax_check(dat, dis = "a"), error = TRUE)
  expect_snapshot(tax_check(dat, dis = numeric(0)), error = TRUE)
  expect_snapshot(tax_check(dat, dis = NULL), error = TRUE)
})

test_that("arg 'start' works", {
  dat <- data.frame(
    genus = c(
      "Automaton",
      "Kunlungoides sp",
      "Kunmungoides sp",
      NA
    )
  )

  # "Kunlungoides sp" and "Kunmungoides sp" are reported as synonyms
  expect_equal(
    tax_check(dat),
    list(
      synonyms = data.frame(
        group = "K",
        greater = "Kunlungoides sp",
        lesser = "Kunmungoides sp",
        count_greater = 1L,
        count_lesser = 1L
      ),
      non_letter_name = NULL,
      non_letter_group = NULL
    )
  )

  # The letter that diverges for "Kunlungoides sp" and "Kunmungoides sp" is the 4th one
  # so this synonym isn't reported if start >= 4
  expect_equal(
    tax_check(dat, start = 4),
    list(
      synonyms = NULL,
      non_letter_name = NULL,
      non_letter_group = NULL
    )
  )

  # input checks
  expect_snapshot(tax_check(dat, start = -1), error = TRUE)
  expect_snapshot(tax_check(dat, start = numeric(0)), error = TRUE)
  expect_snapshot(tax_check(dat, start = "a"), error = TRUE)

  # TODO: should error
  # expect_snapshot(tax_check(dat, start = NULL), error = TRUE)
})

test_that("arg 'verbose' works", {
  dat <- data.frame(
    genus = c(
      "Automaton",
      "Kunlungoides sp",
      "Kunmungoides sp",
      NA
    )
  )

  expect_equal(
    tax_check(dat, verbose = FALSE),
    data.frame(
      group = "K",
      greater = "Kunlungoides sp",
      lesser = "Kunmungoides sp",
      count_greater = 1L,
      count_lesser = 1L
    )
  )

  # input checks
  expect_snapshot(tax_check(dat, verbose = 1), error = TRUE)
  expect_snapshot(tax_check(dat, verbose = numeric(0)), error = TRUE)
  expect_snapshot(tax_check(dat, verbose = "a"), error = TRUE)
  expect_snapshot(tax_check(dat, verbose = NULL), error = TRUE)
})
