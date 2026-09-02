test_that("basic behaviour works", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae")
  )

  # T-rex row is duplicated so we drop one
  # fmt: skip
  expect_equal(
    tax_unique(
      occdf = dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      order = "order",
      class = "class"
    ),
    data.frame(
      class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda"),
      order = c("Orionides", "Coelurosauria", NA, NA),
      family = c("Spinosauridae", "Tyrannosauridae", "Diplodocidae", NA),
      genus = c("Spinosaurus", "Tyrannosaurus", NA, NA),
      genus_species = c("Spinosaurus aegyptiacus", "Tyrannosaurus rex", NA, NA),
      unique_name = c(
        "Spinosaurus aegyptiacus", "Tyrannosaurus rex", "Diplodocidae indet.",
        "Neosauropoda indet."
      )
    )
  )

  # must have columns genus and species
  expect_snapshot(
    tax_unique(
      data.frame(
        genus = "Tyrannosaurus",
        binomial = "Tyrannosaurus rex",
        family = "Tyrannosauridae",
        order = "Coelurosauria",
        class = "Tetanurae"
      ),
      species = "species",
      genus = "genus"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      data.frame(
        species = "Tyrannosaurus",
        binomial = "Tyrannosaurus rex",
        family = "Tyrannosauridae",
        order = "Coelurosauria",
        class = "Tetanurae"
      ),
      species = "species",
      genus = "genus"
    ),
    error = TRUE
  )

  # input checks
  expect_snapshot(tax_unique(data.frame()), error = TRUE)
  expect_snapshot(tax_unique(100), error = TRUE)
  expect_snapshot(tax_unique(NA), error = TRUE)
  skip_if(getRversion() < "4.3.0")
  expect_snapshot(tax_unique(), error = TRUE)
})

test_that("tax_unique() cannot use the same column for multiple arguments", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae")
  )
  expect_snapshot(
    tax_unique(dinosaurs, family = "species", genus = "species"),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(dinosaurs, binomial = "species", genus = "species"),
    error = TRUE
  )
})

test_that("arg 'binomial' works", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae")
  )

  # Supplying a single `binomial` column is equivalent to supplying separate
  # `species` and `genus` columns.
  expect_equal(
    tax_unique(
      occdf = dinosaurs,
      binomial = "binomial",
      family = "family",
      order = "order",
      class = "class"
    ),
    tax_unique(
      occdf = dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      order = "order",
      class = "class"
    )
  )

  # TODO: this should pass, cf https://github.com/palaeoverse/palaeoverse/issues/256
  #
  # Underscores in binomials are treated as spaces
  # dinosaurs$binomial <- gsub(" ", "_", dinosaurs$binomial)
  # expect_equal(
  #   tax_unique(
  #     occdf = dinosaurs,
  #     binomial = "binomial",
  #     family = "family",
  #     order = "order",
  #     class = "class"
  #   )$genus_species,
  #   c("Spinosaurus aegyptiacus", "Tyrannosaurus rex", NA, NA)
  # )

  # input checks
  expect_snapshot(tax_unique(dinosaurs, binomial = "test"), error = TRUE)
  expect_snapshot(tax_unique(dinosaurs, binomial = character(0)), error = TRUE)
  expect_snapshot(tax_unique(dinosaurs, binomial = 1), error = TRUE)
  expect_snapshot(tax_unique(dinosaurs, binomial = NA), error = TRUE)
})

test_that("arg 'name' works", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae"),
    accepted_name = "Tyrannosaurus rex"
  )

  expect_equal(
    tax_unique(
      occdf = dinosaurs,
      genus = "genus",
      family = "family",
      class = "class",
      name = "accepted_name"
    ),
    data.frame(
      class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda"),
      family = c("Spinosauridae", "Tyrannosauridae", "Diplodocidae", NA),
      genus = c("Spinosaurus", "Tyrannosaurus", NA, NA),
      genus_species = "Tyrannosaurus rex",
      unique_name = "Tyrannosaurus rex"
    )
  )

  # TODO: test for https://github.com/palaeoverse/palaeoverse/issues/257

  # input checks
  expect_snapshot(
    tax_unique(dinosaurs, genus = "genus", family = "family", name = "test"),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      dinosaurs,
      genus = "genus",
      family = "family",
      name = character(0)
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(dinosaurs, genus = "genus", family = "family", name = 1),
    error = TRUE
  )
})

test_that("higher taxonomic levels supplied via `...` work", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae")
  )

  expect_equal(
    tax_unique(
      dinosaurs,
      species = "species",
      genus = "genus",
      family = "family"
    ),
    data.frame(
      family = c("Spinosauridae", "Tyrannosauridae", "Diplodocidae"),
      genus = c("Spinosaurus", "Tyrannosaurus", NA),
      genus_species = c("Spinosaurus aegyptiacus", "Tyrannosaurus rex", NA),
      unique_name = c(
        "Spinosaurus aegyptiacus",
        "Tyrannosaurus rex",
        "Diplodocidae indet."
      )
    )
  )

  # at least one higher level is required
  expect_snapshot(
    tax_unique(occdf = dinosaurs, species = "species", genus = "genus"),
    error = TRUE
  )

  # input checks
  expect_snapshot(
    tax_unique(
      dinosaurs,
      species = "species",
      genus = "genus",
      family = "test"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      dinosaurs,
      species = "species",
      genus = "genus",
      family = character(0)
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      dinosaurs,
      species = "species",
      genus = "genus",
      family = NA
    ),
    error = TRUE
  )
})

test_that("arg 'resolution' works", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae")
  )

  # At genus resolution, species-level names collapse to "<genus> sp." and the
  # higher-level-only occurrences are still retained as cryptic diversity.
  expect_equal(
    tax_unique(
      occdf = dinosaurs,
      binomial = "binomial",
      family = "family",
      order = "order",
      class = "class",
      resolution = "genus"
    ),
    data.frame(
      class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda"),
      order = c("Orionides", "Coelurosauria", NA, NA),
      family = c("Spinosauridae", "Tyrannosauridae", "Diplodocidae", NA),
      genus = c("Spinosaurus", "Tyrannosaurus", NA, NA),
      unique_name = c(
        "Spinosaurus sp.",
        "Tyrannosaurus sp.",
        "Diplodocidae indet.",
        "Neosauropoda indet."
      )
    )
  )

  # a `genus` column alone (no `binomial`) is enough at genus resolution
  expect_equal(
    tax_unique(
      occdf = dinosaurs,
      genus = "genus",
      family = "family",
      order = "order",
      class = "class",
      resolution = "genus"
    )$unique_name,
    c(
      "Spinosaurus sp.",
      "Tyrannosaurus sp.",
      "Diplodocidae indet.",
      "Neosauropoda indet."
    )
  )

  # input checks
  expect_snapshot(
    tax_unique(
      occdf = dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      resolution = "test"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      occdf = dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      resolution = 1
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      occdf = dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      resolution = character(0)
    ),
    error = TRUE
  )
})

test_that("arg 'append' works", {
  # fmt: skip
  dinosaurs <- data.frame(
    species = c("rex", "aegyptiacus", NA, NA, "rex"),
    genus = c("Tyrannosaurus", "Spinosaurus", NA, NA, "Tyrannosaurus"),
    binomial = c("Tyrannosaurus rex", "Spinosaurus aegyptiacus", NA, NA, "Tyrannosaurus rex"),
    family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae", NA, "Tyrannosauridae"),
    order = c("Coelurosauria", "Orionides", NA, NA, "Coelurosauria"),
    class = c("Tetanurae", "Tetanurae", NA, "Neosauropoda", "Tetanurae")
  )

  expect_equal(
    tax_unique(
      occdf = dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      order = "order",
      class = "class",
      append = TRUE
    ),
    cbind(
      dinosaurs,
      data.frame(
        unique_name = c(
          "Tyrannosaurus rex",
          "Spinosaurus aegyptiacus",
          "Diplodocidae indet.",
          "Neosauropoda indet.",
          "Tyrannosaurus rex"
        )
      )
    )
  )

  # input checks
  expect_snapshot(
    tax_unique(
      dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      order = "order",
      class = "class",
      append = "foo"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      dinosaurs,
      species = "species",
      genus = "genus",
      family = "family",
      order = "order",
      class = "class",
      append = logical(0)
    ),
    error = TRUE
  )

  # TODO: using append = 1 passes because it can be coerced to logical, but I
  # don't think this should be allowed.
  #
  # expect_snapshot(
  #   tax_unique(
  #     dinosaurs,
  #     species = "species",
  #     genus = "genus",
  #     family = "family",
  #     order = "order",
  #     class = "class",
  #     append = 1
  #   ),
  #   error = TRUE
  # )
})

test_that("taxonomic columns must not contain punctuation", {
  # `identified_name` in the tetrapods dataset contains punctuation (e.g. "sp.")
  data("tetrapods")

  expect_snapshot(
    tax_unique(
      occdf = tetrapods,
      genus = "identified_name",
      family = "family",
      resolution = "genus"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      occdf = tetrapods,
      genus = "genus",
      family = "identified_name",
      resolution = "genus"
    ),
    error = TRUE
  )
  expect_snapshot(
    tax_unique(
      occdf = tetrapods,
      species = "identified_name",
      genus = "genus",
      family = "family",
      resolution = "genus"
    ),
    error = TRUE
  )
})
