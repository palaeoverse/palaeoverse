# basic behaviour works

    Code
      tax_unique(data.frame(genus = "Tyrannosaurus", binomial = "Tyrannosaurus rex",
        family = "Tyrannosauridae", order = "Coelurosauria", class = "Tetanurae"),
      species = "species", genus = "genus")
    Condition
      Error in `tax_unique()`:
      ! Column "species" not found in `occdf`.

---

    Code
      tax_unique(data.frame(species = "Tyrannosaurus", binomial = "Tyrannosaurus rex",
        family = "Tyrannosauridae", order = "Coelurosauria", class = "Tetanurae"),
      species = "species", genus = "genus")
    Condition
      Error in `tax_unique()`:
      ! Column "genus" not found in `occdf`.

---

    Code
      tax_unique(data.frame())
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family = "family"`).

---

    Code
      tax_unique(100)
    Condition
      Error in `tax_unique()`:
      ! `occdf` must be of class <data.frame>, not the number 100.

---

    Code
      tax_unique(NA)
    Condition
      Error in `tax_unique()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      tax_unique()
    Condition
      Error in `tax_unique()`:
      ! `occdf` must be of class <data.frame>, not absent.

# tax_unique() cannot use the same column for multiple arguments

    Code
      tax_unique(dinosaurs, family = "species", genus = "species")
    Condition
      Error in `tax_unique()`:
      ! Species names must be supplied to estimate richness at species level.
      i Specify `binomial`, or `genus` and `species`, or `genus` and `name`.

---

    Code
      tax_unique(dinosaurs, binomial = "species", genus = "species")
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family = "family"`).

# arg 'binomial' works

    Code
      tax_unique(dinosaurs, binomial = "test")
    Condition
      Error in `tax_unique()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_unique(dinosaurs, binomial = character(0))
    Condition
      Error in `tax_unique()`:
      ! `binomial` must be a single string, not an empty character vector.

---

    Code
      tax_unique(dinosaurs, binomial = 1)
    Condition
      Error in `tax_unique()`:
      ! `binomial` must be a single string, not the number 1.

---

    Code
      tax_unique(dinosaurs, binomial = NA)
    Condition
      Error in `tax_unique()`:
      ! `binomial` must be a single string, not `NA`.

# arg 'name' works

    Code
      tax_unique(dinosaurs, genus = "genus", family = "family", name = "test")
    Condition
      Error in `tax_unique()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_unique(dinosaurs, genus = "genus", family = "family", name = character(0))
    Condition
      Error in `tax_unique()`:
      ! `name` must be a single string, not an empty character vector.

---

    Code
      tax_unique(dinosaurs, genus = "genus", family = "family", name = 1)
    Condition
      Error in `tax_unique()`:
      ! `name` must be a single string, not the number 1.

# higher taxonomic levels supplied via `...` work

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus")
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family = "family"`).

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = "test")
    Condition
      Error in `tax_unique()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = character(
        0))
    Condition
      Error in `tax_unique()`:
      ! `family` must be a single string, not an empty character vector.

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = NA)
    Condition
      Error in `tax_unique()`:
      ! `family` must be a single string, not `NA`.

# arg 'resolution' works

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = "test")
    Condition
      Error in `tax_unique()`:
      ! `resolution` must be one of "species" or "genus", not "test".

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = 1)
    Condition
      Error in `tax_unique()`:
      ! `resolution` must be a character vector, not the number 1.

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = character(0))
    Condition
      Error in `tax_unique()`:
      ! `resolution` must be length 1, not length 0

# arg 'append' works

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = "family",
        order = "order", class = "class", append = "foo")
    Condition
      Error in `tax_unique()`:
      ! `append` must be `TRUE` or `FALSE`, not the string "foo".

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = "family",
        order = "order", class = "class", append = logical(0))
    Condition
      Error in `tax_unique()`:
      ! `append` must be `TRUE` or `FALSE`, not an empty logical vector.

# taxonomic columns must not contain punctuation

    Code
      tax_unique(occdf = tetrapods, genus = "identified_name", family = "family",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! Column "genus" must not contain punctuation.

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "identified_name",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! Column "family" must not contain punctuation.

---

    Code
      tax_unique(occdf = tetrapods, species = "identified_name", genus = "genus",
        family = "family", resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! Column "species" must not contain punctuation.

