# basic behavior works

    Code
      tax_unique(data.frame())
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family`)

---

    Code
      tax_unique(100)
    Condition
      Error in `tax_unique()`:
      ! `occdf` must be a data.frame

---

    Code
      tax_unique(NA)
    Condition
      Error in `tax_unique()`:
      ! `occdf` must be a data.frame

# tax_unique() cannot use the same column for multiple arguments

    Code
      tax_unique(dinosaurs, family = "species", genus = "species")
    Condition
      Error in `tax_unique()`:
      ! Species names must be supplied by specifying `binomial`, `genus` and
          `species`, or `genus` and `name` columns to estimate richness at species
          level

---

    Code
      tax_unique(dinosaurs, binomial = "species", genus = "species")
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family`)

# arg 'binomial' works

    Code
      tax_unique(dinosaurs, binomial = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `binomial`

---

    Code
      tax_unique(dinosaurs, binomial = character(0))
    Condition
      Error in `if (!is.null(binomial) && !(binomial %in% colnames(occdf))) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      tax_unique(dinosaurs, binomial = 1)
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `binomial`

---

    Code
      tax_unique(dinosaurs, binomial = NA)
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `binomial`

# arg 'name' works

    Code
      tax_unique(dinosaurs, genus = "genus", family = "family", name = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `names`

---

    Code
      tax_unique(dinosaurs, genus = "genus", family = "family", name = character(0))
    Condition
      Error in `if (!is.null(name) && !(name %in% colnames(occdf))) ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      tax_unique(dinosaurs, genus = "genus", family = "family", name = 1)
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `names`

# higher taxonomic levels supplied via `...` work

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus")
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family`)

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `family`

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = character(
        0))
    Condition
      Error in `if (!(col_name %in% colnames(occdf))) ...`:
      ! argument is of length zero

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = NA)
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `family`

# arg 'resolution' works

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = "test")
    Condition
      Error in `tax_unique()`:
      ! Resolution must be 'species' or 'genus'

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = 1)
    Condition
      Error in `tax_unique()`:
      ! Resolution must be 'species' or 'genus'

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = character(0))
    Condition
      Error in `if ((resolution != "species") && (resolution != "genus")) ...`:
      ! missing value where TRUE/FALSE needed

# arg 'append' works

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = "family",
        order = "order", class = "class", append = "foo")
    Condition
      Error in `if (append) ...`:
      ! argument is not interpretable as logical

---

    Code
      tax_unique(dinosaurs, species = "species", genus = "genus", family = "family",
        order = "order", class = "class", append = logical(0))
    Condition
      Error in `if (append) ...`:
      ! argument is of length zero

# taxonomic columns must not contain punctuation

    Code
      tax_unique(occdf = tetrapods, genus = "identified_name", family = "family",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `genus` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "identified_name",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `family` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, species = "identified_name", genus = "genus",
        family = "family", resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `species` column should not contain punctuation

