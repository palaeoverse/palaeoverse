# tax_unique() works

    Code
      tax_unique(species = "species", genus = "genus")
    Condition
      Error in `tax_unique()`:
      ! Must enter an `occdf` of occurrences or taxon names

---

    Code
      tax_unique(occdf = 100)
    Condition
      Error in `tax_unique()`:
      ! `occdf` must be a data.frame

---

    Code
      tax_unique(occdf = tetrapods, binomial = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `binomial`

---

    Code
      tax_unique(occdf = tetrapods, species = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `species`

---

    Code
      tax_unique(occdf = tetrapods, genus = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `genus`

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus")
    Condition
      Error in `tax_unique()`:
      ! At least one higher taxonomic level must be supplied (e.g. `family`)

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `family`

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        order = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `order`

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        order = "order", class = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `class`

---

    Code
      tax_unique(occdf = dinosaurs, genus = "genus", family = "family", order = "order",
        class = "class", name = "test")
    Condition
      Error in `tax_unique()`:
      ! `occdf` does not contain column name provided to `names`

---

    Code
      tax_unique(occdf = dinosaurs, genus = "genus", family = "family")
    Condition
      Error in `tax_unique()`:
      ! Species names must be supplied by specifying `binomial`, `genus` and
          `species`, or `genus` and `name` columns to estimate richness at species
          level

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", family = "family",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! Genus names must be supplied by specifying `binomial` or `genus`
          columns to estimate richness at genus level

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "family", class = "identified_name",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `class` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "family", order = "identified_name",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `order` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "identified_name",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `family` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, genus = "identified_name", family = "family",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `genus` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, species = "identified_name", genus = "genus",
        family = "family", resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `species` column should not contain punctuation

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "family", binomial = "identified_name",
        resolution = "genus")
    Condition
      Error in `tax_unique()`:
      ! `binomial` column should not contain punctuation except spaces or
               underscores

---

    Code
      tax_unique(occdf = tetrapods, genus = "genus", family = "family", name = "identified_name")
    Condition
      Error in `tax_unique()`:
      ! `name` column should not contain punctuation except spaces or
               underscores

---

    Code
      tax_unique(occdf = dinosaurs, species = "species", genus = "genus", family = "family",
        resolution = "test")
    Condition
      Error in `tax_unique()`:
      ! Resolution must be 'species' or 'genus'

