# tax_range_strat() error handling

    Code
      tax_range_strat(occdf = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` should be a data.frame

---

    Code
      tax_range_strat(occdf = occdf, group = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf = occdf, name = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Either `name` or `level` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf = occdf, level = "test")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_strat(occdf = occdf, level = "genus")
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be of class numeric

---

    Code
      tax_range_strat(occdf = occdf, certainty = 0)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must either be of class character or NULL

---

    Code
      tax_range_strat(occdf = occdf, certainty = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf = occdf, by = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_strat(occdf = occdf)
    Condition
      Error in `tax_range_strat()`:
      ! The `name` column contains NA values

---

    Code
      tax_range_strat(occdf = occdf)
    Condition
      Error in `tax_range_strat()`:
      ! The `level` column contains NA values

---

    Code
      tax_range_strat(occdf = occdf, certainty = "certainty")
    Condition
      Error in `tax_range_strat()`:
      ! The `certainty` column contains NA values

