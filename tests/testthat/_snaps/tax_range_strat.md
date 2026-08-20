# basic behavior works

    Code
      tax_range_strat(data.frame())
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_strat(NULL)
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` should be a data.frame

---

    Code
      tax_range_strat(NA)
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` should be a data.frame

---

    Code
      tax_range_strat("a")
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` should be a data.frame

# argument 'name' works

    Code
      tax_range_strat(occdf, name = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Either `name` or `level` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf, name = character(0))
    Condition
      Error in `xtfrm.data.frame()`:
      ! cannot xtfrm data frames

---

    Code
      tax_range_strat(occdf, name = NA)
    Condition
      Error in `tax_range_strat()`:
      ! Either `name` or `level` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf, name = 1)
    Condition
      Error in `tax_range_strat()`:
      ! Either `name` or `level` is not a named column in `occdf`

---

    Code
      tax_range_strat(nadf)
    Condition
      Error in `tax_range_strat()`:
      ! The `name` column contains NA values

# argument 'level' works

    Code
      tax_range_strat(occdf, level = "test")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_strat(occdf, level = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be of class numeric

---

    Code
      tax_range_strat(occdf, level = NA)
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_strat(occdf, level = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be of class numeric

---

    Code
      tax_range_strat(nadf)
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be of class numeric

# argument 'group' works

    Code
      tax_range_strat(occdf, group = c("class", "genus"))
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be of length 1.

---

    Code
      tax_range_strat(occdf, group = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf, group = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be of length 1.

---

    Code
      tax_range_strat(occdf, group = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf, group = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `group` is not a named column in `occdf`

# argument 'certainty' works

    Code
      tax_range_strat(occdf, certainty = c("class", "genus"))
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be of length 1.

---

    Code
      tax_range_strat(occdf, certainty = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` is not a named column in `occdf`

---

    Code
      tax_range_strat(occdf, certainty = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be of length 1.

---

    Code
      tax_range_strat(occdf, certainty = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must either be of class character or NULL

---

    Code
      tax_range_strat(occdf, certainty = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must either be of class character or NULL

# argument 'by' works

    Code
      tax_range_strat(occdf, by = c("FAD", "LAD"))
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be of length 1.

---

    Code
      tax_range_strat(occdf, by = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_strat(occdf, by = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be of length 1.

---

    Code
      tax_range_strat(occdf, by = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_strat(occdf, by = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be either "FAD", "LAD", or "name"

