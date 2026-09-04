# basic behavior works

    Code
      tax_range_strat(data.frame())
    Condition
      Error in `tax_range_strat()`:
      ! Column "genus" not found in `occdf`.

---

    Code
      tax_range_strat(NULL)
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` must be of class <data.frame>, not `NULL`.

---

    Code
      tax_range_strat(NA)
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_strat("a")
    Condition
      Error in `tax_range_strat()`:
      ! `occdf` must be of class <data.frame>, not the string "a".

# argument 'name' works

    Code
      tax_range_strat(occdf, name = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_range_strat(occdf, name = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `name` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(occdf, name = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `name` must be a single string, not `NA`.

---

    Code
      tax_range_strat(occdf, name = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      tax_range_strat(nadf)
    Condition
      Error in `tax_range_strat()`:
      ! Column "genus" in `occdf` must not have missing values.

# argument 'level' works

    Code
      tax_range_strat(occdf, level = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_range_strat(occdf, level = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(occdf, level = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be a single string, not `NA`.

---

    Code
      tax_range_strat(occdf, level = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be a single string, not the number 1.

---

    Code
      tax_range_strat(nadf)
    Condition
      Error in `tax_range_strat()`:
      ! Column "bed" in `occdf` must be of class <character>, not <logical>.

# argument 'group' works

    Code
      tax_range_strat(occdf, group = c("class", "genus"))
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not a character vector.

---

    Code
      tax_range_strat(occdf, group = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_range_strat(occdf, group = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(occdf, group = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not `NA`.

---

    Code
      tax_range_strat(occdf, group = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not the number 1.

# argument 'certainty' works

    Code
      tax_range_strat(occdf, certainty = c("class", "genus"))
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not a character vector.

---

    Code
      tax_range_strat(occdf, certainty = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `occdf`.

---

    Code
      tax_range_strat(occdf, certainty = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(occdf, certainty = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not `NA`.

---

    Code
      tax_range_strat(occdf, certainty = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not the number 1.

# argument 'by' works

    Code
      tax_range_strat(occdf, by = c("FAD", "LAD"))
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not a character vector.

---

    Code
      tax_range_strat(occdf, by = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be one of "FAD", "LAD", or "name", not "test".

---

    Code
      tax_range_strat(occdf, by = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(occdf, by = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not `NA`.

---

    Code
      tax_range_strat(occdf, by = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not the number 1.

