# basic behavior works

    Code
      tax_range_strat(data.frame())
    Condition
      Error in `tax_range_strat()`:
      ! Column "genus" not found in `data`.

---

    Code
      tax_range_strat(NULL)
    Condition
      Error in `tax_range_strat()`:
      ! `data` must be of class <data.frame>, not `NULL`.

---

    Code
      tax_range_strat(NA)
    Condition
      Error in `tax_range_strat()`:
      ! `data` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_strat("a")
    Condition
      Error in `tax_range_strat()`:
      ! `data` must be of class <data.frame>, not the string "a".

# tax_range_strat errors with unnamed args

    Code
      tax_range_strat(data, "genus")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_strat(data, "genus")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_strat(data, "genus", "bed")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_strat(data, "genus", level = "bed")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# argument 'name' works

    Code
      tax_range_strat(data, name = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `data`.

---

    Code
      tax_range_strat(data, name = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `name` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(data, name = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `name` must be a single string, not `NA`.

---

    Code
      tax_range_strat(data, name = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      tax_range_strat(nadf)
    Condition
      Error in `tax_range_strat()`:
      ! Column "genus" in `data` must not have missing values.

# argument 'level' works

    Code
      tax_range_strat(data, level = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `data`.

---

    Code
      tax_range_strat(data, level = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(data, level = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be a single string, not `NA`.

---

    Code
      tax_range_strat(data, level = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `level` must be a single string, not the number 1.

---

    Code
      tax_range_strat(nadf)
    Condition
      Error in `tax_range_strat()`:
      ! Column "bed" in `data` must be of class <numeric>, not <logical>.

# argument 'group' works

    Code
      tax_range_strat(data, group = c("class", "genus"))
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not a character vector.

---

    Code
      tax_range_strat(data, group = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `data`.

---

    Code
      tax_range_strat(data, group = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(data, group = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not `NA`.

---

    Code
      tax_range_strat(data, group = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `group` must be a single string, not the number 1.

# argument 'certainty' works

    Code
      tax_range_strat(data, certainty = c("class", "genus"))
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not a character vector.

---

    Code
      tax_range_strat(data, certainty = "test")
    Condition
      Error in `tax_range_strat()`:
      ! Column "test" not found in `data`.

---

    Code
      tax_range_strat(data, certainty = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(data, certainty = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not `NA`.

---

    Code
      tax_range_strat(data, certainty = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `certainty` must be a single string, not the number 1.

# argument 'by' works

    Code
      tax_range_strat(data, by = c("FAD", "LAD"))
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not a character vector.

---

    Code
      tax_range_strat(data, by = "test")
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be one of "FAD", "LAD", or "name", not "test".

---

    Code
      tax_range_strat(data, by = character(0))
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not an empty character vector.

---

    Code
      tax_range_strat(data, by = NA)
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not `NA`.

---

    Code
      tax_range_strat(data, by = 1)
    Condition
      Error in `tax_range_strat()`:
      ! `by` must be a single string, not the number 1.

