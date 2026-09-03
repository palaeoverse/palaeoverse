# basic behaviour works

    Code
      tax_range_time(occdf = data.frame())
    Condition
      Error in `tax_range_time()`:
      ! Column "genus" not found in `occdf`.

---

    Code
      tax_range_time(occdf = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `occdf` must be of class <data.frame>, not `NULL`.

---

    Code
      tax_range_time(occdf = NA)
    Condition
      Error in `tax_range_time()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_time(occdf = "a")
    Condition
      Error in `tax_range_time()`:
      ! `occdf` must be of class <data.frame>, not the string "a".

# argument 'name' works

    Code
      tax_range_time(nadf, name = "species")
    Condition
      Error in `tax_range_time()`:
      ! Column "species" in `occdf` must not have missing values.

---

    Code
      tax_range_time(occdf, name = c("Species", "max_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not a character vector.

---

    Code
      tax_range_time(occdf, name = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_time(occdf, name = 1)
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      tax_range_time(occdf, name = NA)
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not `NA`.

---

    Code
      tax_range_time(occdf, name = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not `NULL`.

# argument 'max_ma' works

    Code
      tax_range_time(occdf, max_ma = c("Species", "max_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not a character vector.

---

    Code
      tax_range_time(occdf, max_ma = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_time(occdf, max_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not the number 1.

---

    Code
      tax_range_time(occdf, max_ma = NA)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not `NA`.

---

    Code
      tax_range_time(occdf, max_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not `NULL`.

---

    Code
      tax_range_time(chardf)
    Condition
      Error in `tax_range_time()`:
      ! Column "max_ma" in `occdf` must be numeric, not <character>.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! Column "max_ma" in `occdf` must not have missing values.

# argument 'min_ma' works

    Code
      tax_range_time(occdf, min_ma = c("Species", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not a character vector.

---

    Code
      tax_range_time(occdf, min_ma = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_time(occdf, min_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not the number 1.

---

    Code
      tax_range_time(occdf, min_ma = NA)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not `NA`.

---

    Code
      tax_range_time(occdf, min_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not `NULL`.

---

    Code
      tax_range_time(chardf)
    Condition
      Error in `tax_range_time()`:
      ! Column "min_ma" in `occdf` must be numeric, not <character>.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! Column "min_ma" in `occdf` must not have missing values.

# argument 'group' works

    Code
      tax_range_time(occdf, group = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `group` must be a single string, not a character vector.

---

    Code
      tax_range_time(occdf, group = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_time(occdf, group = 1)
    Condition
      Error in `tax_range_time()`:
      ! `group` must be a single string, not the number 1.

---

    Code
      tax_range_time(occdf, group = NA)
    Condition
      Error in `tax_range_time()`:
      ! `group` must be a single string, not `NA`.

# argument 'by' works

    Code
      tax_range_time(occdf, by = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `by` must be a single string, not a character vector.

---

    Code
      tax_range_time(occdf, by = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `by` must be one of "FAD", "LAD", or "name", not "nonexistent".

---

    Code
      tax_range_time(occdf, by = 1)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be a single string, not the number 1.

---

    Code
      tax_range_time(occdf, by = NA)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be a single string, not `NA`.

# argument 'plot' works

    Code
      tax_range_time(occdf, plot = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "test".

---

    Code
      tax_range_time(occdf, plot = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot` must be `TRUE` or `FALSE`, not `NA`.

# argument 'plot_args' works

    Code
      tax_range_time(occdf, plot_args = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be of class <list> or `NULL`, not the string "test".

---

    Code
      tax_range_time(occdf, plot_args = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be of class <list> or `NULL`, not `NA`.

