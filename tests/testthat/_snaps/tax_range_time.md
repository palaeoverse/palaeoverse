# basic behaviour works

    Code
      tax_range_time(data = data.frame())
    Condition
      Error in `tax_range_time()`:
      ! Column "genus" not found in `data`.

---

    Code
      tax_range_time(data = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `data` must be of class <data.frame>, not `NULL`.

---

    Code
      tax_range_time(data = NA)
    Condition
      Error in `tax_range_time()`:
      ! `data` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_time(data = "a")
    Condition
      Error in `tax_range_time()`:
      ! `data` must be of class <data.frame>, not the string "a".

# tax_range_time errors with unnamed args

    Code
      tax_range_time(data, "genus")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_time(data, "genus", "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_time(data, "genus", min_ma = "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# argument 'name' works

    Code
      tax_range_time(nadf, name = "species")
    Condition
      Error in `tax_range_time()`:
      ! Column "species" in `data` must not have missing values.

---

    Code
      tax_range_time(data, name = c("Species", "max_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, name = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_time(data, name = 1)
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, name = NA)
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not `NA`.

---

    Code
      tax_range_time(data, name = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `name` must be a single string, not `NULL`.

# argument 'max_ma' works

    Code
      tax_range_time(data, max_ma = c("Species", "max_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, max_ma = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_time(data, max_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, max_ma = NA)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not `NA`.

---

    Code
      tax_range_time(data, max_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` must be a single string, not `NULL`.

---

    Code
      tax_range_time(chardf)
    Condition
      Error in `tax_range_time()`:
      ! Column "max_ma" in `data` must be of class <numeric>, not <character>.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! Column "max_ma" in `data` must not have missing values.

# argument 'min_ma' works

    Code
      tax_range_time(data, min_ma = c("Species", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, min_ma = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_time(data, min_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, min_ma = NA)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not `NA`.

---

    Code
      tax_range_time(data, min_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` must be a single string, not `NULL`.

---

    Code
      tax_range_time(chardf)
    Condition
      Error in `tax_range_time()`:
      ! Column "min_ma" in `data` must be of class <numeric>, not <character>.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! Column "min_ma" in `data` must not have missing values.

# max ages must be larger than or equal to min ages

    Code
      tax_range_time(data)
    Condition
      Error in `tax_range_time()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) of `data` where "max_ma" is smaller than "min_ma": 2, 3.

# argument 'group' works

    Code
      tax_range_time(data, group = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `group` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, group = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_time(data, group = 1)
    Condition
      Error in `tax_range_time()`:
      ! `group` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, group = NA)
    Condition
      Error in `tax_range_time()`:
      ! `group` must be a single string, not `NA`.

# argument 'by' works

    Code
      tax_range_time(data, by = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `by` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, by = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `by` must be one of "FAD", "LAD", or "name", not "nonexistent".

---

    Code
      tax_range_time(data, by = 1)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, by = NA)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be a single string, not `NA`.

# argument 'plot' works

    Code
      tax_range_time(data, plot = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "test".

---

    Code
      tax_range_time(data, plot = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot` must be `TRUE` or `FALSE`, not `NA`.

# argument 'plot_args' works

    Code
      tax_range_time(data, plot_args = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be of class <list> or `NULL`, not the string "test".

---

    Code
      tax_range_time(data, plot_args = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be of class <list> or `NULL`, not `NA`.

