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

# tax_range_time errors with unnamed args

    Code
      tax_range_time(occdf, "genus")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_time(occdf, "genus", "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_time(occdf, "genus", min_ma = "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

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
      ! Column "max_ma" in `occdf` must be of class <numeric>, not <character>.

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
      ! Column "min_ma" in `occdf` must be of class <numeric>, not <character>.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! Column "min_ma" in `occdf` must not have missing values.

# max ages must be larger than or equal to min ages

    Code
      tax_range_time(occdf)
    Condition
      Error in `tax_range_time()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) of `occdf` where "max_ma" is smaller than "min_ma": 2, 3.

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

# tax_range_time plotting with extra args works

    Code
      plot(tax_range_time(occdf), xlim = 1, ylim = 1, xaxt = 1, yaxt = 1, yaxs = 1)
    Condition
      Error in `plot()`:
      ! Cannot pass arguments `xlim`, `ylim`, `xaxt`, and `yaxt` when calling `plot()` on an object of class <palaeoverse_tax_range_time>.
      i These arguments are already set by `plot()` internally.

# plot is deprecated but still works

    Code
      tax_range_time(occdf, plot = "6")
    Condition
      Warning:
      The `plot` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `tax_range_time()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "6".

# plot_args is deprecated but still works

    Code
      tax_range_time(occdf, plot_args = 1)
    Condition
      Warning:
      The `plot_args` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `tax_range_time()`:
      ! `plot_args` must be of class <list> or `NULL`, not the number 1.

# argument 'intervals' works

    Code
      plot(tax_range_time(occdf), intervals = c("genus", "min_ma"))
    Condition
      Error in `plot()`:
      ! `intervals` must be of class <character>, <data.frame>, or a list of <character> or <data.frame>.

---

    Code
      plot(tax_range_time(occdf), intervals = 1)
    Condition
      Error in `plot()`:
      ! `intervals` must be of class <character>, <data.frame>, or a list of <character> or <data.frame>.

---

    Code
      plot(tax_range_time(occdf), intervals = NA)
    Condition
      Error in `plot()`:
      ! `intervals` must be of class <character>, <data.frame>, or a list of <character> or <data.frame>.

