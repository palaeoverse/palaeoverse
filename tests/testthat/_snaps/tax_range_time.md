# basic behaviour works

    Code
      tax_range_time(occdf = data.frame())
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `occdf` should be a dataframe

---

    Code
      tax_range_time(occdf = NA)
    Condition
      Error in `tax_range_time()`:
      ! `occdf` should be a dataframe

---

    Code
      tax_range_time(occdf = "a")
    Condition
      Error in `tax_range_time()`:
      ! `occdf` should be a dataframe

# argument 'name' works

    Code
      tax_range_time(occdf, name = c("Species", "max_ma"))
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf, name = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf, name = 1)
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf, name = NA)
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf, name = NULL)
    Condition
      Error in `xtfrm.data.frame()`:
      ! cannot xtfrm data frames

---

    Code
      tax_range_time(nadf, name = "species")
    Condition
      Error in `tax_range_time()`:
      ! The `name` column contains NA values

# argument 'max_ma' works

    Code
      tax_range_time(occdf, max_ma = c("Species", "max_ma"))
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf, max_ma = "nonexistent")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf, max_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf, max_ma = NA)
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf, max_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(chardf)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` and/or `max_ma` columns contain NA values

# argument 'min_ma' works

    Code
      tax_range_time(occdf, min_ma = c("Species", "min_ma"))
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf, min_ma = "nonexistent")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf, min_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf, min_ma = NA)
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf, min_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(chardf)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(nadf)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` and/or `max_ma` columns contain NA values

# argument 'group' works

    Code
      tax_range_time(occdf, group = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `group` length is >1, only a single grouping variable is accepted.

---

    Code
      tax_range_time(occdf, group = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_time(occdf, group = 1)
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_time(occdf, group = NA)
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

# argument 'by' works

    Code
      tax_range_time(occdf, by = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `by` must be of length 1.

---

    Code
      tax_range_time(occdf, by = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_time(occdf, by = 1)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_time(occdf, by = NA)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

# argument 'plot' works

    Code
      tax_range_time(occdf, plot = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot` should be logical (TRUE/FALSE)

---

    Code
      tax_range_time(occdf, plot = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot` should be logical (TRUE/FALSE)

# argument 'plot_args' works

    Code
      tax_range_time(occdf, plot_args = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be either NULL, or a list

---

    Code
      tax_range_time(occdf, plot_args = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be either NULL, or a list

