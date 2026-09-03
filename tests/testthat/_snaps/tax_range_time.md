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

# tax_range_time errors with unnamed args

    Code
      tax_range_time(occdf, "genus")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_time(occdf = occdf, "genus")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_time(occdf, "genus", "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_time(occdf, "genus", min_ma = "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for "occdf").
      i Currently, there is 1 argument that should be named.

# argument 'name' works

    Code
      tax_range_time(occdf = nadf, name = "species")
    Condition
      Error in `tax_range_time()`:
      ! The `name` column contains NA values

---

    Code
      tax_range_time(occdf = occdf, name = c("Species", "max_ma"))
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf = occdf, name = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf = occdf, name = 1)
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf = occdf, name = NA)
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf = occdf, name = NULL)
    Condition
      Error in `xtfrm.data.frame()`:
      ! cannot xtfrm data frames

# argument 'max_ma' works

    Code
      tax_range_time(occdf = occdf, max_ma = c("Species", "max_ma"))
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, max_ma = "nonexistent")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, max_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf = occdf, max_ma = NA)
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, max_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf = chardf)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf = nadf)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` and/or `max_ma` columns contain NA values

# argument 'min_ma' works

    Code
      tax_range_time(occdf = occdf, min_ma = c("Species", "min_ma"))
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, min_ma = "nonexistent")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, min_ma = 1)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf = occdf, min_ma = NA)
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, min_ma = NULL)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf = chardf)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

---

    Code
      tax_range_time(occdf = nadf)
    Condition
      Error in `tax_range_time()`:
      ! `min_ma` and/or `max_ma` columns contain NA values

# argument 'group' works

    Code
      tax_range_time(occdf = occdf, group = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `group` length is >1, only a single grouping variable is accepted.

---

    Code
      tax_range_time(occdf = occdf, group = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_time(occdf = occdf, group = 1)
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_time(occdf = occdf, group = NA)
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

# argument 'by' works

    Code
      tax_range_time(occdf = occdf, by = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `by` must be of length 1.

---

    Code
      tax_range_time(occdf = occdf, by = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_time(occdf = occdf, by = 1)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_time(occdf = occdf, by = NA)
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

# argument 'plot' works

    Code
      tax_range_time(occdf = occdf, plot = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot` should be logical (TRUE/FALSE)

---

    Code
      tax_range_time(occdf = occdf, plot = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot` should be logical (TRUE/FALSE)

# argument 'plot_args' works

    Code
      tax_range_time(occdf = occdf, plot_args = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be either NULL, or a list

---

    Code
      tax_range_time(occdf = occdf, plot_args = NA)
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be either NULL, or a list

