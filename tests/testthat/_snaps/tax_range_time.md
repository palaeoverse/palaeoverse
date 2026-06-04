# tax_range_time() works

    Code
      tax_range_time(occdf = NA)
    Condition
      Error in `tax_range_time()`:
      ! `occdf` should be a dataframe

---

    Code
      tax_range_time(occdf = occdf, max_ma = "test")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, min_ma = "test")
    Condition
      Error in `[.data.frame`:
      ! undefined columns selected

---

    Code
      tax_range_time(occdf = occdf, by = "test")
    Condition
      Error in `tax_range_time()`:
      ! `by` must be either "FAD", "LAD", or "name"

---

    Code
      tax_range_time(occdf = occdf, group = "test")
    Condition
      Error in `tax_range_time()`:
      ! `group` is not a named column in `occdf`

---

    Code
      tax_range_time(occdf = occdf, plot = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot` should be logical (TRUE/FALSE)

---

    Code
      tax_range_time(occdf = occdf, name = "test")
    Condition
      Error in `tax_range_time()`:
      ! Either `name`, `min_ma`, or `max_ma`, is not a named column in
               `occdf`

---

    Code
      tax_range_time(occdf = occdf, plot_args = "test")
    Condition
      Error in `tax_range_time()`:
      ! `plot_args` must be either NULL, or a list

---

    Code
      tax_range_time(occdf = occdf)
    Condition
      Error in `tax_range_time()`:
      ! The `name` column contains NA values

---

    Code
      tax_range_time(occdf = occdf)
    Condition
      Error in `tax_range_time()`:
      ! `max_ma` and `min_ma` must be of class numeric.

