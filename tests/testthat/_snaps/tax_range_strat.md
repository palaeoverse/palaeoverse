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

# tax_range_strat errors with unnamed args

    Code
      tax_range_strat(occdf, "genus")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_strat(occdf, "genus")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_strat(occdf, "genus", "bed")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_strat(occdf, "genus", level = "bed")
    Condition
      Error in `tax_range_strat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

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
      ! Column "bed" in `occdf` must be of class <numeric>, not <logical>.

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

# tax_range_strat plotting with extra args works

    Code
      plot(tax_range_strat(occdf, group = "class"), col.group = c("red", "blue"))
    Condition
      Error in `plot()`:
      ! Length of `col.group` (2) must be equal to the number of unique groups in the data (3).

---

    Code
      plot(tax_range_strat(occdf), y = 1, axes = TRUE, type = "l", xaxs = "r")
    Condition
      Error in `plot()`:
      ! Cannot pass arguments `y`, `axes`, `type`, and `xaxs` when calling `plot()` on an object of class <palaeoverse_tax_range_strat>.
      i These arguments are already set by `plot()` internally.

# plot argument 'axis' works

    Code
      plot(tax_range_strat(occdf), axis = "periods")
    Condition
      Error in `plot()`:
      ! `axis` must be a function call, not `"periods"`.

---

    Code
      plot(tax_range_strat(occdf), axis = axis_geo(intervals = 1))
    Condition
      Error in `plot()`:
      ! Error in `axis_geo(intervals = 1)`.
      Caused by error in `time_bins()`:
      ! `scale` must be one of the following, not the number 1:
      * the name of an in-built time scale (e.g. "GTS2020")
      * the name of a Macrostrat time scale
      * a <data.frame>
      > See `time_bins()` (`?palaeoverse::time_bins()`) for more details.

# plot_args is deprecated but still works

    Code
      tax_range_strat(occdf, plot_args = 1)
    Condition
      Warning:
      The `plot_args` argument of `tax_range_strat()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `tax_range_strat()`:
      ! `plot_args` must be of class <list> or `NULL`, not the number 1.

# arguments 'x_args' and 'y_args' are hard deprecated

    Code
      tax_range_strat(occdf, x_args = list(las = 1))
    Condition
      Error in `tax_range_strat()`:
      ! The arguments `x_args` and `y_args` have been removed in palaeoverse 2.0.0.
      i Use `plot()` on the output of this function instead.

---

    Code
      tax_range_strat(occdf, y_args = list(las = 1))
    Condition
      Error in `tax_range_strat()`:
      ! The arguments `x_args` and `y_args` have been removed in palaeoverse 2.0.0.
      i Use `plot()` on the output of this function instead.

