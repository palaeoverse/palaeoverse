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
      ! All arguments must be named (except for `data`).
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_time(data, "genus", "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for `data`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_time(data, "genus", min_ma = "min_ma")
    Condition
      Error in `tax_range_time()`:
      ! All arguments must be named (except for `data`).
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

# argument 'group' is deprecated but still works

    Code
      tax_range_time(data, group = c("genus", "min_ma"))
    Condition
      Warning:
      The `group` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `group_apply()` instead.
      Error in `tax_range_time()`:
      ! `group` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, group = "nonexistent")
    Condition
      Warning:
      The `group` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `group_apply()` instead.
      Error in `tax_range_time()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_time(data, group = 1)
    Condition
      Warning:
      The `group` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `group_apply()` instead.
      Error in `tax_range_time()`:
      ! `group` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, group = NA)
    Condition
      Warning:
      The `group` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `group_apply()` instead.
      Error in `tax_range_time()`:
      ! `group` must be a single string, not `NA`.

# argument 'sort' works

    Code
      tax_range_time(data, sort = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! `sort` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, sort = "nonexistent")
    Condition
      Error in `tax_range_time()`:
      ! `sort` must be one of "FAD", "LAD", or "name", not "nonexistent".

---

    Code
      tax_range_time(data, sort = 1)
    Condition
      Error in `tax_range_time()`:
      ! `sort` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, sort = NA)
    Condition
      Error in `tax_range_time()`:
      ! `sort` must be a single string, not `NA`.

# argument 'by' is deprecated but still works

    Code
      tax_range_time(data, by = "LAD", sort = "LAD")
    Condition
      Warning:
      The `by` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use the `sort` argument instead.
      Error in `tax_range_time()`:
      ! Exactly one of `by` or `sort` must be supplied.

---

    Code
      tax_range_time(data, by = c("genus", "min_ma"))
    Condition
      Warning:
      The `by` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use the `sort` argument instead.
      Error in `tax_range_time()`:
      ! `by` must be a single string, not a character vector.

---

    Code
      tax_range_time(data, by = "nonexistent")
    Condition
      Warning:
      The `by` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use the `sort` argument instead.
      Error in `tax_range_time()`:
      ! `by` must be one of "FAD", "LAD", or "name", not "nonexistent".

---

    Code
      tax_range_time(data, by = 1)
    Condition
      Warning:
      The `by` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use the `sort` argument instead.
      Error in `tax_range_time()`:
      ! `by` must be a single string, not the number 1.

---

    Code
      tax_range_time(data, by = NA)
    Condition
      Warning:
      The `by` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use the `sort` argument instead.
      Error in `tax_range_time()`:
      ! `by` must be a single string, not `NA`.

# tax_range_time plotting with extra args works

    Code
      plot(suppressWarnings(tax_range_time(data, group = "family")), col.group = c(
        "red", "blue"))
    Condition
      Error in `plot()`:
      ! Length of `col.group` (2) must be equal to the number of unique groups in the data (3).

---

    Code
      plot(tax_range_time(data), xlim = 1, ylim = 1, xaxt = 1, yaxt = 1, yaxs = 1)
    Condition
      Error in `plot()`:
      ! Cannot pass arguments `xlim`, `ylim`, `xaxt`, and `yaxt` when calling `plot()` on an object of class <palaeoverse_tax_range_time>.
      i These arguments are already set by `plot()` internally.

# plot is deprecated but still works

    Code
      tax_range_time(data, plot = "6")
    Condition
      Warning:
      The `plot` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `tax_range_time()`:
      ! `plot` must be `TRUE` or `FALSE`, not the string "6".

# plot_args is deprecated but still works

    Code
      tax_range_time(data, plot_args = 1)
    Condition
      Warning:
      The `plot_args` argument of `tax_range_time()` is deprecated as of palaeoverse 2.0.0.
      i Please use `plot()` on the output of this function instead.
      Error in `tax_range_time()`:
      ! `plot_args` must be of class <list> or `NULL`, not the number 1.

# argument 'intervals' is hard deprecated

    Code
      tax_range_time(data, intervals = c("genus", "min_ma"))
    Condition
      Error in `tax_range_time()`:
      ! The argument `intervals` has been removed in palaeoverse 2.0.0.
      i Use `plot(axis =)` on the output of this function instead.

# plot argument 'axis' works

    Code
      plot(tax_range_time(data), axis = "periods")
    Condition
      Error in `plot()`:
      ! `axis` must be a function call, not `"periods"`.

---

    Code
      plot(tax_range_time(data), axis = axis_geo(intervals = "nonexistent"))
    Condition
      Error in `plot()`:
      ! Error in `axis_geo(intervals = "nonexistent")`.
      Caused by error:
      ! `scale` must match a built-in or Macrostrat time scale.

---

    Code
      plot(tax_range_time(data), axis = axis_geo(intervals = 1))
    Condition
      Error in `plot()`:
      ! Error in `axis_geo(intervals = 1)`.
      Caused by error in `time_bins()`:
      ! `scale` must be one of the following, not the number 1:
      * the name of an in-built time scale (e.g. "GTS2020")
      * the name of a Macrostrat time scale
      * a <data.frame>
      > See `time_bins()` (`?palaeoverse::time_bins()`) for more details.

---

    Code
      plot(tax_range_time(data), axis = axis_geo(intervals = NA))
    Condition
      Error in `plot()`:
      ! Error in `axis_geo(intervals = NA)`.
      Caused by error in `time_bins()`:
      ! `scale` must be one of the following, not `NA`:
      * the name of an in-built time scale (e.g. "GTS2020")
      * the name of a Macrostrat time scale
      * a <data.frame>
      > See `time_bins()` (`?palaeoverse::time_bins()`) for more details.

