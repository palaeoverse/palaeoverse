# basic behavior works

    Code
      tax_expand_time()
    Condition
      Error in `tax_expand_time()`:
      ! argument "taxdf" is missing, with no default

---

    Code
      tax_expand_time(data.frame())
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(1)
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` should be a dataframe

---

    Code
      tax_expand_time(NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` should be a dataframe

# rows must be unique

    Code
      tax_expand_time(taxdf)
    Condition
      Error in `tax_expand_time()`:
      ! Not all rows in `taxdf` are unique!

# ages must be positive

    Code
      tax_expand_time(taxdf)
    Condition
      Error in `tax_expand_time()`:
      ! Maximum and minimum ages must be positive.

# max ages must be larger than or equal to min ages

    Code
      tax_expand_time(taxdf)
    Condition
      Error in `tax_expand_time()`:
      ! Maximum ages must be larger than or equal to minimum ages.

# arg 'bins' works

    Code
      tax_expand_time(taxdf, bins = data.frame())
    Condition
      Error in `tax_expand_time()`:
      ! bin, max_ma and/or min_ma do not exist in `bins`.

---

    Code
      tax_expand_time(taxdf, bins = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `bins` should be a dataframe.

---

    Code
      tax_expand_time(taxdf, bins = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `bins` should be a dataframe.

# args 'max_ma' and 'min_ma' work

    Code
      tax_expand_time(taxdf, bins = bins)
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "nonexistent", min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = NULL, min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! The class of the max_ma column must be numeric.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = character(0), min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! The class of the max_ma column must be numeric.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = NA, min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = c("a", "b"), min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = "nonexistent")
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! The class of the min_ma column must be numeric.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = character(0))
    Condition
      Error in `tax_expand_time()`:
      ! The class of the min_ma column must be numeric.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = NA)
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = c("a", "b"))
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

# arg 'scale' works

    Code
      tax_expand_time(taxdf, scale = "foo")
    Condition
      Error:
      ! `name` does not match a built-in or Macrostrat time scale.

---

    Code
      tax_expand_time(taxdf, scale = character(0))
    Condition
      Error in `if (scale %in% c("GTS2020", "GTS2012")) ...`:
      ! argument is of length zero

---

    Code
      tax_expand_time(taxdf, scale = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! Either `bin` or `scale` and `rank` must be specified.

---

    Code
      tax_expand_time(taxdf, scale = 1)
    Condition
      Error in `time_bins()`:
      ! `scale` must be either:
       The name of an in-built time scale (e.g. 'GTS2020'), the name of a Macrostrat time scale (see details), or a `data.frame`.

---

    Code
      tax_expand_time(taxdf, scale = NA)
    Condition
      Error in `time_bins()`:
      ! `scale` must be either:
       The name of an in-built time scale (e.g. 'GTS2020'), the name of a Macrostrat time scale (see details), or a `data.frame`.

# arg 'rank' works

    Code
      tax_expand_time(taxdf, rank = c("eon", "period"))
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be either: stage, epoch, period, era, or eon

---

    Code
      tax_expand_time(taxdf, rank = "foo")
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be either: stage, epoch, period, era, or eon

---

    Code
      tax_expand_time(taxdf, rank = character(0))
    Condition
      Error in `if (...) NULL`:
      ! missing value where TRUE/FALSE needed

---

    Code
      tax_expand_time(taxdf, rank = NULL)
    Condition
      Error in `if (...) NULL`:
      ! missing value where TRUE/FALSE needed

---

    Code
      tax_expand_time(taxdf, rank = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be either: stage, epoch, period, era, or eon

---

    Code
      tax_expand_time(taxdf, rank = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be either: stage, epoch, period, era, or eon

# arg 'ext_orig' works

    Code
      tax_expand_time(taxdf, ext_orig = "foo")
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` should be logical (TRUE/FALSE)

---

    Code
      tax_expand_time(taxdf, ext_orig = logical(0))
    Condition
      Error in `if (ext_orig) ...`:
      ! argument is of length zero

---

    Code
      tax_expand_time(taxdf, ext_orig = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` should be logical (TRUE/FALSE)

---

    Code
      tax_expand_time(taxdf, ext_orig = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` should be logical (TRUE/FALSE)

---

    Code
      tax_expand_time(taxdf, ext_orig = NA)
    Condition
      Error in `if (ext_orig) ...`:
      ! missing value where TRUE/FALSE needed

