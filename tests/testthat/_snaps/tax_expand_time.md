# tax_expand_time() works

    Code
      tax_expand_time(taxdf, scale = "GTS2067")
    Condition
      Error:
      ! `name` does not match a built-in or Macrostrat time scale.

---

    Code
      tax_expand_time(taxdf, rank = "stages")
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be either: stage, epoch, period, era, or eon

---

    Code
      tax_expand_time(taxdf, rank = c("stage", "period"))
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be either: stage, epoch, period, era, or eon

---

    Code
      tax_expand_time(taxdf, bins = "stages")
    Condition
      Error in `tax_expand_time()`:
      ! `bins` should be a dataframe.

---

    Code
      tax_expand_time(taxdf, bins = bins)
    Condition
      Error in `tax_expand_time()`:
      ! bin, max_ma and/or min_ma do not exist in `bins`.

---

    Code
      tax_expand_time(taxdf, scale = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! Either `bin` or `scale` and `rank` must be specified.

---

    Code
      tax_expand_time(taxdf, scale = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! Either `bin` or `scale` and `rank` must be specified.

---

    Code
      tax_expand_time(taxdf, ext_orig = "ext")
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` should be logical (TRUE/FALSE)

---

    Code
      tax_expand_time(c("A", "B"))
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` should be a dataframe

---

    Code
      tax_expand_time(taxdf2)
    Condition
      Error in `tax_expand_time()`:
      ! The class of the max_ma column must be numeric.

---

    Code
      tax_expand_time(taxdf3)
    Condition
      Error in `tax_expand_time()`:
      ! The class of the min_ma column must be numeric.

---

    Code
      tax_expand_time(taxdf4)
    Condition
      Error in `tax_expand_time()`:
      ! Not all rows in `taxdf` are unique!

---

    Code
      tax_expand_time(taxdf5)
    Condition
      Error in `tax_expand_time()`:
      ! Maximum and minimum ages must be positive.

---

    Code
      tax_expand_time(taxdf6)
    Condition
      Error in `tax_expand_time()`:
      ! Maximum ages must be larger than or equal to minimum ages.

---

    Code
      tax_expand_time(taxdf7, max_ma = "test")
    Condition
      Error in `tax_expand_time()`:
      ! Either `min_ma` or `max_ma` is not a named column in `taxdf`

