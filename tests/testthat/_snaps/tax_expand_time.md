# basic behaviour works

    Code
      tax_expand_time(data.frame())
    Condition
      Error in `tax_expand_time()`:
      ! Column "max_ma" not found in `taxdf`.

---

    Code
      tax_expand_time(1)
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` must be of class <data.frame>, not the number 1.

---

    Code
      tax_expand_time(NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` must be of class <data.frame>, not `NULL`.

---

    Code
      tax_expand_time()
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` must be of class <data.frame>, not absent.

# rows must be unique

    Code
      tax_expand_time(taxdf)
    Condition
      Error in `tax_expand_time()`:
      ! `taxdf` must not have duplicated rows.

# ages must be positive

    Code
      tax_expand_time(taxdf)
    Condition
      Error in `tax_expand_time()`:
      ! All values of column "min_ma" in `taxdf` must be positive.
      i Value(s) outside the range: -20.

# max ages must be larger than or equal to min ages

    Code
      tax_expand_time(taxdf)
    Condition
      Error in `tax_expand_time()`:
      ! Maximum age must be larger than or equal to minimum age.
      i Row(s) where `max_ma` is smaller than `min_ma`: 3.

# arg 'bins' works

    Code
      tax_expand_time(taxdf, bins = data.frame())
    Condition
      Error in `tax_expand_time()`:
      ! Column "bin" not found in `bins`.

---

    Code
      tax_expand_time(taxdf, bins = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `bins` must be of class <data.frame>, not the number 1.

---

    Code
      tax_expand_time(taxdf, bins = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `bins` must be of class <data.frame>, not `NA`.

# args 'max_ma' and 'min_ma' work

    Code
      tax_expand_time(taxdf, bins = bins)
    Condition
      Error in `tax_expand_time()`:
      ! Column "max_ma" not found in `taxdf`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "nonexistent", min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! Column "nonexistent" not found in `taxdf`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = NULL, min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! `max_ma` must be a single string, not `NULL`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = character(0), min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! `max_ma` must be a single string, not an empty character vector.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = NA, min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! `max_ma` must be a single string, not `NA`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = c("a", "b"), min_ma = "lad")
    Condition
      Error in `tax_expand_time()`:
      ! `max_ma` must be a single string, not a character vector.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = "nonexistent")
    Condition
      Error in `tax_expand_time()`:
      ! Column "nonexistent" not found in `taxdf`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `min_ma` must be a single string, not `NULL`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = character(0))
    Condition
      Error in `tax_expand_time()`:
      ! `min_ma` must be a single string, not an empty character vector.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `min_ma` must be a single string, not `NA`.

---

    Code
      tax_expand_time(taxdf, bins = bins, max_ma = "fad", min_ma = c("a", "b"))
    Condition
      Error in `tax_expand_time()`:
      ! `min_ma` must be a single string, not a character vector.

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
      Error in `tax_expand_time()`:
      ! `scale` must be a single string, not an empty character vector.

---

    Code
      tax_expand_time(taxdf, scale = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `scale` must be a single string, not `NULL`.

---

    Code
      tax_expand_time(taxdf, scale = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `scale` must be a single string, not the number 1.

---

    Code
      tax_expand_time(taxdf, scale = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `scale` must be a single string, not `NA`.

# arg 'rank' works

    Code
      tax_expand_time(taxdf, rank = c("eon", "period"))
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be a single string, not a character vector.

---

    Code
      tax_expand_time(taxdf, rank = "foo")
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be one of "stage", "epoch", "period", "era", or "eon", not "foo".

---

    Code
      tax_expand_time(taxdf, rank = character(0))
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be a single string, not an empty character vector.

---

    Code
      tax_expand_time(taxdf, rank = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be a single string, not `NULL`.

---

    Code
      tax_expand_time(taxdf, rank = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be a single string, not the number 1.

---

    Code
      tax_expand_time(taxdf, rank = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `rank` must be a single string, not `NA`.

# arg 'ext_orig' works

    Code
      tax_expand_time(taxdf, ext_orig = "foo")
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` must be `TRUE` or `FALSE`, not the string "foo".

---

    Code
      tax_expand_time(taxdf, ext_orig = logical(0))
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      tax_expand_time(taxdf, ext_orig = NULL)
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` must be `TRUE` or `FALSE`, not `NULL`.

---

    Code
      tax_expand_time(taxdf, ext_orig = 1)
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      tax_expand_time(taxdf, ext_orig = NA)
    Condition
      Error in `tax_expand_time()`:
      ! `ext_orig` must be `TRUE` or `FALSE`, not `NA`.

