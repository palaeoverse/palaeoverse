# basic behavior works

    Code
      tax_expand_lat(taxdf = 5)
    Condition
      Error in `tax_expand_lat()`:
      ! `taxdf` must be of class <data.frame>, not the number 5.

---

    Code
      tax_expand_lat(taxdf)
    Condition
      Error in `tax_expand_lat()`:
      ! `bins` must be of class <data.frame>, not absent.

---

    Code
      tax_expand_lat(taxdf, bins = 1)
    Condition
      Error in `tax_expand_lat()`:
      ! `bins` must be of class <data.frame>, not the number 1.

---

    Code
      tax_expand_lat(taxdf, bins = bins, max_lat = "lat")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "lat" not found in `taxdf`.

---

    Code
      tax_expand_lat(taxdf, bins = bins, min_lat = "lat")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "lat" not found in `taxdf`.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(92, 20,
        -10), min_lat = c(20, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "max_lat" in `taxdf` must be between -90 and 90.
      i Value(s) outside the range: 92.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c(-92, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "min_lat" in `taxdf` must be between -90 and 90.
      i Value(s) outside the range: -92.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = "a", max_lat = 91:100, min_lat = 1),
      bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "max_lat" in `taxdf` must be between -90 and 90.
      i Value(s) outside the range (first 5): 91, 92, 93, 94, 95.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = "a", max_lat = 1, min_lat = 91:100),
      bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "min_lat" in `taxdf` must be between -90 and 90.
      i Value(s) outside the range (first 5): 91, 92, 93, 94, 95.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c("60",
        "20", "-10"), min_lat = c(-90, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "max_lat" in `taxdf` must be <numeric>, not <character>.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c("20", -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "min_lat" in `taxdf` must be <numeric>, not <character>.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c(72, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum latitude must be larger than or equal to minimum latitude.
      i Row(s) where `max_lat` is smaller than `min_lat`: 1.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = "a", max_lat = c(90, 1:10), min_lat = c(
        72, 21:30)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum latitude must be larger than or equal to minimum latitude.
      i Row(s) where `max_lat` is smaller than `min_lat` (first 5): 2, 3, 4, 5, 6.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "A", "C"), max_lat = c(60, 60,
        -10), min_lat = c(20, 20, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! `taxdf` must not have duplicated rows.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "bin" not found in `bins`.

# args 'min_lat' and 'max_lat' work

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "max_lat" not found in `taxdf`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, max_lat = "nonexistent")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "nonexistent" not found in `taxdf`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, max_lat = NULL)
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not `NULL`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, max_lat = character(0))
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not an empty character vector.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, max_lat = NA)
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not `NA`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, max_lat = c("a", "b"))
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not a character vector.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, min_lat = "nonexistent")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "nonexistent" not found in `taxdf`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, min_lat = NULL)
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not `NULL`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, min_lat = character(0))
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not an empty character vector.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, min_lat = NA)
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not `NA`.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins, min_lat = c("a", "b"))
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not a character vector.

