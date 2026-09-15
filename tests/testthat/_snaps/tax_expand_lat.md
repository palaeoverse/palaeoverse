# basic behavior works

    Code
      tax_expand_lat(data = 5)
    Condition
      Error in `tax_expand_lat()`:
      ! `data` must be of class <data.frame>, not the number 5.

---

    Code
      tax_expand_lat(data)
    Condition
      Error in `tax_expand_lat()`:
      ! `bins` must be of class <data.frame>, not absent.

---

    Code
      tax_expand_lat(data, bins = 1)
    Condition
      Error in `tax_expand_lat()`:
      ! `bins` must be of class <data.frame>, not the number 1.

---

    Code
      tax_expand_lat(data, bins = bins, max_lat = "lat")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "lat" not found in `data`.

---

    Code
      tax_expand_lat(data, bins = bins, min_lat = "lat")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "lat" not found in `data`.

---

    Code
      tax_expand_lat(data = data.frame(name = c("A", "B", "C"), max_lat = c(92, 20,
        -10), min_lat = c(20, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "max_lat" in `data` must be between -90 and 90.
      i Value(s) outside the range: 92.

---

    Code
      tax_expand_lat(data = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c(-92, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "min_lat" in `data` must be between -90 and 90.
      i Value(s) outside the range: -92.

---

    Code
      tax_expand_lat(data = data.frame(name = "a", max_lat = 91:100, min_lat = 1),
      bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "max_lat" in `data` must be between -90 and 90.
      i Value(s) outside the range (first 5): 91, 92, 93, 94, 95.

---

    Code
      tax_expand_lat(data = data.frame(name = "a", max_lat = 1, min_lat = 91:100),
      bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All values of column "min_lat" in `data` must be between -90 and 90.
      i Value(s) outside the range (first 5): 91, 92, 93, 94, 95.

---

    Code
      tax_expand_lat(data = data.frame(name = c("A", "B", "C"), max_lat = c("60",
        "20", "-10"), min_lat = c(-90, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "max_lat" in `data` must be <numeric>, not <character>.

---

    Code
      tax_expand_lat(data = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c("20", -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "min_lat" in `data` must be <numeric>, not <character>.

---

    Code
      tax_expand_lat(data = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c(72, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum latitude must be larger than or equal to minimum latitude.
      i Row(s) where `max_lat` is smaller than `min_lat`: 1.

---

    Code
      tax_expand_lat(data = data.frame(name = "a", max_lat = c(90, 1:10), min_lat = c(
        72, 21:30)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum latitude must be larger than or equal to minimum latitude.
      i Row(s) where `max_lat` is smaller than `min_lat` (first 5): 2, 3, 4, 5, 6.

---

    Code
      tax_expand_lat(data = data.frame(name = c("A", "A", "C"), max_lat = c(60, 60,
        -10), min_lat = c(20, 20, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! `data` must not have duplicated rows.

---

    Code
      tax_expand_lat(data = data, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "bin" not found in `bins`.

# tax_expand_lat errors with unnamed args

    Code
      tax_expand_lat(data, bins)
    Condition
      Error in `tax_expand_lat()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_expand_lat(data, bins, "max_lat")
    Condition
      Error in `tax_expand_lat()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_expand_lat(data, bins, max_lat = "max_lat")
    Condition
      Error in `tax_expand_lat()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# args 'min_lat' and 'max_lat' work

    Code
      tax_expand_lat(data = data, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Column "max_lat" not found in `data`.

---

    Code
      tax_expand_lat(data = data, bins = bins, max_lat = "nonexistent")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_expand_lat(data = data, bins = bins, max_lat = NULL)
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not `NULL`.

---

    Code
      tax_expand_lat(data = data, bins = bins, max_lat = character(0))
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not an empty character vector.

---

    Code
      tax_expand_lat(data = data, bins = bins, max_lat = NA)
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not `NA`.

---

    Code
      tax_expand_lat(data = data, bins = bins, max_lat = c("a", "b"))
    Condition
      Error in `tax_expand_lat()`:
      ! `max_lat` must be a single string, not a character vector.

---

    Code
      tax_expand_lat(data = data, bins = bins, min_lat = "nonexistent")
    Condition
      Error in `tax_expand_lat()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_expand_lat(data = data, bins = bins, min_lat = NULL)
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not `NULL`.

---

    Code
      tax_expand_lat(data = data, bins = bins, min_lat = character(0))
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not an empty character vector.

---

    Code
      tax_expand_lat(data = data, bins = bins, min_lat = NA)
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not `NA`.

---

    Code
      tax_expand_lat(data = data, bins = bins, min_lat = c("a", "b"))
    Condition
      Error in `tax_expand_lat()`:
      ! `min_lat` must be a single string, not a character vector.

