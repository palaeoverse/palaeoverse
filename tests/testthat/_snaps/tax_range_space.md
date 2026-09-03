# tax_range_space() works

    Code
      tax_range_space(occdf = data.frame())
    Condition
      Error in `tax_range_space()`:
      ! Column "genus" not found in `occdf`.

---

    Code
      tax_range_space(occdf = NA)
    Condition
      Error in `tax_range_space()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_space(occdf = "a")
    Condition
      Error in `tax_range_space()`:
      ! `occdf` must be of class <data.frame>, not the string "a".

# argument 'name' works

    Code
      tax_range_space(occdf = occdf, name = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_space(occdf = nadf, name = "genus")
    Condition
      Error in `tax_range_space()`:
      ! Column "genus" in `occdf` must not have missing values.

# argument 'lng' works

    Code
      tax_range_space(occdf, lng = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_space(chardf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lng" in `occdf` must be <numeric>, not <character>.

---

    Code
      tax_range_space(nadf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lng" in `occdf` must not have missing values.

# argument 'lat' works

    Code
      tax_range_space(occdf, lat = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_space(chardf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lat" in `occdf` must be <numeric>, not <character>.

---

    Code
      tax_range_space(nadf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lat" in `occdf` must not have missing values.

# argument 'method' works

    Code
      tax_range_space(occdf, method = c("gcd", "occ"))
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not a character vector.

---

    Code
      tax_range_space(occdf, method = "test")
    Condition
      Error in `tax_range_space()`:
      ! `method` must be one of "lat", "con", "gcd", or "occ", not "test".

---

    Code
      tax_range_space(occdf, method = character(0))
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not an empty character vector.

---

    Code
      tax_range_space(occdf, method = NA)
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not `NA`.

---

    Code
      tax_range_space(occdf, method = 1)
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not the number 1.

# argument 'spacing' works

    Code
      tax_range_space(occdf, method = "occ", spacing = "a")
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not the string "a".

---

    Code
      tax_range_space(occdf, method = "occ", spacing = numeric(0))
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not an empty numeric vector.

---

    Code
      tax_range_space(occdf, method = "occ", spacing = NA)
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not `NA`.

# argument 'coords' works

    Code
      tax_range_space(occdf, method = "gcd", coords = "a")
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be `TRUE` or `FALSE`, not the string "a".

---

    Code
      tax_range_space(occdf, method = "gcd", coords = logical(0))
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      tax_range_space(occdf, method = "gcd", coords = NA)
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be `TRUE` or `FALSE`, not `NA`.

