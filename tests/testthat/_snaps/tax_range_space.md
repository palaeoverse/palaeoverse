# tax_range_space_lat() works

    Code
      tax_range_space_lat(occdf = data.frame())
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "genus" not found in `occdf`.

---

    Code
      tax_range_space_lat(occdf = NA)
    Condition
      Error in `tax_range_space_lat()`:
      ! `occdf` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_space_lat(occdf = "a")
    Condition
      Error in `tax_range_space_lat()`:
      ! `occdf` must be of class <data.frame>, not the string "a".

# tax_range_space_*() functions error with unnamed args

    Code
      tax_range_space_lat(occdf, "genus")
    Condition
      Error in `tax_range_space_lat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_space_lat(occdf, "genus", "lng")
    Condition
      Error in `tax_range_space_lat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_space_lat(occdf, "genus", lng = "lng")
    Condition
      Error in `tax_range_space_lat()`:
      ! All arguments must be named (except for `occdf`).
      i Currently, there is 1 argument that should be named.

# argument 'name' works

    Code
      tax_range_space_lat(occdf = occdf, name = "nonexistent")
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_space_lat(occdf = nadf, name = "genus")
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "genus" in `occdf` must not have missing values.

# argument 'lng' works

    Code
      tax_range_space_lat(occdf, lng = "nonexistent")
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_space_lat(chardf)
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "lng" in `occdf` must be of class <numeric>, not <character>.

---

    Code
      tax_range_space_lat(nadf)
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "lng" in `occdf` must not have missing values.

# argument 'lat' works

    Code
      tax_range_space_lat(occdf, lat = "nonexistent")
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "nonexistent" not found in `occdf`.

---

    Code
      tax_range_space_lat(chardf)
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "lat" in `occdf` must be of class <numeric>, not <character>.

---

    Code
      tax_range_space_lat(nadf)
    Condition
      Error in `tax_range_space_lat()`:
      ! Column "lat" in `occdf` must not have missing values.

# argument 'spacing' works

    Code
      tax_range_space_occ(occdf, spacing = "a")
    Condition
      Error in `tax_range_space_occ()`:
      ! `spacing` must be a number, not the string "a".

---

    Code
      tax_range_space_occ(occdf, spacing = numeric(0))
    Condition
      Error in `tax_range_space_occ()`:
      ! `spacing` must be a number, not an empty numeric vector.

---

    Code
      tax_range_space_occ(occdf, spacing = NA)
    Condition
      Error in `tax_range_space_occ()`:
      ! `spacing` must be a number, not `NA`.

---

    Code
      tax_range_space_occ(occdf, spacing = 1:2)
    Condition
      Error in `tax_range_space_occ()`:
      ! `spacing` must be a number, not an integer vector.

# argument 'coords' works

    Code
      tax_range_space_gcd(occdf, coords = "a")
    Condition
      Error in `tax_range_space_gcd()`:
      ! `coords` must be `TRUE` or `FALSE`, not the string "a".

---

    Code
      tax_range_space_gcd(occdf, coords = logical(0))
    Condition
      Error in `tax_range_space_gcd()`:
      ! `coords` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      tax_range_space_gcd(occdf, coords = NA)
    Condition
      Error in `tax_range_space_gcd()`:
      ! `coords` must be `TRUE` or `FALSE`, not `NA`.

# tax_range_space() is deprecated but still works

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

