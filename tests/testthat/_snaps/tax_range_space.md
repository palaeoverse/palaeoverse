# tax_range_space() works

    Code
      tax_range_space(data = data.frame())
    Condition
      Error in `tax_range_space()`:
      ! Column "genus" not found in `data`.

---

    Code
      tax_range_space(data = NA)
    Condition
      Error in `tax_range_space()`:
      ! `data` must be of class <data.frame>, not `NA`.

---

    Code
      tax_range_space(data = "a")
    Condition
      Error in `tax_range_space()`:
      ! `data` must be of class <data.frame>, not the string "a".

# tax_range_space errors with unnamed args

    Code
      tax_range_space(data, "genus")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_space(data, "genus", "lng")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_space(data, "genus", lng = "lng")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named (except for "data").
      i Currently, there is 1 argument that should be named.

# argument 'name' works

    Code
      tax_range_space(data = data, name = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_space(data = nadf, name = "genus")
    Condition
      Error in `tax_range_space()`:
      ! Column "genus" in `data` must not have missing values.

# argument 'lng' works

    Code
      tax_range_space(data, lng = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_space(chardf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lng" in `data` must be <numeric>, not <character>.

---

    Code
      tax_range_space(nadf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lng" in `data` must not have missing values.

# argument 'lat' works

    Code
      tax_range_space(data, lat = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Column "nonexistent" not found in `data`.

---

    Code
      tax_range_space(chardf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lat" in `data` must be <numeric>, not <character>.

---

    Code
      tax_range_space(nadf)
    Condition
      Error in `tax_range_space()`:
      ! Column "lat" in `data` must not have missing values.

# argument 'method' works

    Code
      tax_range_space(data, method = c("gcd", "occ"))
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not a character vector.

---

    Code
      tax_range_space(data, method = "test")
    Condition
      Error in `tax_range_space()`:
      ! `method` must be one of "lat", "con", "gcd", or "occ", not "test".

---

    Code
      tax_range_space(data, method = character(0))
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not an empty character vector.

---

    Code
      tax_range_space(data, method = NA)
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not `NA`.

---

    Code
      tax_range_space(data, method = 1)
    Condition
      Error in `tax_range_space()`:
      ! `method` must be a single string, not the number 1.

# argument 'spacing' works

    Code
      tax_range_space(data, method = "occ", spacing = "a")
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not the string "a".

---

    Code
      tax_range_space(data, method = "occ", spacing = numeric(0))
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not an empty numeric vector.

---

    Code
      tax_range_space(data, method = "occ", spacing = NA)
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not `NA`.

---

    Code
      tax_range_space(data, method = "occ", spacing = 1:2)
    Condition
      Error in `tax_range_space()`:
      ! `spacing` must be a number, not an integer vector.

# argument 'coords' works

    Code
      tax_range_space(data, method = "gcd", coords = "a")
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be `TRUE` or `FALSE`, not the string "a".

---

    Code
      tax_range_space(data, method = "gcd", coords = logical(0))
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be `TRUE` or `FALSE`, not an empty logical vector.

---

    Code
      tax_range_space(data, method = "gcd", coords = NA)
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be `TRUE` or `FALSE`, not `NA`.

