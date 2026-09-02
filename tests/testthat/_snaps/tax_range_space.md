# tax_range_space() works

    Code
      tax_range_space(occdf = data.frame())
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(occdf = NA)
    Condition
      Error in `tax_range_space()`:
      ! `occdf` should be a dataframe

---

    Code
      tax_range_space(occdf = "a")
    Condition
      Error in `tax_range_space()`:
      ! `occdf` should be a dataframe

# tax_range_space errors with unnamed args

    Code
      tax_range_space(occdf, "genus")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

---

    Code
      tax_range_space(occdf = occdf, "genus")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

---

    Code
      tax_range_space(occdf, "genus", "lng")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named.
      i Currently, there are 3 arguments that should be named.

---

    Code
      tax_range_space(occdf, "genus", lng = "lng")
    Condition
      Error in `tax_range_space()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

# argument 'name' works

    Code
      tax_range_space(occdf = occdf, name = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(occdf = nadf, name = "genus")
    Condition
      Error in `tax_range_space()`:
      ! The `name` column contains NA values

# argument 'lng' works

    Code
      tax_range_space(occdf = occdf, lng = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(occdf = chardf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns are not of numeric class

---

    Code
      tax_range_space(occdf = nadf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns contain NA values

# argument 'lat' works

    Code
      tax_range_space(occdf = occdf, lat = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(occdf = chardf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns are not of numeric class

---

    Code
      tax_range_space(occdf = nadf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns contain NA values

# argument 'method' works

    Code
      tax_range_space(occdf = occdf, method = c("gcd", "occ"))
    Condition
      Error in `tax_range_space()`:
      ! `method` must be of length 1.

---

    Code
      tax_range_space(occdf = occdf, method = "test")
    Condition
      Error in `tax_range_space()`:
      ! Invalid `method`. Choose either:
        'con', 'lat', 'gcd', or 'occ'.

---

    Code
      tax_range_space(occdf = occdf, method = character(0))
    Condition
      Error in `tax_range_space()`:
      ! `method` must be of length 1.

---

    Code
      tax_range_space(occdf = occdf, method = NA)
    Condition
      Error in `tax_range_space()`:
      ! `method` is not of character class

---

    Code
      tax_range_space(occdf = occdf, method = 1)
    Condition
      Error in `tax_range_space()`:
      ! `method` is not of character class

# argument 'spacing' works

    Code
      tax_range_space(occdf = occdf, method = "occ", spacing = "a")
    Condition
      Error in `h3jsr::h3_info_table$avg_cendist_km - spacing`:
      ! non-numeric argument to binary operator

---

    Code
      tax_range_space(occdf = occdf, method = "occ", spacing = numeric(0))
    Condition
      Error in `$<-.data.frame`:
      ! replacement has 0 rows, data has 3

---

    Code
      tax_range_space(occdf = occdf, method = "occ", spacing = NA)
    Condition
      Error in `$<-.data.frame`:
      ! replacement has 0 rows, data has 3

# argument 'coords' works

    Code
      tax_range_space(occdf = occdf, method = "gcd", coords = "a")
    Condition
      Error in `!coords`:
      ! invalid argument type

---

    Code
      tax_range_space(occdf = occdf, method = "gcd", coords = logical(0))
    Condition
      Error in `tax_range_space()`:
      ! `coords` must be of length 1.

---

    Code
      tax_range_space(occdf = occdf, method = "gcd", coords = NA)
    Condition
      Error in `tax_range_space()`:
      ! `coords` must not be NA.

