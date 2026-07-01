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
      tax_range_space(occdf, lng = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(chardf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns are not of numeric class

---

    Code
      tax_range_space(nadf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns contain NA values

# argument 'lat' works

    Code
      tax_range_space(occdf, lat = "nonexistent")
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(chardf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns are not of numeric class

---

    Code
      tax_range_space(nadf)
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns contain NA values

# argument 'method' works

    Code
      tax_range_space(occdf, method = c("gcd", "occ"))
    Condition
      Error in `if (is.na(method_match)) ...`:
      ! the condition has length > 1

---

    Code
      tax_range_space(occdf, method = "test")
    Condition
      Error in `tax_range_space()`:
      ! Invalid `method`. Choose either:
        'con', 'lat', 'gcd', or 'occ'.

---

    Code
      tax_range_space(occdf, method = character(0))
    Condition
      Error in `if (is.na(method_match)) ...`:
      ! argument is of length zero

---

    Code
      tax_range_space(occdf, method = NA)
    Condition
      Error in `tax_range_space()`:
      ! `method` is not of character class

---

    Code
      tax_range_space(occdf, method = 1)
    Condition
      Error in `tax_range_space()`:
      ! `method` is not of character class

# argument 'spacing' works

    Code
      tax_range_space(occdf, method = "occ", spacing = "a")
    Condition
      Error in `h3jsr::h3_info_table$avg_cendist_km - spacing`:
      ! non-numeric argument to binary operator

---

    Code
      tax_range_space(occdf, method = "occ", spacing = numeric(0))
    Condition
      Error in `$<-.data.frame`:
      ! replacement has 0 rows, data has 3

---

    Code
      tax_range_space(occdf, method = "occ", spacing = NA)
    Condition
      Error in `$<-.data.frame`:
      ! replacement has 0 rows, data has 3

# argument 'coords' works

    Code
      tax_range_space(occdf, method = "gcd", coords = "a")
    Condition
      Error in `!coords`:
      ! invalid argument type

---

    Code
      tax_range_space(occdf, method = "gcd", coords = logical(0))
    Condition
      Error in `if (!coords) ...`:
      ! argument is of length zero

---

    Code
      tax_range_space(occdf, method = "gcd", coords = NA)
    Condition
      Error in `if (!coords) ...`:
      ! missing value where TRUE/FALSE needed

