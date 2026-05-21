# tax_range_space() works

    Code
      tax_range_space(occdf = NA)
    Condition
      Error in `tax_range_space()`:
      ! `occdf` should be a dataframe

---

    Code
      tax_range_space(occdf = occdf)
    Condition
      Error in `tax_range_space()`:
      ! The `name` column contains NA values

---

    Code
      tax_range_space(occdf = occdf, name = "accepted_name", lat = "test", method = "con")
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

---

    Code
      tax_range_space(occdf = occdf, name = "accepted_name", method = "test")
    Condition
      Error in `tax_range_space()`:
      ! Invalid `method`. Choose either:
        'con', 'lat', 'gcd', or 'occ'.

---

    Code
      tax_range_space(occdf = occdf, name = "accepted_name", method = 1)
    Condition
      Error in `tax_range_space()`:
      ! `method` is not of character class

---

    Code
      tax_range_space(occdf = occdf, name = "accepted_name", method = "con")
    Condition
      Error in `tax_range_space()`:
      ! The `name` column contains NA values

---

    Code
      tax_range_space(occdf = occdf)
    Condition
      Error in `tax_range_space()`:
      ! The `name` column contains NA values

---

    Code
      tax_range_space(occdf = occdf, name = "accepted_name")
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns are not of numeric class

---

    Code
      tax_range_space(occdf = occdf, name = "accepted_name")
    Condition
      Error in `tax_range_space()`:
      ! `lng` and/or `lat` columns are not of numeric class

---

    Code
      tax_range_space(occdf = occdf)
    Condition
      Error in `tax_range_space()`:
      ! Either `name`, `lng`, or `lat`, is not a named column
      in `occdf`

