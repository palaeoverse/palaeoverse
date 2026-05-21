# tax_expand_lat works

    Code
      tax_expand_lat(taxdf = 5)
    Condition
      Error in `tax_expand_lat()`:
      ! `taxdf` should be a dataframe

---

    Code
      tax_expand_lat(taxdf)
    Condition
      Error in `tax_expand_lat()`:
      ! argument "bins" is missing, with no default

---

    Code
      tax_expand_lat(taxdf, bins = 1)
    Condition
      Error in `tax_expand_lat()`:
      ! `bins` should be a dataframe

---

    Code
      tax_expand_lat(taxdf, bins = bins, max_lat = "lat")
    Condition
      Error in `tax_expand_lat()`:
      ! Either `max_lat` or `min_lat` is not a named column in `taxdf`

---

    Code
      tax_expand_lat(taxdf, bins = bins, min_lat = "lat")
    Condition
      Error in `tax_expand_lat()`:
      ! Either `max_lat` or `min_lat` is not a named column in `taxdf`

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum and minimum latitudes must be less than or equal to 90

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum and minimum latitudes must be more than or equal to -90

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! The class of the max_lat column must be numeric.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! The class of the min_lat column must be numeric.

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum latitude must be larger than or equal to minimum latitude

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Not all rows in `taxdf` are unique

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Either 'bin', 'max' or 'min' is not a named column in `bins`

