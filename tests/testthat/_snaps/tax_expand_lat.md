# basic behavior works

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
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(92, 20,
        -10), min_lat = c(20, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum and minimum latitudes must be less than or equal to 90

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c(-92, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum and minimum latitudes must be more than or equal to -90

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c("60",
        "20", "-10"), min_lat = c(-92, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! The class of the max_lat column must be numeric.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c("20", -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! The class of the min_lat column must be numeric.

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "B", "C"), max_lat = c(60, 20,
        -10), min_lat = c(72, -40, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Maximum latitude must be larger than or equal to minimum latitude

---

    Code
      tax_expand_lat(taxdf = data.frame(name = c("A", "A", "C"), max_lat = c(60, 60,
        -10), min_lat = c(20, 20, -60)), bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Not all rows in `taxdf` are unique

---

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Either 'bin', 'max' or 'min' is not a named column in `bins`

# args 'min_lat' and 'max_lat' work

    Code
      tax_expand_lat(taxdf = taxdf, bins = bins)
    Condition
      Error in `tax_expand_lat()`:
      ! Either `max_lat` or `min_lat` is not a named column in `taxdf`

