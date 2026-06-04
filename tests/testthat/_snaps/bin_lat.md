# bin_lat error handling

    Code
      bin_lat(occdf = 2, bins = bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `occdf` should be a dataframe.

---

    Code
      bin_lat(occdf = occdf, bins = 2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `bins` should be a dataframe.

---

    Code
      bin_lat(occdf = occdf, bins = bins, lat = "plat")
    Condition
      Error in `bin_lat()`:
      ! `lat` column name does not exist in `occdf`

---

    Code
      bin_lat(occdf = occdf, bins = bins2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `bins` does not contain bin, max and min named columns

---

    Code
      bin_lat(occdf = occdf, bins = bins2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `bins` does not contain bin, max and min named columns

---

    Code
      bin_lat(occdf = occdf, bins = bins2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `bins` does not contain bin, max and min named columns

---

    Code
      bin_lat(occdf = occdf, bins = bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `lat` contains NA values

---

    Code
      bin_lat(occdf = occdf, bins = bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! Latitudes should be more than -90 and less than 90

