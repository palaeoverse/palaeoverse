# bin_lat errors with unnamed args

    Code
      bin_lat(tetrapods, bins)
    Condition
      Error in `bin_lat()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

---

    Code
      bin_lat(occdf = tetrapods, bins)
    Condition
      Error in `bin_lat()`:
      ! All arguments must be named.
      i Currently, there is 1 argument that should be named.

---

    Code
      bin_lat(tetrapods, bins, "lat")
    Condition
      Error in `bin_lat()`:
      ! All arguments must be named.
      i Currently, there are 3 arguments that should be named.

---

    Code
      bin_lat(tetrapods, bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! All arguments must be named.
      i Currently, there are 2 arguments that should be named.

# bin_lat error handling

    Code
      bin_lat(occdf = 2, bins = bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `occdf` must be of class <data.frame>, not the number 2.

---

    Code
      bin_lat(occdf = occdf, bins = 2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! `bins` must be of class <data.frame>, not the number 2.

---

    Code
      bin_lat(occdf = occdf, bins = bins, lat = "plat")
    Condition
      Error in `bin_lat()`:
      ! Column "plat" not found in `occdf`.

---

    Code
      bin_lat(occdf = occdf, bins = bins2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! Column "bin" not found in `bins`.

---

    Code
      bin_lat(occdf = occdf, bins = bins2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! Column "min" not found in `bins`.

---

    Code
      bin_lat(occdf = occdf, bins = bins2, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! Column "max" not found in `bins`.

---

    Code
      bin_lat(occdf = occdf, bins = bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! Column "lat" in `occdf` must not have missing values.

---

    Code
      bin_lat(occdf = occdf, bins = bins, lat = "lat")
    Condition
      Error in `bin_lat()`:
      ! All values of column "lat" in `occdf` must be between -90 and 90.
      i Value(s) outside the range: 91.

